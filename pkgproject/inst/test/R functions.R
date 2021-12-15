packages <- c(
  "here", "readxl", # for the project's organization
  "tidyverse", "lubridate", # for wrangling
  "knitr", "kableExtra", "bookdown", "rmarkdown", "DT", # for the report
  "summarytools","caret","ggplot2",
  "dplyr", "berryFunctions")

purrr::walk(packages, library, character.only = TRUE)
nasty <- pkgproject::nasty
nasty <- nasty[!is.na(nasty$Process.Sample),]
nasty <- nasty[!is.na(nasty$Target.Value),]

nasty <- nasty[,-1]

nasty <- nasty %>%
  pivot_longer(
    cols = starts_with("Measure"),
    names_to = "Measure",
    names_prefix = "Measure",
    values_to = "measure",
    values_drop_na = TRUE
  )



nasty <- nasty[!is.na(nasty$Tare),]

a <- unique(nasty %>% filter(Process.Sample >= 40))

nasty_SPC <- nasty %>% filter(Request %in% a$Request) %>%
  group_by(Request, Process.Sample, Target.Value) %>%
  summarise(real_measure = measure - Tare,
            median = median(real_measure),
            sd = sd(real_measure),
            range = max(real_measure) - min(real_measure))

nasty_SPC <- nasty_SPC[,-4] %>%
  distinct()

#This function draws a density graph according to a particular request
#It shows the median of the process as well as their control limits.
#It shows also the the process Target.Value as well as their specication limits (deviation around the Target.Value).


#text to be rephrased (I just copied and pasted)

#(Control limits are used to detect signals in process data that indicate that a process is not in control and,
#therefore, not operating predictably.)
#Control limits are calculated from process data.
#They represent how your process actually performs.
#Specification limits are defined by your customer and represent the desired performance of your process.
#Specification limits and control limits are used for different purposes.
#Control limits let you assess whether your process is stable.
#Specification limits allow you to assess how capable your process is of meeting customer requirements.

request_CL <- function(request, A2 = 0.483, d2 = 5.534) {
  request_SPC <- nasty_SPC %>%
    dplyr::filter(Request == request)
  Rbar = mean(request_SPC$range)
  UCL = median(request_SPC$median) + A2*Rbar
  LCL = median(request_SPC$median) - A2*Rbar


  z <- request_SPC$median > UCL | request_SPC$median < LCL
  out_control_perc <- sum(z)/length(request_SPC)


  USL = request_SPC$Target.Value[1] + request_SPC$Target.Value[1]*0.015 #Ask to Joao : What value should be set for the gap around the target
  LSL = request_SPC$Target.Value[1] - request_SPC$Target.Value[1]*0.015 #Ask to Joao : What value should be set for the gap around the target
  Cp = (USL - LSL)/(6*Rbar/d2)
  Cpu <- (USL - mean(request_SPC$median))/(3*Rbar/d2)
  Cpl <- (mean(request_SPC$median) - LSL)/(3*Rbar/d2)
  Cpk <- min(Cpu, Cpl)


  graph <- request_SPC %>%
    ggplot2::ggplot() +
    ggplot2::geom_histogram(aes(x = request_SPC$median, fill = request_SPC$median > UCL | request_SPC$median < LCL), bins = 100) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(request_SPC$median)), color = "black", linetype = 3) +
    ggplot2::geom_text(aes(x = mean(request_SPC$median), label = "Process Median", y = -0.5), colour="black") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = UCL), color = "black", linetype = 3) +
    ggplot2::geom_text(aes(x = UCL, label = "UCL", y = -0.5), colour = "black") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = LCL), color = "black", linetype = 3) +
    ggplot2::geom_text(aes(x = LCL, label = "LCL", y = -0.5), colour = "black") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = request_SPC$Target.Value), color = "blue") +
    ggplot2::geom_text(aes(x = request_SPC$Target.Value, label = "Target.Value", y = 23), colour="blue") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = USL), color = "blue") +
    ggplot2::geom_text(aes(x = USL, label = "USL", y = 23), colour = "blue") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = LSL), color = "blue") +
    ggplot2::geom_text(aes(x = LSL, label = "LSL", y = 23), colour = "blue") +
    stat_function(fun = dnorm, n = 101,
                  args = list(mean = mean(request_SPC$median),
                                                    sd = median(request_SPC$sd))) +
    ylab("") +
    xlab("") + theme(legend.position = "none") +
    scale_fill_manual(values = c("black", "red")) + labs(
      title = paste("Request",request_SPC$Request, "density chart and Cpk analysis"))

if (Cpk < 1) {
  caption1 <- "Process variation is not equal to the specs"
}

if (Cpk >= 1 && Cpk < 1.3) {
  caption1 <- "Process variation is about equal to the specs"
}

if (Cpk >= 1.3) {
  caption1 <- "Process variation is equal to the specs"
  }

  if (berryFunctions::almost.equal(Cp, Cpk)) {
    caption2 <- "Process is centered"
  } else{caption2 <- "Process is off-center"}


print(graph + labs(caption = paste(
        "Cp = ", Cp,"\t",
        "Cpk = ", Cpk,"\t", "\n",
        caption1,"\n", caption2, "\n")) +
    theme(plot.caption = element_text(hjust = 0, face = "italic"),
          plot.title.position = "plot",
          plot.caption.position =  "plot"))

}

request_CL(929)


#This function shows the evolution of prélèvements over time according to a particular request. In SixSigma theory, this graph is called an R-chart
#It shows the median of the process as well as their control limits.
#It shows also the the process Target.Value.

R_bar_chart <- function(request, A2 = 0.483) {

  request_SPC <- nasty_SPC %>%
    filter(Request == request)

  Rbar = mean(request_SPC$range)
  UCL = median(request_SPC$median) + A2*Rbar
  LCL = median(request_SPC$median) - A2*Rbar

  z <- request_SPC$median > UCL | request_SPC$median < LCL
  out_control_point <- sum(z)

  Rchart <- request_SPC %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(aes(x = Process.Sample, y = median, colour = request_SPC$median > UCL | request_SPC$median < LCL)) +
    ggplot2::geom_hline(yintercept = mean(request_SPC$median),
                        linetype = "dashed",
                        color = "black") +
    ggplot2::geom_text(aes(x = -5, label = "Process \n Median", y = mean(request_SPC$median) + 0.007), colour = "black") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = UCL),
                        color = "black",
                        linetype = 3) +
    ggplot2::geom_text(aes(x = -5, label = "UCL", y = UCL + 0.005), colour = "black") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = LCL),
                        color = "black",
                        linetype = 3) +
    ggplot2::geom_text(aes(x = -5, label = "LCL", y = LCL + 0.005), colour = "black") +
    ggplot2::geom_hline(yintercept = (request_SPC$Target.Value),
                        color = "blue") +
    ggplot2::geom_text(aes(x = -5, label = "Target.Value", y = (request_SPC$Target.Value) + 0.005), colour = "blue") +
    ggplot2::xlab("Prélèvements") +
    ggplot2::ylab("Median of each sample in grams") + theme(legend.position = "none") +
    ggplot2::scale_color_manual(values = c("black", "red")) + labs(
                    title = paste("Request",request_SPC$Request, "R chart"),
                    subtitle = paste("The", out_control_point, "red dots are outside the control limits. Process variation cannot explain these extreme values, Process must be analysed" ))

print(Rchart)

}
R_bar_chart(929)









summary_stat <- function(request, A2 = 0.483) {

  request_SPC <- nasty_SPC %>%
    filter(Request == request)

  Rbar = mean(request_SPC$range)
  UCL = median(request_SPC$median) + A2*Rbar
  LCL = median(request_SPC$median) - A2*Rbar
  Process_median <- median(request_SPC$median)

  z <- request_SPC$median > UCL | request_SPC$median < LCL
  beyond_limit <- sum(z)
  out_control_perc <- sum(z)/length(request_SPC)


  summary <- as.matrix(c(Process_median, request_SPC$Target.Value[1], Rbar, UCL, LCL, beyond_limit, out_control_perc))

  summary <-
    cbind(c(
      "Process Median",
      "Process Target.Value",
      "Process Mean Deviation",
      "Upper Control Limit",
      "Lower Control Limit",
      "Number beyonds Limits",
      "Out of control %"
    ), summary)
}

summary_stat(929)


b <- unique(nasty %>% group_by(Target.Value) %>% count() %>%
              filter(n > 60))

Target.Value_p_SPC_ <- nasty %>%
  filter(Target.Value %in% b$Target.Value) %>%
  group_by(Product.Size, Request, Target.Value) %>%
  summarise(
    real_measure = measure - Tare,
    median = median(real_measure),
    sd = sd(real_measure),
    range = max(real_measure) - min(real_measure)
  )

## size and Target.Value filter

cible_CL <- function(size, Target.Value, A2 = 0.483, d2 = 5.534) {
  Target.Value_p_SPC_ <-  Target.Value_p_SPC_ %>%
    filter(Product.Size == size & Target.Value == Target.Value)

  Rbar_Target.Value = mean(Target.Value_p_SPC_$range)
  UCL_Target.Value = median(Target.Value_p_SPC_$median) + A2 * Rbar_Target.Value
  LCL_Target.Value = median(Target.Value_p_SPC_$median) - A2 * Rbar_Target.Value


  USL = Target.Value_p_SPC_$Target.Value[1] + Target.Value_p_SPC_$Target.Value[1]*0.01 #Ask to Joao : What value should be set for the gap around the target
  LSL = Target.Value_p_SPC_$Target.Value[1] - Target.Value_p_SPC_$Target.Value[1]*0.01 #Ask to Joao : What value should be set for the gap around the target
  Cp = (USL - LSL)/(6*Rbar_Target.Value/d2)
  Cpu <- (USL - mean(Target.Value_p_SPC_$median))/(3*Rbar_Target.Value/d2)
  Cpl <- (mean(Target.Value_p_SPC_$median) - LSL)/(3*Rbar_Target.Value/d2)
  Cpk <- min(Cpu, Cpl)


  graph <- Target.Value_p_SPC_ %>%
    ggplot2::ggplot() +
    ggplot2::geom_histogram(aes(x = Target.Value_p_SPC_$median, fill = Target.Value_p_SPC_$median > UCL_Target.Value | Target.Value_p_SPC_$median < LCL_Target.Value), bins = 100) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(Target.Value_p_SPC_$median)), color = "black", linetype = 3) +
    ggplot2::geom_text(aes(x = mean(Target.Value_p_SPC_$median), label = "Process Median", y = -0.5), colour="black") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = UCL_Target.Value), color = "black", linetype = 3) +
    ggplot2::geom_text(aes(x = UCL_Target.Value, label = "UCL", y = -0.5), colour = "black") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = LCL_Target.Value), color = "black", linetype = 3) +
    ggplot2::geom_text(aes(x = LCL_Target.Value, label = "LCL", y = -0.5), colour = "black") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = Target.Value_p_SPC_$Target.Value), color = "blue") +
    ggplot2::geom_text(aes(x = Target.Value_p_SPC_$Target.Value, label = "Target.Value", y = 23), colour="blue") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = USL), color = "blue") +
    ggplot2::geom_text(aes(x = USL, label = "USL", y = 23), colour = "blue") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = LSL), color = "blue") +
    ggplot2::geom_text(aes(x = LSL, label = "LSL", y = 23), colour = "blue") +
    stat_function(fun = dnorm, n = 101,
                  args = list(mean = mean(Target.Value_p_SPC_$median),
                              sd = median(Target.Value_p_SPC_$sd))) +
    ylab("") +
    xlab("") + theme(legend.position = "none") +
    scale_fill_manual(values = c("black", "red")) + labs(
      title = paste(Target.Value_p_SPC_$Product.Size, "Pod size with a target of", Target.Value_p_SPC_$Target.Value, "Gr" , "density chart and Cpk analysis"))
  if (Cpk < 1) {
    caption1 <- "Process variation is not equal to the specs"
  }

  if (Cpk >= 1 && Cpk < 1.3) {
    caption1 <- "Process variation is about equal to the specs"
  }

  if (Cpk >= 1.3) {
    caption1 <- "Process variation is equal to the specs"
  }

  if (berryFunctions::almost.equal(Cp, Cpk)) {
    caption2 <- "Process is centered"
  } else{caption2 <- "Process is off-center"}

  print(graph + labs(caption = paste(
    "Cp = ", Cp,"\t",
    "Cpk = ", Cpk,"\t", "\n",
    caption1,"\n", caption2, "\n")) +
      theme(plot.caption = element_text(hjust = 0, face = "italic"),
            plot.title.position = "plot",
            plot.caption.position =  "plot"))
}

cible_CL("L", 8.7)
