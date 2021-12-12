packages <- c(
  "here", "readxl", # for the project's organization
  "tidyverse", "lubridate", # for wrangling
  "knitr", "kableExtra", "bookdown", "rmarkdown", "DT", # for the report
  "summarytools","caret","ggplot2",
  "dplyr")

purrr::walk(packages, library, character.only = TRUE)

#poids <- readxl::read_xlsx("~/NESTLE/PodSPC - Documents/Data/VenusLab - QMS poids.xlsx")
poids <- readxl::read_xlsx("C:/Users/sophi//NESTLE/PodSPC - Documents/Data/VenusLab - QMS poids.xlsx")
poids <- poids[!is.na(poids$Prelevement),]
poids <- poids[!is.na(poids$Cible),]
poids$Poids1 <- as.double(poids$Poids1)
poids$Poids2 <- as.double(poids$Poids2)
poids <- poids[,-16]
poids <- poids %>%
  separate(Batch_pod_bottom, c("Batch Pod size", "Spec"), "-")

poids <- poids %>%
  pivot_longer(
    cols = starts_with("Poids"),
    names_to = "Poids",
    names_prefix = "Poids",
    values_to = "weight",
    values_drop_na = TRUE
  )

poids_SPC <- poids %>% 
  group_by(Request, Prelevement, Cible) %>%
  summarise(real_weight = weight - Tare,
            median = median(real_weight),
            sd = sd(real_weight),
            range = max(real_weight) - min(real_weight))

poids_SPC <- poids_SPC[,-4] %>%
  distinct()

#This function draws a density graph according to a particular request
#It shows the median of the process as well as their control limits.
#It shows also the the process Cible as well as their specication limits (deviation around the Cible).


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
  request_SPC <- poids_SPC %>%
    filter(Request == request)
  Rbar = mean(request_SPC$range)
  UCL = median(request_SPC$median) + A2*Rbar
  LCL = median(request_SPC$median) - A2*Rbar
  
  
  z <- request_SPC$median > UCL | request_SPC$median < LCL
  out_control_perc <- sum(z)/length(request_SPC)
  
  
  USL = request_SPC$Cible[1] + request_SPC$Cible[1]*0.015 #Ask to Joao : What value should be set for the gap around the target
  LSL = request_SPC$Cible[1] - request_SPC$Cible[1]*0.015 #Ask to Joao : What value should be set for the gap around the target
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
    ggplot2::geom_vline(ggplot2::aes(xintercept = request_SPC$Cible), color = "blue") +
    ggplot2::geom_text(aes(x = request_SPC$Cible, label = "Cible", y = 23), colour="blue") +
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
  
if (Cpk < Cp) {
  caption2 <- "Process is off center"
}


print(graph + labs(caption = paste(
        round(out_control_perc, 3),"% of observations are considered as outliers and are represented by the red stripes." , "\n",
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
#It shows also the the process Cible.

R_bar_chart <- function(request, A2 = 0.483) {
  
  request_SPC <- poids_SPC %>%
    filter(Request == request)

  Rbar = mean(request_SPC$range)
  UCL = median(request_SPC$median) + A2*Rbar
  LCL = median(request_SPC$median) - A2*Rbar
  
  z <- request_SPC$median > UCL | request_SPC$median < LCL
  out_control_point <- sum(z)
  
  Rchart <- request_SPC %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(aes(x = Prelevement, y = median, colour = request_SPC$median > UCL | request_SPC$median < LCL)) +
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
    ggplot2::geom_hline(yintercept = (request_SPC$Cible),
                        color = "blue") +
    ggplot2::geom_text(aes(x = -5, label = "Cible", y = (request_SPC$Cible) + 0.005), colour = "blue") +
    ggplot2::xlab("Prélèvements") +
    ggplot2::ylab("Median of each sample in grams") + theme(legend.position = "none") + 
    ggplot2::scale_color_manual(values = c("black", "red")) + labs(
                    title = paste("Request",request_SPC$Request, "R chart"),
                    subtitle = paste("The", out_control_point, "red dots are outside the control limits. Process variation cannot explain these extreme values, Process must be analysed" ))
  
print(Rchart)
  
}
R_bar_chart(929)









summary_stat <- function(request, A2 = 0.483) {
  
  request_SPC <- poids_SPC %>%
    filter(Request == request)
  
  Rbar = mean(request_SPC$range)
  UCL = median(request_SPC$median) + A2*Rbar
  LCL = median(request_SPC$median) - A2*Rbar
  Process_median <- median(request_SPC$median)
  
  z <- request_SPC$median > UCL | request_SPC$median < LCL
  beyond_limit <- sum(z)
  out_control_perc <- sum(z)/length(request_SPC)
  
  
  summary <- as.matrix(c(Process_median, request_SPC$Cible[1], Rbar, UCL, LCL, beyond_limit, out_control_perc))
  
  summary <-
    cbind(c(
      "Process Median",
      "Process Cible",
      "Process Mean Deviation",
      "Upper Control Limit",
      "Lower Control Limit",
      "Number beyonds Limits", 
      "Out of control %"
    ), summary)
}

summary_stat(929)

