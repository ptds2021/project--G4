#' @title Create Cpk graph for a given request
#' @description TBD
#' @param request \code{numeric} used to identify the rquest
#' @param A2 \code{numeric} defines the boundaries of the control limits, default value 0.483
#' @param d2 \code{numeric}
#' @author Özgür Aydemir, Sophie La Gennusa, Louis del Perugia, Daniel Szenes, Francesca Darino
#' @export
#'

request_CL <- function(data,request, A2 = 0.483, d2 = 5.534) {

  request <- data %>%
    filter(Request == request)

  data_long <- request %>%
    pivot_longer(
      cols = starts_with("Measure"),
      names_to = "Inputs",
      names_prefix = "Inputs",
      values_to = "weight",
      values_drop_na = TRUE
    )

  df <- data_long %>%
    group_by(Request, Process.Sample, Target.Value) %>%
    mutate(real_weight = weight - Tare)%>%
    summarise(
      median = median(real_weight),
      sd = sd(real_weight),
      range = max(real_weight) - min(real_weight))

  Rbar = mean(df$range)
  UCL = median(df$median) + A2*Rbar
  LCL = median(df$median) - A2*Rbar


  z <- df$median > UCL | df$median < LCL
  out_control_perc <- sum(z)/length(df)


  USL = df$Target.Value[1] + df$Target.Value[1]*0.015
  LSL = df$Target.Value[1] - df$Target.Value[1]*0.015
  Cp = (USL - LSL)/(6*Rbar/d2)
  Cpu <- (USL - mean(df$median))/(3*Rbar/d2)
  Cpl <- (mean(df$median) - LSL)/(3*Rbar/d2)
  Cpk <- min(Cpu, Cpl)


  graph <- df %>%
    ggplot2::ggplot() +
    ggplot2::geom_histogram(aes(x = df$median, fill = df$median > UCL | df$median < LCL), bins = 100) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(df$median)), color = "black", linetype = 3) +
    ggplot2::geom_text(aes(x = mean(df$median), label = "Process Median", y = -0.5), colour="black") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = UCL), color = "black", linetype = 3) +
    ggplot2::geom_text(aes(x = UCL, label = "UCL", y = -0.5), colour = "black") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = LCL), color = "black", linetype = 3) +
    ggplot2::geom_text(aes(x = LCL, label = "LCL", y = -0.5), colour = "black") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = df$Target.Value), color = "blue") +
    ggplot2::geom_text(aes(x = df$Target.Value, label = "Target.Value", y = 23), colour="blue") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = USL), color = "blue") +
    ggplot2::geom_text(aes(x = USL, label = "USL", y = 23), colour = "blue") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = LSL), color = "blue") +
    ggplot2::geom_text(aes(x = LSL, label = "LSL", y = 23), colour = "blue") +
    stat_function(fun = dnorm, n = 101,
                  args = list(mean = mean(df$median),
                              sd = median(df$sd))) +
    ylab("") +
    xlab("") + theme(legend.position = "none") +
    scale_fill_manual(values = c("black", "red")) + labs(
      title = paste("Request",df$Request, "density chart and Cpk analysis"))

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
