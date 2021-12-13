#' @title R bar chart
#' @author Özgür Aydemir, Sophie La Gennusa, Louis del Perugia, Daniel Szenes, Francesca Darino
#' @description This function will produce a R bar chart. It will display the mean of the request specify and all the all other prelevement found in the data base.
#' We use the A2 constants to calculate control limits for an Average.
#' @return A plot displaying the R bar chart
#' @param \code{data.frame} data The data set of the analysis
#' @param \code{numeric} request Request in the data set (a value in the data.frame)
#' @param A2 \code{numeric} constant determined by a normal law based on the size of the prelevement, default value 0.483
#' @import tidyverse
#' @import shiny
#' @export



R_bar_chart <- function(data, request, A2 = 0.483) {
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
  out_control_point <- sum(z)

  Rchart <- df %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(aes(x = Process.Sample, y = median, colour = df$median > UCL | df$median < LCL)) +
    ggplot2::geom_hline(yintercept = mean(df$median),
                        linetype = "dashed",
                        color = "black") +
    ggplot2::geom_text(aes(x = -5, label = "Process \n Median", y = mean(df$median) + 0.007), colour = "black") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = UCL),
                        color = "black",
                        linetype = 3) +
    ggplot2::geom_text(aes(x = -5, label = "UCL", y = UCL + 0.005), colour = "black") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = LCL),
                        color = "black",
                        linetype = 3) +
    ggplot2::geom_text(aes(x = -5, label = "LCL", y = LCL + 0.005), colour = "black") +
    ggplot2::geom_hline(yintercept = (df$Target.Value),
                        color = "blue") +
    ggplot2::geom_text(aes(x = -5, label = "Target.Value", y = (df$Target.Value) + 0.005), colour = "blue") +
    ggplot2::xlab("Prélèvements") +
    ggplot2::ylab("Median of each sample in grams") + theme(legend.position = "none") +
    ggplot2::scale_color_manual(values = c("black", "red")) + labs(
      title = paste("Request",df$Request, "R chart"),
      subtitle = paste("The", out_control_point, "red dots are outside the control limits. Process variation cannot explain these extreme values, Process must be analysed" ))

  print(Rchart)

}
