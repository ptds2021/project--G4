#' @title Summary table
#' @author Özgür Aydemir, Sophie La Gennusa, Louis del Perugia, Daniel Szenes, Francesca Darino
#' @param data The data set of the analysis (data.frame)
#' @param request Request in the data set (a value in the data.frame)
#' @param A2 constant fixing the normal law
#' @import tidyverse
#' @import shiny
#' @import dplyr
#' @import knitr
#' @export
#'



summary_stat <- function(data,request,A2 = 0.483) {

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
  Process_median <- median(df$median)

  z <- df$median > UCL | df$median < LCL
  beyond_limit <- sum(z)
  out_control_perc <- sum(z)/length(df)


  summary <- as_tibble(c(Process_median, df$Target.Value[1], Rbar, UCL, LCL, beyond_limit, out_control_perc))

  summary$name <-
    c(
      "Process Median",
      "Process Cible",
      "Process Mean Deviation",
      "Upper Control Limit",
      "Lower Control Limit",
      "Number beyonds Limits",
      "Out of control %"
    )
  kable(summary, "simple")
}
