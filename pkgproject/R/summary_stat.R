#' @title Summary table
#' @description Displays a table with summary statistics for the measurements of a process.
#' The process should generally be measuring the weight of a substance in aontainer.
#' @author Özgür Aydemir, Sophie La Gennusa, Louis del Perugia, Daniel Szenes, Francesca Darino
#' @param data \code{data.frame} The dataset for the analysis (data.frame).
#' #' For this function to work, your dataset must have :
#' a `Request` column that has a number that identifies the request,
#' columns starting with `Measure` that has the values for you measurements,
#' a `Process Sample` column that records the number of the sample per request,
#' a `Target value` column that gives us the the value on which we base the specifications and
#' a `Tare` column for the weight of the container.
#' @param request \code{numeric} Number used to identify the request.
#' @param A2 \code{numeric} constant defining the specification limits, default value 0.483.
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
