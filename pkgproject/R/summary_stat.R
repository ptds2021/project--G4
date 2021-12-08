#' @title Summary table
#' @author Özgür Aydemir, Sophie La Gennusa, Louis del Perugia, Daniel Szenes, Francesca Darino
#' @export
#'
summary_stat <- function(request, A2 = 0.483) {

  request_SPC <- poids_SPC %>%
    filter(Request == 929)

  Rbar = mean(request_SPC$range)
  UCL = median(request_SPC$median) + A2*Rbar
  LCL = median(request_SPC$median) - A2*Rbar
  Process_median <- median(request_SPC$median)

  z <- request_SPC$median > UCL | request_SPC$median < LCL
  beyond_limit <- sum(z)
  out_control_perc <- sum(z)/length(request_SPC)


  summary <- dplyr::as_tibble(c(Process_median, request_SPC$Cible[1], Rbar, UCL, LCL, beyond_limit, out_control_perc))

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
  knitr::kable(summary, "simple")
}
