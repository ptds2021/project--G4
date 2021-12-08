#' @title R bar chart
#' @author Özgür Aydemir, Sophie La Gennusa, Louis del Perugia, Daniel Szenes, Francesca Darino
#' @export
#'



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
