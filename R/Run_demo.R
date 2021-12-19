#' @title Run Shiny app for pkgproject
#'
#' @description Runs an interactive Shiny app that allows the user
#'  to organise inputs and analysis for his process data
#' @author Özgür Aydemir, Sophie La Gennusa, Louis del Perugia, Daniel Szenes, Francesca Darino
#' @return A \code{Shiny app} containing the following elements:
#'  \describe{
#'      \item{Input tab}{Tab where one can input new values and see historical data}
#'      \item{Density plot}{Tab that displays analytical plots and summary statistics per request}
#'      \item{Time seires tab}{Tab where one can see the values for measurements per request per sample}
#'      \item{App info tab}{Tab with general information about the app}
#' }
#' @export
Run_demo <- function() {
  appDir <- system.file( "test", package = "pkgproject")
  if (appDir == "") {
    stop(
      "Could not find example directory. Try re-installing pkgproject",
      call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal")
}
