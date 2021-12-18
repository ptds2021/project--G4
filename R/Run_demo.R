#' @title Run Shiny app for estimate_area function
#'
#' @description Runs an interactive Shiny app that allows the user
#'  use the functions from the pkgproject package
#' @author Özgür Aydemir, Sophie La Gennusa, Louis del Perugia, Daniel Szenes, Francesca Darino
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
