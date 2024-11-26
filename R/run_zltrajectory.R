#' Run the zltrajectory Shiny App
#'
#' @description This function runs the Shiny web application for this package,
#'   providing an interactive interface for a common workflow provided by
#'   zltrajectory. This function does not return, but a webpage will be opened.
#'
#'
#' @examples
#' \dontrun{
#' run_zltrajectory()
#' }
#'
#' @export
#' @importFrom shiny runApp
run_zltrajectory <- function() {
  # Run the Shiny application from its directory
  app_dir <- system.file("shiny-scripts", package = "zltrajectory")
  shiny::runApp(appDir = app_dir, display.mode = "normal")

  # Nothing to return
  return(invisible(NULL))
}
