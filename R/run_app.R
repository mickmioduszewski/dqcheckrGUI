#' Launch the dqcheckrGUI Shiny application
#'
#' Opens the point-and-click interface for configuring and running
#' \pkg{dqcheckr} dataset quality checks.
#'
#' @param ... Arguments passed to \code{\link[shiny]{runApp}} (e.g.
#'   \code{port}, \code{launch.browser}).
#'
#' @return Called for its side effect; does not return a value.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(...) {
  app_dir <- system.file("app", package = "dqcheckrGUI")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing dqcheckrGUI.")
  }
  shiny::runApp(app_dir, ...)
}
