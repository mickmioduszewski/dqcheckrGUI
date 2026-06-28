#' Launch the dqcheckrGUI Shiny application
#'
#' Opens the point-and-click interface for configuring and running
#' \pkg{dqcheckr} dataset quality checks.
#'
#' @param config_dir Path to the directory that contains (or will contain)
#'   \file{dqcheckr.yml} and individual dataset \file{.yml} files.  Defaults
#'   to the \env{DQCHECKR_CONFIG_DIR} environment variable when set, otherwise
#'   \file{config/} inside the current working directory.  Passing this
#'   argument explicitly is recommended when launching from a script so that
#'   the correct project folder is used regardless of the R session's working
#'   directory.
#' @param ... Arguments passed to \code{\link[shiny]{runApp}} (e.g.
#'   \code{port}, \code{launch.browser}).
#'
#' @return Called for its side effect; does not return a value.
#'
#' @seealso
#' \code{vignette("dqcheckrGUI", package = "dqcheckrGUI")} for a full
#' setup walkthrough.  \code{\link[dqcheckr]{run_dq_check}} for the
#' underlying check function.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#'
#'   # Explicit project folder (recommended from a launcher script)
#'   # run_app(config_dir = file.path(getwd(), "config"))
#' }
run_app <- function(config_dir = NULL, ...) {
  app_dir <- system.file("app", package = "dqcheckrGUI")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing dqcheckrGUI.")
  }
  if (!is.null(config_dir)) {
    old <- Sys.getenv("DQCHECKR_CONFIG_DIR", unset = NA_character_)
    Sys.setenv(DQCHECKR_CONFIG_DIR = config_dir)
    on.exit({
      if (is.na(old)) Sys.unsetenv("DQCHECKR_CONFIG_DIR")
      else            Sys.setenv(DQCHECKR_CONFIG_DIR = old)
    }, add = TRUE)
  }
  shiny::runApp(app_dir, ...)
}
