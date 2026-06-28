#' dqcheckrGUI: Point-and-Click GUI Client for 'dqcheckr'
#'
#' A point-and-click 'shiny' front-end for \pkg{dqcheckr} -- configure and run
#' automated data quality checks on recurring dataset deliveries without
#' writing any R code.
#'
#' @section Getting started:
#' Launch the app with \code{\link{run_app}}.  On first run it will offer to
#' create the standard project layout (\file{config/}, \file{data/},
#' \file{reports/}) in the chosen directory.
#'
#' The config directory can be set via the \env{DQCHECKR_CONFIG_DIR}
#' environment variable or the \code{config_dir} argument to
#' \code{\link{run_app}}.  See the getting-started vignette for a full
#' walkthrough including Windows and OneDrive setups:
#' \code{vignette("dqcheckrGUI", package = "dqcheckrGUI")}.
#'
#' @section Related packages:
#' \pkg{dqcheckrGUI} delegates all data processing to \pkg{dqcheckr}.
#' See \code{vignette("dqcheckr", package = "dqcheckr")} for the core
#' package introduction and \code{vignette("specification", package =
#' "dqcheckr")} for the full config/schema reference.
#'
#' These packages are only called from the 'shiny' app sourced at runtime via
#' \code{system.file("app", package = "dqcheckrGUI")}
#' (\code{inst/app/R/*.R}), so static analysis of \code{R/} cannot see them
#' as used -- without a reference here, \code{R CMD check} reports
#' "Namespaces in Imports field not imported from".
#' @importFrom DBI dbConnect
#' @importFrom DT datatable
#' @importFrom RSQLite SQLite
#' @importFrom bslib bs_theme
#' @importFrom callr r_bg
#' @importFrom dqcheckr run_dq_check
#' @importFrom reactable reactable
#' @importFrom readr cols
#' @importFrom shinyAce aceEditor
#' @importFrom shinyFiles shinyDirButton
#' @importFrom shinyvalidate InputValidator
#' @importFrom stats median
#' @importFrom tools file_path_sans_ext
#' @importFrom yaml read_yaml
#'
#' @keywords internal
"_PACKAGE"
