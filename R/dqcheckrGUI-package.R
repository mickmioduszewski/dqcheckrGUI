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
#' @section Status:
#' \pkg{dqcheckrGUI} is feature-complete and is maintained for corrections
#' only.  Configuration features are developed in \pkg{dqcheckr} itself.
#' Existing deployments continue to work unchanged.
#'
#' Since \pkg{dqcheckr} 0.3.0 the workflow this interface provides is available
#' as plain function calls, with no 'shiny' app to run:
#' \code{\link[dqcheckr]{generate_dataset_config}()} inspects a delivery and
#' writes a fully-commented YAML config,
#' \code{\link[dqcheckr]{validate_config}()} checks it,
#' \code{\link[dqcheckr]{run_dq_check}()} runs the checks, and
#' \code{\link[dqcheckr]{list_runs}()} lists past runs.  See
#' \code{vignette("workflow", package = "dqcheckr")}.
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
#' @importFrom stringi stri_enc_isutf8
#' @importFrom tools file_path_sans_ext
#' @importFrom utils URLencode
#' @importFrom yaml read_yaml
#'
#' @keywords internal
"_PACKAGE"
