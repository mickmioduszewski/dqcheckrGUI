#!/bin/bash
cd "$(dirname "$0")"
Rscript -e "shiny::runApp('inst/app', port=4321, launch.browser=TRUE)"
