@echo off
cd /d "%~dp0"
Rscript -e "shiny::runApp('inst/app', port=4321, launch.browser=TRUE)"
pause
