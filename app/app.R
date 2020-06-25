# Title: Crime Map
# Author: Adriel Martins
# Date: 08/05/2020

# EXECUTING THE APP
## Global
source('app_files/global.R')
## UI
source('app_files/ui.R')
## Server0
source('app_files/server.R')

shiny::shinyApp(ui, server)


