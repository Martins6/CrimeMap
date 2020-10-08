# Title: Crime Map
# Author: Adriel Martins
# Date: 08/05/2020

# Installing Package dependencies
if (!requireNamespace("renv")){
  install.packages('renv')
  renv::restore()
}else{
  renv::restore()
}
# EXECUTING THE APP
## Global
source('app/global.R')
## UI
source('app/ui.R')
## Server0
source('app/server.R')

shiny::shinyApp(ui, server)


