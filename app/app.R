# Title: Crime Map
# Author: Adriel Martins
# Date: 08/05/2020

# EXECUTING THE APP
## Global
source('global.R')
## UI
source('ui.R')
## Server
source('server.R')

#Configurations of the server
shiny::shinyApp(ui, server)


