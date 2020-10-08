# Title: Crime Map
# Author: Adriel Martins
# Date: 08/05/2020

# EXECUTING THE APP
## Global
source('app/global.R')
## UI
source('app/ui.R')
## Server
source('app/server.R')

# Configurations of the server
options(shiny.port = 3137, shiny.server = '0.0.0.0')

shiny::shinyApp(ui, server)


