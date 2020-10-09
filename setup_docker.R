# If we wish to install the packages without the help of the renv package.
packages <- c('shiny', 'shinydashboard', 'shinyWidgets',
              'DT', 'tidyverse', 'tidyselect', 'lubridate',
              'sf', 'sp', 'rgdal', 'maptools', 'spatstat',
              'plotly', 'leaflet', 'mapview', 'rvest', 'mapsapi')
install.packages(packages)