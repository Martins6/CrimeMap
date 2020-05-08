########################### / HEADER / #################################
header <- dashboardHeader(title = "Crimes em SP")

########################## / SIDEBAR / #################################
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Mapa", tabName = "map"),
    menuItem("Estatísticas", tabName = "stats"),
    menuItem("Modelagem de Risco", tabName = "model"),
    menuItem("Sobre", tabName = "about")
  )
)

################################### / BODY / ##########################
body <- dashboardBody(
  tabItems(
    #####################  Map Section #####################
    tabItem(tabName = "map",
            
            fluidRow(
              box('', width = 10,
                  leafletOutput('map.plot')
                  ),
              # Map options
              box('Opções de Visualizações', width = 2,
                  selectInput('year.ch', 'Qual ano gostaria de escolher?',
                              choices = c(2015, 2016), multiple = F),
                  selectInput('crime.type', 'Que tipo de crime gostaria de visualizar no mapa?',
                              choices = c('Todos os tipos', crime_types),
                              selected = 'Todos os tipos',
                              multiple = F),
                  actionButton('go_map.plot', 'Submeter')
                  )
            )
     # End of the Map Section       
    ),
    
    #####################  Stats Section #####################
    tabItem(tabName = "stats"
            
      # End of the Stats Section       
    ),
    
    #####################  Modelling Section #####################
    tabItem(tabName = "model"
            
      # End of the Stats Section       
    ),
    
    #####################  About Section #####################
    tabItem(tabName = "about"
            
              
            
      # End of the Stats Section       
    )
    
    # End of the tabitems
  )
  # End of the body
)

############################## Putting it all together in the UI #################################
ui <- dashboardPage(
  skin = 'red',
  header, sidebar, body)
