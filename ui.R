########################### / HEADER / #################################
header <- dashboardHeader(title = "Crimes em São Paulo")

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
              box(title = '', width = 10, solidHeader = T, footer = 'Fonte: SSP',
                  leafletOutput('map.plot')
                  ),
              # Map options
              box('Opções de Visualizações', width = 2, solidHeader = T,
                  selectInput('year.ch', 'Qual ano gostaria de escolher?',
                              choices = c(2015, 2016), multiple = F),
                  selectInput('crime.type', 'Que tipo de crime gostaria de visualizar no mapa?',
                              choices = c('Todos os tipos', crime_types),
                              selected = 'Todos os tipos',
                              multiple = F),
                  actionButton('go_map.plot', 'Submeter')
                  )
            ),
            
            fluidRow(
              
              box('', width = 12,
                  plotlyOutput('ts_map')
                  )
              
            )
            
     # End of the Map Section       
    ),
    
    #####################  Stats Section #####################
    tabItem(tabName = "stats",
            
            fluidRow(
              
              box('Ranking dos bairros', width = 10, solidHeader = T,
                  plotlyOutput('rank_neigh')
                  ),
              
              # Stats options
              box('Opções de estudo', width = 2, solidHeader = T,
                  selectInput('year.ch.stats', 'Qual ano gostaria de escolher?',
                              choices = c(2015, 2016), multiple = F),
                  selectInput('crime.type.stats', 'Que tipo de crime gostaria de visualizar no mapa?',
                              choices = c('Todos os tipos', crime_types),
                              selected = 'Todos os tipos',
                              multiple = F),
                  actionButton('go_stats', 'Submeter')
              )
              
              
            )
            
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
