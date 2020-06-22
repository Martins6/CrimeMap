########################### / HEADER / #################################
header <- dashboardHeader(title = "Crimes em São Paulo")

########################## / SIDEBAR / #################################
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Mapa de São Paulo", tabName = "map"),
    menuItem("Análise dos Bairros", tabName = "neighborhoods"),
    menuItem("Modelagem de Risco de Assalto", tabName = "rob_model"),
    menuItem("Sobre", tabName = "about")
  )
)

################################### / BODY / ##########################
body <- dashboardBody(
  tabItems(
    #####################  Map Section #####################
    tabItem(tabName = "map",
            
            fluidRow(
              # Crime map plot
              box(title = 'Mapeamento dos Crime', width = 10, solidHeader = T, footer = 'Fonte: SSP',
                  leafletOutput('map.plot')
                  ),
              # Map options
              box(title = 'Opções de Visualizações', width = 2, solidHeader = T,status = 'primary',
                  selectInput('year.ch', 'Qual ano gostaria de escolher?',
                              choices = c(2015, 2016), multiple = F),
                  selectInput('crime.type', 'Que tipo de crime gostaria de visualizar no mapa e gráfico abaixo?',
                              choices = c('Todos os tipos', crime_types),
                              selected = 'Todos os tipos',
                              multiple = F),
                  actionButton('go_map.plot', 'Submeter')
                  )
            ),
            
            fluidRow(
              # Time Series Plot
              box(title = 'Série Temporal Anual do Crime', width = 12, footer = 'Fonte: SSP',
                  plotlyOutput('ts_map')
                  )
              
            )
            
     # End of the Map Section       
    ),
    
    #####################  Neighborhood Section #####################
    tabItem(tabName = "neighborhoods",
            
            fluidRow(
              
              # Plot of the ranking of the best Neighborhoods
              box(title = 'Ranking dos Bairros', width = 10, solidHeader = T, footer = 'Fonte: SSP',
                  plotlyOutput('rank_neigh', height = '500px')
                  ),
              
              # Options
              box(title = 'Opções', width = 2, solidHeader = T, status = 'primary',
                  selectInput('year.ch.neigh', 'Qual ano gostaria de escolher?',
                              choices = c(2015, 2016), multiple = F),
                  selectInput('crime.type.neigh', 'Que tipo de crime gostaria de visualizar no mapa?',
                              choices = c('Todos os tipos', crime_types),
                              selected = 'Todos os tipos',
                              multiple = F),
                  #helpText('Não precisa re-submeter a opção de bairro, caso deseje ver outro bairro.'),
                  actionButton('go_neigh', 'Submeter')
              )
            ),
            
            fluidRow(
              # Time Series of the crime in the specific Neighborhood choosen
              box(title = 'Série temporal do Crime no Bairro',
                  width = 10, solidHeader = T, footer = 'Fonte: SSP',
                  plotlyOutput('neighborhood_ts_plot')
                  ),
              box(title = 'Escolha um bairro', width = 2, solidHeader = T, status = 'primary',
                  selectInput('neigh.ts.opt', 'Qual bairro gostaria de ver a série no tempo?',
                              choices = Bairros.choices,
                              multiple = F)
                  )
            )
            
      # End of the Stats Section       
    ),
    
    #####################  Robbery Modelling Section #####################
    tabItem(tabName = "rob_model",
            
            fluidRow(
              # Descriptive Risk
              box(title = 'Mapeamento do risco nos bairros', width = 10,
                  leafletOutput('map.risk')
                  ),
              box(title = 'Opções para visualização', width = 2, solidHeader = T, status = 'primary',
                  selectInput('year.ch.risk', 'Qual ano gostaria de escolher?',
                              choices = c(2015, 2016), multiple = F),
                  actionButton('go.map.risk', 'Submeter')
                  )
              
            )
            
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
