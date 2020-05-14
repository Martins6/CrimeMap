########################### / HEADER / #################################
header <- dashboardHeader(title = "Crimes em São Paulo")

########################## / SIDEBAR / #################################
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Mapa de São Paulo", tabName = "map"),
    menuItem("Análise dos Bairros", tabName = "stats"),
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
              
              box(title = 'Série Temporal Anual do Crime', width = 12, footer = 'Fonte: SSP',
                  plotlyOutput('ts_map')
                  )
              
            )
            
     # End of the Map Section       
    ),
    
    #####################  Stats Section #####################
    tabItem(tabName = "stats",
            
            fluidRow(
              
              # Ranking of the best Neighborhoods
              box(title = 'Ranking dos Bairros', width = 10, solidHeader = T, footer = 'Fonte: SSP',
                  plotlyOutput('rank_neigh', height = '500px')
                  ),
              
              # Options
              box(title = 'Opções', width = 2, solidHeader = T, status = 'primary',
                  selectInput('year.ch.stats', 'Qual ano gostaria de escolher?',
                              choices = c(2015, 2016), multiple = F),
                  selectInput('crime.type.stats', 'Que tipo de crime gostaria de visualizar no mapa?',
                              choices = c('Todos os tipos', crime_types),
                              selected = 'Todos os tipos',
                              multiple = F),
                  helpText('Não precisa re-submeter a opção de bairro, caso deseje ver outro bairro.'),
                  selectInput('neigh.ts.opt', 'Qual bairro gostaria de ver a série no tempo?',
                              choices = Bairros.choices,
                              multiple = F),
                  actionButton('go_stats', 'Submeter')
              )
            ),
            
            fluidRow(
              # Time Series of the crime in the specific Neighborhood choosen
              box(title = 'Série temporal do Crime Selecionado no Bairro Escolhido',
                  width = 12, solidHeader = T, footer = 'Fonte: SSP',
                  plotlyOutput('neighborhood_ts_plot')
                  )
            )
            
      # End of the Stats Section       
    ),
    
    #####################  Modelling Section #####################
    tabItem(tabName = "model",
            
            fluidRow(
              box(title = 'Mapeamento da probabilidade de assalto em determinado bairro', width = 10,
                  plotOutput('map.prob')
                  ),
              box(title = 'Opções para a modelagem', width = 2, solidHeader = T, status = 'primary',
                  selectInput('year.ch.model', 'Qual ano gostaria de escolher?',
                              choices = c(2015, 2016), multiple = F),
                  helpText('Para ter uma melhor compreensão dos parâmetros, por favor, consultar a aba "Sobre".'),
                  selectInput('neigh.model', label = 'Qual bairro deseja modelar?',
                              choices = sp.sf$Bairros, multiple = F),
                  numericInput('h.model', 'Em quantos pedaços deseja dividir os lados do bairro?',
                               value = 10, min = 10, max = 100),
                  actionButton('go.model', 'Submeter')
                  
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
