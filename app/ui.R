########################### / HEADER / #################################
header <- dashboardHeader(title = "Crimes em São Paulo")

########################## / SIDEBAR / #################################
sidebar <- dashboardSidebar(
  sidebarMenu(
    # Tab 1 - Whole City
    menuItem("Mapa de São Paulo", tabName = "map"),
    # Tab 2 - Neighborhoods
    menuItem("Análise dos Bairros", tabName = "neighborhoods"),
    # Tab 3 - Modelling Theft Data
    menuItem("Mapa de Calor do Crime de Assalto", tabName = "rob_model"),
    # Tab 4 - About
    menuItem("Sobre", tabName = "about")
  )
)
################################### / BODY / ##########################
body <- dashboardBody(
  tabItems(
    #####################  Whole City Section #####################
    tabItem(tabName = "map",
            
            fluidRow(
              # Whole City - Crime map plot
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
              # Whole City - Time Series Plot
              box(title = 'Série Temporal Anual do Crime', width = 12, footer = 'Fonte: SSP',
                  plotlyOutput('ts_map')
                  )
              
            )
            
     # End of the Whole City Section       
    ),
    
    #####################  Neighborhood Section #####################
    tabItem(tabName = "neighborhoods",
            
            fluidRow(
              # Display of the data from the ranking of the best Neighborhoods
              tabBox(
                title = 'Ranking dos Bairros',
                width = 10,
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "neighborhood_ranking",
                # Resumo Retorno Percentual
                tabPanel("Gráfico de Comparação", plotlyOutput('rank_neigh', height = '500px')),
                # Histórico da Métrica
                tabPanel("Tabela de Comparação", DT::dataTableOutput('rank_datatable'))
              ),
              
              # Options
              box(title = 'Opções', width = 2, solidHeader = T, status = 'primary',
                  selectInput('year.ch.neigh', 'Qual ano gostaria de escolher?',
                              choices = c(2015, 2016), multiple = F),
                  selectInput('crime.type.neigh', 'Que tipo de crime gostaria de visualizar no mapa?',
                              choices = c('Todos os tipos', crime_types),
                              selected = 'Todos os tipos',
                              multiple = F),
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
            
      # End of the Neighborhood Section       
    ),
    
    #####################  Robbery Modelling Section #####################
    tabItem(tabName = "rob_model",
            
            fluidRow(
              
              tabBox(
                width = 10,
                title = "Mapeamentos",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset_heat_map",
                # Frequency between number of robberies and population
                tabPanel("Frequência entre N. de Assaltos e População", leafletOutput('freq_rob_pop_risk')),
                # Prevalence Risk
                tabPanel("Risco de prevalência", leafletOutput('prevalence_risk'))
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
