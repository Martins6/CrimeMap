########################### / HEADER / #################################
header <- dashboardHeader(title = "Crime Map - São Paulo")

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
    tabItem(tabName = "about",
            
            fluidRow(
              box('Map', width = 10
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
