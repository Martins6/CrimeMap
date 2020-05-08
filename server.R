server <- function(input, output) {
  
  ########################### / INPUT / ############################
  ########################### MAP ##################################
  # year_choosen <- eventReactive(input$go_map.plot,{
  #   
  #   res <- input$year.ch %>% as.character()
  #   
  #   return(res)
  # })
  SP_data <- eventReactive(input$go_map.plot,{

    SP <- readRDS("data/SP.rds")
    
    ych <- input$year.ch
    path.file <- paste('data/SPcrimetibble', ych, '.rds', sep = '')
    SPtibble <- readRDS(path.file)
    
    res <- merge(SP, SPtibble)

    return(res)
  })
  
  y.choosen <- eventReactive(input$go_map.plot,{
    
    ych <- input$year.ch
    
    res <- paste('Número de Crimes em ', ych, sep = '')
    
    return(res)
  })
  
  ########################### / OUTPUT / ############################
  ########################### MAP ##################################
  output$map.plot <- renderLeaflet({
    
    SP <- readRDS("data/SP.rds")
    
    ych <- input$year.ch
    path.file <- paste('data/SPcrimetibble', ych, '.rds', sep = '')
    SPtibble <- readRDS(path.file)
    
    SP <- merge(SP, SPtibble)
    
    zcol.ch <- paste('Número de Crimes em ', ych, sep = '')
    a <- mapview(SP, zcol = zcol.ch)
    
    return(a@map)
  })
  
  

  # End of the server function 
}

