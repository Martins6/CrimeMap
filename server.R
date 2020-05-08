server <- function(input, output) {
  
  ########################### / INPUT / ############################
  ########################### MAP ##################################
  ########################### *********** GEO-TAGGED DATA ##################################
  SP_data <- eventReactive(input$go_map.plot,{
    # The geospatial data
    SP <- readRDS("data/SP.rds")
    # The year choosen
    ych <- input$year.ch
    # Type of crime choosen
    typ.crime <- input$crime.type
    
    if(typ.crime == 'Todos os tipos'){
      
      path.file <- paste('data/SPcrimetibble', ych, '.rds', sep = '')
      
      # All crimes commited in the whole year
      res.titulo <- paste('Número de Crimes em ', ych, sep = '')
      
    }else{
      
      path.file <- paste('data/SPcrimetibble', ych, typ.crime, '.rds', sep = '')
      
      # All of the selected crime commited in the whole year
      res.titulo <- paste('Número do Crime Selecionado em ', ych, sep = '')
      
    }
    # Retrieving dataframe choosen
    SPtibble <- readRDS(path.file)
    # Merging with the geotag
    res.data <- merge(SP, SPtibble)
    
    return(list(df = res.data, col.df = res.titulo))
  })
  
  ########################### / OUTPUT / ############################
  ########################### MAP ##################################
  output$map.plot <- renderLeaflet({
    
    SP <- SP_data()$df
    zcol.ch <- SP_data()$col.df
    print(head(SP))
    print(zcol.ch)
    
    res <- mapview(SP, zcol = zcol.ch)
    
    return(res@map)
  })
  
  

  # End of the server function 
}

