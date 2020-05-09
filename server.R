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
    
    # Options for the map
    mapviewOptions(basemaps = c("CartoDB.Positron", "OpenStreetMap", "CartoDB.DarkMatter"))
    
    # Retrieving data
    SP <- SP_data()$df
    zcol.ch <- SP_data()$col.df
    
    res <- mapview(SP, zcol = zcol.ch,
            col.regions = c('blue', 'red'))
    
    return(res@map)
  })
  
  output$ts_map <- renderPlotly({
    
    col.name.aux <- SP_data()$col.df
    # Taking just the crimes in each month by neighborhood
    SP <- SP_data()$df %>% select(-(all_of(col.name.aux))) %>% select(-Bairros)
    # Transforming into a matrix in order to perform the calculations that we wish to.
    SP <- SP %>% as_tibble() %>% select(-geometry) %>% as.matrix()
    one_vec <- rep(1, nrow(SP)) %>% as.matrix()
    
    total.crime.by.month <- 
      # Calculating
      (t(one_vec) %*% SP) %>% 
      as.vector() %>% 
      # Transforming into a data-frame
      enframe() %>% 
      # Changing the name of the months
      mutate(value = as.double(value),
             name = as.integer(name)) %>%
      rename(Mês = name,
             Quantidade = value)
    
    # Plotting
    total.crime.by.month.plot <- 
      total.crime.by.month %>% 
      ggplot(aes(x = `Mês`, y = Quantidade)) +
      geom_path() +
      geom_point() +
      xlim(as.character(month(total.crime.by.month$`Mês`, label = T))) +
      theme_economist() +
      labs(x = 'Mês',
           title = 'Quantidade do crime selecionado cometido ao longo do ano',
           y = '')
    
    return(total.crime.by.month.plot)
    
  })
  ########################### STATS ##################################
  output$rank_neigh <- renderPlotly({
    
    # # Taking total crime and by month of every neighborhood
    # # SP <- SP_data()$df %>%
    # #   as_tibble() %>% 
    # #   select(-geometry) %>% 
    # #   filter(Bairros == input$)
    #   
    # # Transforming into a matrix in order to perform the calculations that we wish to.
    # SP <- SP %>% as_tibble() %>% select(-geometry) %>% as.matrix()
    # one_vec <- rep(1, nrow(SP)) %>% as.matrix()
    # 
    # total.crime.by.month <- 
    #   # Calculating
    #   (t(one_vec) %*% SP) %>% 
    #   as.vector() %>% 
    #   # Transforming into a data-frame
    #   enframe() %>% 
    #   # Changing the name of the months
    #   mutate(value = as.double(value),
    #          name = as.integer(name)) %>%
    #   rename(Mês = name,
    #          Quantidade = value)
    # 
    # # Plotting
    # total.crime.by.month.plot <- 
    #   total.crime.by.month %>% 
    #   ggplot(aes(x = `Mês`, y = Quantidade)) +
    #   geom_path() +
    #   geom_point() +
    #   xlim(as.character(month(total.crime.by.month$`Mês`, label = T))) +
    #   theme_economist() +
    #   labs(x = 'Mês',
    #        title = 'Quantidade do crime selecionado cometido ao longo do ano',
    #        y = '')
    # 
    # return(total.crime.by.month.plot)
    
  })
  

  # End of the server function 
}

