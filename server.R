server <- function(input, output) {
  
  ########################### / INPUT / ############################
  ########################### MAP ##################################
  ########################### *********** Geotagged Data ##################################
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
  
  ########################### STATS ##################################
  ########################### *********** Geotagged Data ##################################
  SP_data.stats <- eventReactive(input$go_stats,{
    # The geospatial data
    SP <- readRDS("data/SP.rds")
    # The year choosen
    ych <- input$year.ch.stats
    # Type of crime choosen
    typ.crime <- input$crime.type.stats
    
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
  ########################### RISK MODEL ####################################
  ########################### *********** Probability Map ##################################
  theft_data <- eventReactive(input$go.model, {
    
    # Year choosen
    ych <- input$year.ch.model
    # Neighborhood choosen
    neigh <- input$neigh.model
    
    # The geospatial data
    SP <- readRDS("data/SP.rds") %>% 
      filter(Bairros == neigh)
    
    # Calculating bounding box for further wrangling
    boundbox.sp <- SP$geometry %>% sf::as_Spatial() %>% bbox()
    
    # Reading crime data from the especific choosen year
    path.file <- paste('data/', ych, '_roubo_data.rds', sep = '')
    crime <- readRDS(path.file)
    
    res1 <- crime %>% 
      filter(between(latitude, boundbox.sp[2,1], boundbox.sp[2,2]) &
               between(longitude, boundbox.sp[1,1], boundbox.sp[1,2]))
    
    print('Hakuna Matata')
    res1 %>% glimpse()
    print('Isso é viver, é aprender!')
    SP %>% glimpse()
    
    return(list(df = res1, sp.df = SP))
  })
  ########################### *** Number of squares ##################################
  number_of_divisions <- eventReactive(input$go.model,{
    res <- input$h.model
    return(res)
  })
  
  
  ########################### / OUTPUT / ############################
  ########################### MAP ##################################
  ########################### *********** Mapview Plot ##################################
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
  
  ########################### *********** Time Series for whole SP ##################################
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
           #title = 'Quantidade de crime cometido selecionado ao longo do ano',
           y = '')
    
    return(total.crime.by.month.plot)
    
  })
  ########################### STATS ##################################
  ########################### *********** Neighborhood Rank ##################################
  output$rank_neigh <- renderPlotly({
    
    # Taking total crime and by month of every neighborhood
    SP_data.stats()$df %>%
      as_tibble() %>%
      select(-geometry) %>%
      select(all_of(c(SP_data.stats()$col.df, 'Bairros'))) %>% 
      `colnames<-`(c('Quant', 'Bairros')) %>% 
      dplyr::arrange(desc(Quant)) %>% 
      mutate(Bairros = factor(Bairros, levels = .$Bairros),
             Rank = 1:n()) %>% 
      # Plotting
      ggplot() +
      geom_bar(aes(x = Bairros, y = Quant), 
               stat="identity", width=.5, fill="tomato3") +
      labs(title="") +
      geom_point(aes(x = Bairros, y = Rank)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
      labs(y = 'Número de Crimes Cometidos')
    

  })
  ########################### *********** Time Series of the Crime in the Neighborhood ################
  output$neighborhood_ts_plot <- renderPlotly({
    
    # Taking total crime and by month of every neighborhood
    col_vec <- SP_data.stats()$df %>%
      as_tibble() %>%
      select(-geometry) %>%
      select(-all_of(c(SP_data.stats()$col.df))) %>% 
      filter(Bairros == input$neigh.ts.opt) %>% 
      as.matrix() %>% 
      as.vector()
    
    total.crime.by.month <- 
      col_vec %>% 
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
  ########################### RISK MODEL ####################################
  ########################### *********** Probability Map ##################################
  output$map.prob <- renderPlot({
    
    # Let us put our crime dataset into a proper spatial format for point patterns
    crime.sp <- theft_data()$df %>% select(longitude, latitude) %>% SpatialPoints()
    crime.ppp <- crime.sp %>% maptools::as.ppp.SpatialPoints()
    
    # How many parts to divide the region in each dimension?
    h <- number_of_divisions()

    # Divinding the region into quadrat or 'little squares'
    qcount <- spatstat::quadratcount(crime.ppp, nx = h, ny = h) %>% 
      as_tibble()
    
    qcount %>% glimpse()
    # adapting the tibble
    ## Auxiliary fun
    decomposing <- function(x){
      x %>%
        str_replace('\\[', replacement = '') %>%
        strsplit(',') %>%
        unlist() %>%
        first() %>%
        as.numeric()
    }
    dt <- qcount %>% 
      mutate(y = unlist(lapply(y, decomposing)),
             x = unlist(lapply(x, decomposing)),
             crime.event = if_else(n > 0,
                                   1,
                                   0)) %>% 
      select(-n)
    
    # For more speedy calculations, sampling to 100 squares
    dt.aux <- dt %>% sample_n(100) 
    # Fitting GLGM or GLMMM in a Spatial Statistics context
    t0 <- Sys.time()
    fit.glmm <- spaMM::fitme(crime.event ~ 1 + Matern(1|x + y),
                             data = dt.aux,
                             family = binomial(link = "logit"),
                             method = 'PQL/L')
    t1 <- Sys.time()
    print(t1 - t0)
    
    # Adjusting the spatial setting for the plot
    sp.sp <- theft_data()$sp.df %>% as_Spatial()
    sp.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
    
    # Plotting the prob. map
    
    # Create an empty raster with the same extent and resolution as the bioclimatic layers
    latitude_raster <- longitude_raster <- raster::raster(nrows = 100,
                                                          ncols = 100,
                                                          ext = raster::extent(sp.sp))
    # Change the values to be latitude and longitude respectively
    longitude_raster[] <- coordinates(longitude_raster)[,1]
    latitude_raster[] <- coordinates(latitude_raster)[,2]
    
    # Now create a final prediction stack of the 2 variables we need
    pred_stack <- raster::stack(longitude_raster,
                                latitude_raster)
    
    # Rename to ensure the names of the raster layers in the stack match those used in the model
    names(pred_stack) <- c("x", "y")
    
    # Predicting
    predicted_prevalence_raster <- raster::predict(pred_stack, fit.glmm)
    predicted_prevalence_raster_oromia <- raster::mask(predicted_prevalence_raster, sp.sp)
    points(crime.ppp)
    
    plot(predicted_prevalence_raster_oromia)
    
  })
  

  # End of the server function 
}

