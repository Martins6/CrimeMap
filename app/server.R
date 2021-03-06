server <- function(input, output) {
  ########################### / INPUT / ############################
  ########################### Whole City ##################################
  ########################### *********** Geotagged Data of the City of SP ###################
  SP_data <- eventReactive(input$go_map.plot, {
    # The geospatial data
    SP <- readRDS("data/SP.rds")
    # The year choosen
    ych <- input$year.ch
    # Type of crime choosen
    typ.crime <- input$crime.type
    
    if (typ.crime == 'Todos os tipos') {
      path.file <- paste('data/SPcrimetibble', ych, '.rds', sep = '')
      
      # All crimes commited in the whole year
      res.titulo <- paste('Número de Crimes em ', ych, sep = '')
      
    } else{
      path.file <-
        paste('data/SPcrimetibble', ych, typ.crime, '.rds', sep = '')
      
      # All of the selected crime commited in the whole year
      res.titulo <-
        paste('Número do Crime Selecionado em ', ych, sep = '')
      
    }
    # Retrieving dataframe choosen
    SPtibble <- readRDS(path.file)
    # Merging with the geotag
    res.data <- merge(SP, SPtibble)
    
    return(list(df = res.data, col.df = res.titulo))
  })
  
  ########################### Neighborhoods ##################################
  ############ *********** Geotagged Data of the Neighborhood in the City of SP ###################
  SP_data.neigh <- eventReactive(input$go_neigh, {
    # The geospatial data
    SP <- readRDS("data/SP.rds")
    # The year choosen
    ych <- input$year.ch.neigh
    # Type of crime choosen
    typ.crime <- input$crime.type.neigh
    
    if (typ.crime == 'Todos os tipos') {
      path.file <- paste('data/SPcrimetibble', ych, '.rds', sep = '')
      
      # All crimes commited in the whole year
      res.titulo <- paste('Número de Crimes em ', ych, sep = '')
      
    } else{
      path.file <-
        paste('data/SPcrimetibble', ych, typ.crime, '.rds', sep = '')
      
      # All of the selected crime commited in the whole year
      res.titulo <-
        paste('Número do Crime Selecionado em ', ych, sep = '')
      
    }
    # Retrieving dataframe choosen
    SPtibble <- readRDS(path.file)
    # Merging with the geotag
    res.data <- merge(SP, SPtibble)
    
    return(list(df = res.data, col.df = res.titulo))
  })
  ########################### Theft Risk ####################################
  ########## *********** Frequency between Number of Robberies and Population Map ##############
  theft_freq_map <- eventReactive(input$go.map.risk, {
    # Year choosen
    ych <- input$year.ch.risk
    
    # The geospatial data
    SP <- readRDS("data/SP.rds")
    # Also transforming the desired region
    sp.sp <- SP %>% as_Spatial()
    sp.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
    zone <- 23
    sp.sp <-
      spTransform(sp.sp, CRS(paste(
        "+proj=utm +zone=", zone, "+datum=WGS84", sep = ''
      )))
    SP <- sp.sp %>% st_as_sf()
    
    # Calculating bounding box for further wrangling
    boundbox.sp <- SP$geometry %>% sf::as_Spatial() %>% bbox()
    
    # Reading crime data from the especific choosen year
    path.file <- paste('data/', ych, '_roubo_data.rds', sep = '')
    crime <- readRDS(path.file)
    # Let us put our crime dataset into a proper spatial format for point patterns
    crime.sp <-
      crime %>% select(longitude, latitude) %>% SpatialPoints()
    crime.sp@proj4string <-
      CRS('+proj=longlat +datum=WGS84 +no_defs')
    # Transforming to UTM coordinates
    zone <- 23
    crime.sp <-
      spTransform(crime.sp, CRS(paste(
        "+proj=utm +zone=", zone, "+datum=WGS84", sep = ''
      )))
    crime.sf <- crime.sp %>% st_as_sf()
    
    # Collecting the population of the municipalities in Sao Paulo
    webpage <-
      read_html(
        'https://pt.wikipedia.org/wiki/Lista_dos_distritos_de_S%C3%A3o_Paulo_por_popula%C3%A7%C3%A3o'
      )
    
    municip_html <-
      html_nodes(x = webpage, 'table.wikitable tbody tr td:nth-child(2) a')
    municip_name_data <- html_text(municip_html)
    pop_html <-
      html_nodes(x = webpage, 'table.wikitable tbody tr td:nth-child(3)')
    # Deleting the last value because is just a "\n"
    pop_data <- html_text(pop_html)
    
    # Joining them in a tibble
    cleaning_bairros <- function(x) {
      a <- stringi::stri_trans_general(str = x,
                                       id = "Latin-ASCII")
      b <- str_to_lower(a)
      d <- str_squish(b)
      return(d)
    }
    
    munic_pop <- 
      tibble(Bairros = municip_name_data,
             # Deleting last value because is just a "\n" string
             Pop = pop_data[-length(pop_data)]) %>%
      mutate(Bairros = cleaning_bairros(Bairros),
             Pop = 1000 * as.double(str_replace(Pop, '\n', '')))
    
    hey <- st_contains(SP, crime.sf) %>%
      lengths()
    SP <- SP %>%
      mutate(Crime.count = hey,
             Bairros = cleaning_bairros(Bairros)) %>%
      full_join(munic_pop) %>%
      mutate(Risco = Crime.count / (Pop - Crime.count)) %>%
      mutate(Risco = round(Risco, digit = 3))
    
    return(SP)
  })
  ########## *********** Prevalence Map ##############
  theft_prevalence_map <- eventReactive(input$go.map.risk, {
    # Year choosen
    ych <- input$year.ch.risk
    
    dt.aux <-
      readRDS(paste('data/crime_by_square/year', ych, 'squares_500.rds', sep = ''))
    
    yes.cr <- dt.aux %>%
      select(x, y, crime.event) %>%
      filter(crime.event == 1) %>%
      SpatialPoints() %>%
      as('ppp')
    no.cr <- dt.aux %>%
      select(x, y, crime.event) %>%
      filter(crime.event == 0) %>%
      SpatialPoints() %>%
      as('ppp')
    
    # Kernel Density smoothing
    ## Diggle bandwith
    a1 <- density(yes.cr, sigma = bw.diggle)
    a2 <- density(no.cr, sigma = bw.diggle)
    
    prevalence_matrix <- (a1$v) / (a1$v + a2$v)
    
    # The geospatial data
    SP <- readRDS("data/SP.rds")
    # Also transforming the desired region
    sp.sp <- SP %>% as_Spatial()
    sp.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
    
    # Create an empty raster with the same extent and resolution as the Sao Paulo region
    predicted_raster <-
      raster::raster(
        nrows = nrow(prevalence_matrix),
        ncols = ncol(prevalence_matrix),
        ext = raster::extent(sp.sp)
      )
    # For some reason, the raster takes the inverse order of rows
    predicted_raster[] <-
      prevalence_matrix[nrow(prevalence_matrix):1,]
    predicted_raster_in_map <- raster::mask(predicted_raster, sp.sp)
  })
  ########## *********** Spatial Kernel Map ##############
  spatial_kernel_map <- eventReactive(input$go.map.risk, {
    
    # The geospatial data
    sp.sf <- readRDS("data/SP.rds")
    
    # UTM coordinates
    sp.sp <- sp.sf %>% as_Spatial()
    sp.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
    zone <- 23
    sp.sp <- spTransform(sp.sp, CRS(paste("+proj=utm +zone=",zone,"+datum=WGS84", sep = '')))
    sp.sf <- sp.sp %>% st_as_sf()
    
    # Putting into the format of the 'ppp'
    sp.sp <- as(sp.sf, "Spatial")
    sp.sp@proj4string <- CRS(as.character(NA))
    # Window of the points
    window <- maptools::as.owin.SpatialPolygons(sp.sp)
    
    # Year choosen
    ych <- input$year.ch.risk
    
    # Reading our crime dataset
    path.file <- paste('data/', ych, '_roubo_data.rds', 
                       sep = '')
    
    crime <- readRDS(path.file)
    
    # Let us put our crime dataset into a proper spatial format for point patterns
    crime.sp <- crime %>% select(longitude, latitude) %>% SpatialPoints()
    crime.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
    # Transforming to UTM coordinates
    zone <- 23
    crime.sp <- spTransform(crime.sp, CRS(paste("+proj=utm +zone=",zone,"+datum=WGS84", sep = '')))
    crime.sf <- crime.sp %>% st_as_sf()
    
    # Pure crime coordinates
    crime.coord <- matrix(unlist(crime.sf$geometry), ncol = 2, byrow = T)
    # Checking points outside the desired region
    inside.sp <- inside.owin(crime.coord[,1],
                             crime.coord[,2],
                             w = window)
    # Selecting only those that fall within
    crime.coord <- crime.coord[inside.sp,]
    
    # Creating the ppp data
    crime.ppp <- ppp(x = crime.coord[,1], y = crime.coord[,2],
                     window = window, check = T)
    
    # If there are duplicated versions let us jitter so not to lose any info
    if(any(duplicated(crime.ppp))){crime.ppp <- rjitter(crime.ppp, retry=TRUE, nsim=1, drop=TRUE)}
    
    # Proper rescalling of the values
    crime.ppp <- rescale(crime.ppp, 1000, 'kilometers')

    # Density using
    den.dig <- density(crime.ppp, sigma = bw.diggle, edge = T)
    
    spkm_matrix <- den.dig$v
    
    # The geospatial data
    SP <- readRDS("data/SP.rds")
    # Also transforming the desired region
    sp.sp <- SP %>% as_Spatial()
    sp.sp@proj4string <- CRS('+proj=longlat +datum=WGS84 +no_defs')
    
    # Create an empty raster with the same extent and resolution as the Sao Paulo region
    predicted_raster <-
      raster::raster(
        nrows = nrow(spkm_matrix),
        ncols = ncol(spkm_matrix),
        ext = raster::extent(sp.sp)
      )
    # For some reason, the raster takes the inverse order of rows
    predicted_raster[] <-
      spkm_matrix[nrow(spkm_matrix):1,]
    predicted_raster_in_map <- raster::mask(predicted_raster, sp.sp)
  })
  ########## *********** Extracting Values from Exact Locations the Maps ##############
  table_map_extraction <- eventReactive(input$go_table_adress_risk, {
    
    # Extracting the coordinates from the Adress given
    doc <-  mp_geocode(
      addresses = input$adress,
      key = key,
      quiet = TRUE
    )
    
    sf_point <- mp_get_points(doc)
    
    point_adress <-  
      sf_point %>% 
      st_coordinates() %>%
      as_vector() %>% 
      matrix(1,2)
    
    # Prevalence Map
    v1 <-
      raster::extract(theft_prevalence_map(), point_adress) %>%
      round(3) %>% 
      scales::percent()
    
    # Quantity of Thefts 
    v2 <- 
      raster::extract(spatial_kernel_map(), point_adress) %>%
      round(3)
    
    tibble(`Prevalência` = v1,
           `Quantidade de Assalto` = v2) %>% 
      datatable()
    
  })
  
  ########################### / OUTPUT / ############################
  ########################### Whole City ##################################
  ########################### *********** Mapview Plot ##################################
  output$map.plot <- renderLeaflet({
    # Options for the map
    mapviewOptions(basemaps = c(
      "CartoDB.Positron",
      "OpenStreetMap",
      "CartoDB.DarkMatter"
    ))
    
    # Retrieving data
    SP <- SP_data()$df
    zcol.ch <- SP_data()$col.df
    
    res <- mapview(
      SP,
      zcol = c(zcol.ch),
      col.regions = c('green', 'blue', 'red'),
      pop = stringr::str_to_title(SP$Bairros)
    )
    
    return(res@map)
  })
  
  ########################### *********** Time Series for whole SP ##################################
  output$ts_map <- renderPlotly({
    col.name.aux <- SP_data()$col.df
    # Taking just the crimes in each month by neighborhood
    SP <-
      SP_data()$df %>% select(-(all_of(col.name.aux))) %>% select(-Bairros)
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
      theme_bw() + #theme_economist() +
      labs(x = 'Mês',
           #title = 'Quantidade de crime cometido selecionado ao longo do ano',
           y = '')
    
    return(total.crime.by.month.plot)
    
  })
  
  ########################### *********** Rank of the Neighborhoods ##################################
  output$rank_neigh_best <- renderPlot({
    aux.col.name <- SP_data()$col.df
    
    aux <- SP_data()$df %>%
      as_tibble() %>% 
      select(all_of(c(aux.col.name, 'Bairros'))) %>%
      dplyr::arrange(desc(.data[[aux.col.name]])) %>% 
      head(5) %>% 
      dplyr::arrange(.data[[aux.col.name]]) %>% 
      mutate(Bairros = factor(Bairros, levels = .$Bairros))
    
    aux %>%
      ggplot() +
      geom_bar(
        aes(x = Bairros, y = .data[[aux.col.name]]),
        stat = "identity",
        width = .5,
        fill = "tomato3"
      ) +
      labs(
        title = "",
        y = 'Quantidade de crimes',
        x = '',
        caption = "source: Yahoo Finance"
      ) +
      coord_flip() +
      theme_minimal()
    
  })
  
  output$rank_neigh_worse <- renderPlot({
    aux.col.name <- SP_data()$col.df
    
    aux <- SP_data()$df %>%
      as_tibble() %>% 
      select(all_of(c(aux.col.name, 'Bairros'))) %>%
      dplyr::arrange(.data[[aux.col.name]]) %>% 
      mutate(Bairros = factor(Bairros, levels = .$Bairros))
    
    aux %>%
      head(5) %>% 
      ggplot() +
      geom_bar(
        aes(x = Bairros, y = .data[[aux.col.name]]),
        stat = "identity",
        width = .5,
        fill = "tomato3"
      ) +
      labs(
        title = "",
        y = 'Quantidade de crimes',
        x = '',
        caption = "source: Yahoo Finance"
      ) +
      coord_flip() +
      theme_minimal()
    
  })
  
  ########################### Neighborhoods ##################################
  ########################### *********** Neighborhood Rank - Plot ##################################
  output$rank_neigh <- renderPlotly({
    # Taking total crime and by month of every neighborhood
    SP_data.neigh()$df %>%
      as_tibble() %>%
      select(-geometry) %>%
      select(all_of(c(SP_data.neigh()$col.df, 'Bairros'))) %>%
      `colnames<-`(c('Quant', 'Bairros')) %>%
      dplyr::arrange(desc(Quant)) %>%
      mutate(Bairros = factor(Bairros, levels = .$Bairros),
             Rank = 1:n()) %>%
      # Plotting
      ggplot() +
      geom_bar(
        aes(x = Bairros, y = Quant),
        stat = "identity",
        width = .5,
        fill = "tomato3"
      ) +
      labs(title = "") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
      labs(y = 'Número de Crimes Cometidos')
    
    
  })
  ########################### *********** Neighborhood Rank - Table ##################################
  output$rank_datatable <- renderDataTable({
    # Taking total crime and by month of every neighborhood
    SP_data.neigh()$df %>%
      as_tibble() %>%
      select(-geometry) %>%
      select(all_of(c(SP_data.neigh()$col.df, 'Bairros'))) %>%
      `colnames<-`(c('Quant', 'Bairros')) %>%
      dplyr::arrange(desc(Quant)) %>%
      mutate(Bairros = factor(Bairros, levels = .$Bairros),
             Rank = 1:n()) %>%
      rename(Quantidade = Quant) %>%
      select(Bairros, Quantidade, Rank) %>% 
      datatable(class = 'cell-border stripe', rownames = FALSE)
    
  })
  ########################### *********** Time Series of the Crime in the Neighborhood ################
  output$neighborhood_ts_plot <- renderPlotly({
    # Taking total crime and by month of every neighborhood
    col_vec <- SP_data.neigh()$df %>%
      as_tibble() %>%
      select(-geometry) %>%
      select(-all_of(c(SP_data.neigh()$col.df))) %>%
      filter(Bairros == input$neigh.ts.opt) %>%
      as.matrix() %>%
      as.vector()
    
    col_vec <- col_vec[-1]
    
    total.crime.by.month <-
      col_vec %>%
      # Transforming into a data-frame
      enframe() %>%
      # Changing the name of the months
      mutate(value = as.double(value),
             name = as.integer(name)) %>%
      rename(Mês = name,
             Quantidade = value) %>%
      drop_na()
    
    # Plotting
    total.crime.by.month.plot <-
      total.crime.by.month %>%
      ggplot(aes(x = `Mês`, y = Quantidade)) +
      geom_path() +
      geom_point() +
      xlim(as.character(month(total.crime.by.month$`Mês`, label = T))) +
      theme_bw() + #theme_economist() +
      labs(x = 'Mês',
           #title = 'Quantidade do crime selecionado cometido ao longo do ano',
           y = '')
    
    return(total.crime.by.month.plot)
  })
  ########################### Theft Risk ####################################
  ########################### *********** Frequency between Number of Robberies and Population Map ##############
  output$freq_rob_pop_risk <- renderLeaflet({
    `Frequência` <- theft_freq_map()
    
    res <- mapview(
      `Frequência`,
      zcol = 'Risco',
      col.regions = c('green', 'blue', 'red'),
      pop = `Frequência`$Bairros
    )
    
    return(res@map)
    
  })
  ########## *********** Prevalence Map ##############
  output$prevalence_map <- renderLeaflet({
    predicted_raster_in_map <- theft_prevalence_map()
    
    Mapa <- predicted_raster_in_map
    res <- mapview(Mapa)
    return(res@map)
    
  })
  ########## *********** Theft Quantity Map ##############
  output$theft_quant_map <- renderLeaflet({
    predicted_raster_in_map <- spatial_kernel_map()
    
    Mapa <- predicted_raster_in_map
    res <- mapview(Mapa)
    return(res@map)
    
  })
  ########## *********** Datatable Exact Value ##############
  output$table_of_maps <- renderDataTable({
    df <- table_map_extraction()
    
    df
  })
  
  
  
  # End of the server function
}
