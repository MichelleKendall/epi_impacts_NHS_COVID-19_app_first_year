get_map_plots <- function(cleaned.app.data) {
  
    ltla.average.uptake <- cleaned.app.data %>%
        group_by(ltla, ltla_name) %>%
        summarise("av.uptake" = mean(uptake, na.rm=T), 
              "population"=mean(population, na.rm=T)
             ) %>%
    arrange(av.uptake)
  
    # censor City of London because 76% seems unreasonable!
    ltla.average.uptake <- ltla.average.uptake %>%
        filter(ltla_name != "City of London")
  
   
    engwalesmap <- st_read("data/engwales.shp") # load shapefile for maps
  
 
    # set all the Buckinghamshire ltlas to have the same code so that the shapefile and data files match
    engwalesmap$lad19cd[which(engwalesmap$lad19nm %in% c("Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe"))] <- ltla.average.uptake$ltla[which(ltla.average.uptake$ltla_name == "Buckinghamshire")]
  
    # right join so that we don't plot Scotland and NI
    ltla.uptake.to.map <- right_join(engwalesmap, ltla.average.uptake, by=c("lad19cd" = "ltla"))
  
    # convert the shapefile coordinates into longitudes and latitudes, ready for the leaflet package
    ltla.uptake.to.map <- ltla.uptake.to.map %>% st_transform('+proj=longlat +datum=WGS84') 
  
    uptake_step <- 0.05
    bins_uptake <- seq(0.1, 0.45, by=uptake_step)
    bins_uptake_length <- length(bins_uptake)
  
    pal_uptake <- colorBin("viridis", domain = ltla.average.uptake$av.uptake, bins = bins_uptake, reverse=TRUE)
  
    plots <- list()
    
    plots$l0 <- leaflet(ltla.uptake.to.map, options = leafletOptions(zoomSnap=0.01)) %>%
        addPolygons(fillColor = ~pal_uptake(av.uptake),
                weight = 0.5,
                opacity = 0.5,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.8) %>%
        addLegend("topright",
              colors = pal_uptake(bins_uptake)[1:(bins_uptake_length - 1)],
              labels = paste0(bins_uptake*100+1, "-", (bins_uptake+uptake_step)*100, "%" )[1:(bins_uptake_length - 1)],
              title = "Uptake<br>(% population)",
              opacity=1) %>% 
        setMapWidgetStyle(list(background= "white")) %>%
        setView(lng=-1.3, lat=53, zoom=5.8)  
  
    if (resave.results) { write_csv(ltla.uptake.to.map, file="results/Fig_1b.csv") }
  
    
    averted.by.la <- read_csv("results/averted.first.year.by.ltla.csv", show_col_types=F)
  
    averted.by.la <- left_join(averted.by.la, ltla.average.uptake) %>%  # to get the ltla codes and populations
        mutate("averted_per_1k" = averted/population * 1000)
  
    # need to duplicate / separate the "Cornwall and Isles of Scilly" entries in averted.by.la
    #engwalesmap[(which(engwalesmap$lad19nm %in% c("Cornwall", "Isles of Scilly"))),c("lad19nm","lad19cd")]
  
    CISrow <- averted.by.la %>% filter(ltla_name == "Cornwall and Isles of Scilly")
    CISrow_C <- CISrow
    CISrow_IS <- CISrow
    CISrow_C[,"ltla_name"] <- "Cornwall"
    CISrow_IS[,"ltla_name"] <- "Isles of Scilly"
    CISrow_C[,"ltla"] <- "E06000052"
    CISrow_IS[,"ltla"] <- "E06000053"
  
    # naively split the cases averted between the two, according to population size
    CISrow_C[,"averted"] <- CISrow[,"averted"] * (568210/(568210+2280))
    CISrow_IS[,"averted"] <- CISrow[,"averted"]  * (2280/(568210+2280))
  
    averted.by.la <- rbind(
        averted.by.la,
        CISrow_C, 
        CISrow_IS)
    
  
    # right join so that we don't plot Scotland and NI
    ltla.averted.to.map <- right_join(engwalesmap, averted.by.la, by=c("lad19cd" = "ltla"))
  
    # convert the shapefile coordinates into longitudes and latitudes, ready for the leaflet package
    ltla.averted.to.map <- ltla.averted.to.map %>% st_transform('+proj=longlat +datum=WGS84') 
  
    averted_step <- 1000
    bins_averted <- seq(0, 14000, by=averted_step)
    bins_averted_length <- length(bins_averted)
  
    pal_averted <- colorBin("inferno", domain = ltla.averted.to.map$averted, bins = bins_averted, reverse=TRUE)
  
    plots$l1 <- leaflet(ltla.averted.to.map, options = leafletOptions(zoomSnap=0.01)) %>%
        addPolygons(fillColor = ~pal_averted(averted),
                weight = 0.5,
                opacity =0.5,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.8) %>%
        addLegend("topright",
              colors = pal_averted(bins_averted)[1:(bins_averted_length - 1)],
              labels = paste0(bins_averted+1, "-", bins_averted + 1000)[1:(bins_averted_length - 1)],
              title = "Cases averted",
              opacity=1) %>% 
        setMapWidgetStyle(list(background= "white")) %>%
        setView(lng=-1.3, lat=53, zoom=5.8) 
  
    averted_perc_step <- 5
    bins_averted_perc <- seq(0, 30, by=averted_perc_step)
    bins_averted_perc_length <- length(bins_averted_perc)
  
    pal_averted_perc <- colorBin("magma", domain = ltla.averted.to.map$averted.as.perc.of.counterfactual, bins = bins_averted_perc, reverse=TRUE)
  
    plots$l2 <- leaflet(ltla.averted.to.map, options = leafletOptions(zoomSnap=0.01)) %>%
        addPolygons(fillColor = ~pal_averted_perc(averted.as.perc.of.counterfactual),
                weight = 0.5,
                opacity = 0.5,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.8) %>%
        addLegend("topright",
              colors = pal_averted_perc(bins_averted_perc)[1:(bins_averted_perc_length-1)],
              labels = paste0(bins_averted_perc+1, "-", bins_averted_perc+averted_perc_step,"%")[1:(bins_averted_perc_length-1)],
              title ="% reduction<br>in cases",
              opacity=1) %>% 
        setMapWidgetStyle(list(background= "white")) %>%
        setView(lng=-1.3, lat=53, zoom=5.8) 
  
    
    if (resave.results) {   write_csv(ltla.averted.to.map, file="results/Fig6de.csv") }
    
    plots
}