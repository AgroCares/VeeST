##visualise oeverprofielen
plot_oeverprofielen <- function(){
  #loop through
  uniques <- unique(oevers$oever)
  for(i in 1:length(uniques)){
    #select
    rel.oever <- subset(oevers, oevers$oever == uniques[i])
    
    ###retrieve cardinal direction of the transect----
      #i.e. "zuid" = transect is moving from noord to zuid
      #puntnummer (1) is thus located in the northern part
    cardinal_direction <- get_cardinal_direction(name_oever = uniques[i])
    
    ###retrieve the distances between points and scale to the waterlijn (waterlijn = 0)----
    rel.oever$distance <- st_distance(rel.oever, subset(rel.oever, rel.oever$puntnummer == 1)) |> as.numeric() #distance in metres
  
    #scale to 0 (waterlijn)
    rel.waterlijn <- subset(rel.oever, rel.oever$opmerking == "waterlijn")
    scale_distance <- 0 - mean(rel.waterlijn$z) #assume that waterlijn is equal across distance
    rel.oever$z <- rel.oever$z + scale_distance 
    
    ###create a xyz for slootbodem/bagger----
    slootbodem <- copy(rel.oever) |> as.data.table()
    slootbodem <- slootbodem[!is.na(opmerking) & opmerking != ""] #only numbers written when bagger is present, i.e. within water  
    slootbodem[opmerking == "waterlijn", opmerking := 0] #assume 0cm bagger at start waterlijn
    slootbodem[, opmerking := as.numeric(opmerking)] #start baggerdikte to numeric
    slootbodem[, begin_bagger := 0 - opmerking / 100] #depth at start bagger
    
    #calculate baggerdikte
    slootbodem[, baggerdikte := abs(z - begin_bagger)]
    
    #create a line for maximale baggerdikte
    max_baggerdikte <- slootbodem[baggerdikte == max(baggerdikte)]
    max_baggerdikte <- max_baggerdikte[, c("distance", "z", "begin_bagger", "baggerdikte")]
    max_baggerdikte <- data.table(distance = max_baggerdikte$distance,
               z = c(max_baggerdikte$z, max_baggerdikte$begin_bagger),
               baggerdikte = max_baggerdikte$baggerdikte)
    
    #print
    print(paste0(uniques[i], ": max baggerdikte = ", (unique(max_baggerdikte$baggerdikte) * 100 |> round(0)), "cm"))
      
    ####create a xyz for waterlijn
    waterlijn <- copy(rel.oever) |> as.data.table()
    waterlijn.pntnr <- waterlijn[opmerking == "waterlijn" | opmerking != ""]$puntnummer
    waterlijn.pntnr <- seq(min(waterlijn.pntnr), max(waterlijn.pntnr), 1)
    waterlijn <- waterlijn[puntnummer %in% waterlijn.pntnr]
    waterlijn[, z := mean(waterlijn[opmerking == "waterlijn"]$z)]
    
    ###centre the distances to the midpoint of the waterlijn----
    midpoint_waterlijn <- waterlijn$distance |> mean()
    rel.oever$distance <- rel.oever$distance - midpoint_waterlijn
    slootbodem$distance <- slootbodem$distance - midpoint_waterlijn
    waterlijn$distance <- waterlijn$distance - midpoint_waterlijn
    max_baggerdikte$distance <- max_baggerdikte$distance - midpoint_waterlijn
    
    #slootbodem can never be higher than waterlijn
    setDT(rel.oever)
    rel.oever[puntnummer %in% waterlijn.pntnr & z > 0, z := 0]
    
    ###create xyz for the location of the insteek (manual assignment based on point density)----
    if(uniques[i] == "oever1"){
      insteek <- rel.oever[puntnummer %in% c(5, 24)]
    } else if(uniques[i] == "oever2"){
      insteek <- rel.oever[puntnummer %in% c(6, 29)]
    } else if(uniques[i] == "oever3"){
      insteek <- rel.oever[puntnummer %in% c(4, 27)]
    } else if(uniques[i] == "oever4"){
      insteek <- rel.oever[puntnummer %in% c(4, 23)]
    } else if(uniques[i] == "oever5"){
      insteek <- rel.oever[puntnummer %in% c(5, 23)]
    } else if(uniques[i] == "oever6"){
      insteek <- rel.oever[puntnummer %in% c(4, 21)]
    } else if(uniques[i] == "oever7"){
      insteek <- rel.oever[puntnummer %in% c(5, 30)]
    } else if(uniques[i] == "oever8"){
      insteek <- rel.oever[puntnummer %in% c(4, 23)]
    }
    insteek <- st_as_sf(insteek)
    
    ###create a label for the cardinal direction----
    #add the cardinal direction
    puntnr_text_cardinal <- max(insteek$puntnummer)
    punt_text_cardinal <- rel.oever[puntnummer == puntnr_text_cardinal, c("distance", "z")] |> as.data.table()
    
      #add the label of cardinal direction and adjust z value for plotting
    punt_text_cardinal[, direction := cardinal_direction]
    punt_text_cardinal[, distance := distance + 0.3]
    punt_text_cardinal[, z := z + 0.2]
    
    ###calculate widths of oevers and waterlijn----
    waterlijn.start <- subset(waterlijn, waterlijn$puntnummer == min(waterlijn$puntnummer)) |> st_as_sf()
    waterlijn.end <- subset(waterlijn, waterlijn$puntnummer == max(waterlijn$puntnummer)) |> st_as_sf()
    waterlijn.width <- st_distance(waterlijn.start, waterlijn.end) |> as.numeric() |> round(1)
    
    oever1 <- rel.oever[puntnummer >= min(insteek$puntnummer) &
                          puntnummer < min(waterlijn.pntnr)]
    oever1.start <- oever1[puntnummer == min(puntnummer)] |> st_as_sf()
    oever1.end <- oever1[puntnummer == max(puntnummer)] |> st_as_sf()
    oever1.width <- st_distance(oever1.start, oever1.end) |> as.numeric() |> round(1)
    
    oever2 <- rel.oever[puntnummer <= max(insteek$puntnummer) &
                          puntnummer > max(waterlijn.pntnr)]
    oever2.start <- oever2[puntnummer == min(puntnummer)] |> st_as_sf()
    oever2.end <- oever2[puntnummer == max(puntnummer)] |> st_as_sf()
    oever2.width <- st_distance(oever2.start, oever2.end) |> as.numeric() |> round(1)
    
    widths <- data.table(locatie = c("waterlijn", "oever1", "oever2"),
                         width = c(waterlijn.width,
                                   oever1.width,
                                   oever2.width))
    
    #assign distances and z values  for annotate
    widths[locatie == "waterlijn", z := 0.3]
    widths[locatie == "waterlijn", distance := 0]
    widths[locatie == "oever1", distance :=  oever1[(nrow(oever1)/2) |> round(0),]$distance]
    widths[locatie == "oever1", z :=  oever1[(nrow(oever1)/2) |> round(0),]$z + 0.3]
    widths[locatie == "oever2", distance :=  oever2[(nrow(oever2)/2) |> round(0),]$distance]
    widths[locatie == "oever2", z :=  oever2[(nrow(oever2)/2) |> round(0),]$z + 0.3]
    
    #assign a label
    widths[, label := paste0(width, "m")]
        
    ###tidying of columns, tidy outliers----
    waterlijn <- waterlijn[, c("distance", "z")]
    rel.oever <- rel.oever[, c("distance", "z")]
    slootbodem <- slootbodem[, c("distance", "begin_bagger")]
    
    #for oever 6, omit an outlier
    if(uniques[i] == "oever6"){
      rel.oever <- rel.oever[z > -0.9]
    }
    
    #for oever 7, add a missing 0-0 to complete graph
    if(uniques[i] == "oever7"){
      slootbodem <- rbind(slootbodem, data.table(distance = -2,
                                         begin_bagger = 0))
      waterlijn <- rbind(waterlijn, data.table(distance = -2, 
                                               z = 0))
    }
    
    #select only data which is within 5 metres of the midpoint of waterlijn (standardised)
    rel.oever <- rel.oever[distance >= -5 & distance <= 5]
    
    ###retrieve data on onderholling----
    oh <- copy(onderholling[oever == uniques[i]])
    
    #assume that onderholling starts at 0.3m below water level
    oh[, z := -0.3]
    
    #find the distance of the slootbodem at the start of the waterline
    rel.slootbodem <- slootbodem[distance < 0]
    rel.slootbodem <- dplyr::arrange(rel.slootbodem,rel.slootbodem$distance)
    rel.slootbodem <- rel.slootbodem[1:2,]
    rel.slootbodem.lm <- lm(distance ~ begin_bagger, data = rel.slootbodem)
    rel.distance <- predict(rel.slootbodem.lm, data.table(begin_bagger = -0.3))
    
      #assign
    oh[, distance := rel.distance]
    oh <- oh[, c("distance", "onderholling", "z")]
    oh <- rbind(oh, oh)
    oh[1, distance := distance - onderholling]
    
    ###visualise and save----
    plot1 <- ggplot(rel.oever, aes(x = distance, y = z)) +
      geom_line() +
      geom_line(data = waterlijn, aes(x = distance, y = z)) +
      geom_line(data = slootbodem, aes(x = distance, y = begin_bagger)) +
      geom_line(data = oh, aes(x = distance, y = z), col = 'red', linetype = 'dashed') +
      geom_point(data = insteek, aes(x = distance, y = z), col = 'black') +
      geom_line(data = max_baggerdikte, aes(x = distance,  y = z),
                linetype = 'dashed', col = 'black') +
      scale_x_continuous(
        breaks = seq(-5, 5, 1),
        limits = c(-5, 5),
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        breaks = seq(-2, 0.8, 0.5),
        limits = c(-2, 0.8),
        expand = c(0, 0)
      ) +
      ggspatial::annotation_scale(
        style = "bar",
        location = "bl",
        plot_unit = "m"
      ) +
      coord_fixed() +
      theme_void() +
      annotate('text',
               x = -4,
               y = -0.3,
               label = punt_text_cardinal$direction) +
      annotate("segment", x = -4.8, y = -0.1, xend = -3, yend = -0.1, 
               linewidth = 1,
               arrow = arrow(type = "open")) +
      geom_text(data = widths,
                aes(x = distance, y = z),
                label = widths$label)
    
    #save it
    #ggsave(plot = plot1, filename = paste0("output/plots/oevers/", uniques[i], ".png"),
    #   width = 20,
    #   height = 20,
    #   units = "cm",
    #   dpi = 300,
    #   bg = "white")
    
  }
}
plot_oeverprofielen()

##helper function for cardinal direction
get_cardinal_direction <- function(name_oever) {
  #copy the trasnect of the oever
  dt1 <- copy(subset(oevers, oevers$oever == name_oever)) |> as.data.table()
  
  #select first and last point
  first_point <- dt1[puntnummer == min(puntnummer)] |> st_as_sf() |> st_transform(4326) |> st_coordinates()
  last_point <- dt1[puntnummer == max(puntnummer)] |> st_as_sf() |> st_transform(4326) |> st_coordinates()

  #calculate the azimuth in degrees
  azimuth <- bearing(first_point, last_point)
  
  if (azimuth < 0) {
    azimuth <- azimuth + 360  # Ensure azimuth is positive
  }
  
  #recalculate to a direction
  if (azimuth >= 0 && azimuth < 22.5){
    transect_direction <- "Noord"
  }
  if (azimuth >= 22.5 && azimuth < 67.5){
    transect_direction <- "Noordoost"
  }
  if (azimuth >= 67.5 && azimuth < 112.5){
    transect_direction <- "Oost"
  }
  if (azimuth >= 112.5 && azimuth < 157.5){
    transect_direction <- "Zuidoost"
  }
  if (azimuth >= 157.5 && azimuth < 202.5){
    transect_direction <- "Zuid"
  }
  if (azimuth >= 202.5 && azimuth < 247.5){
    transect_direction <- "Zuidwest"
  }
  if (azimuth >= 247.5 && azimuth < 292.5){
    transect_direction <- "West"
  }
  if (azimuth >= 292.5 && azimuth < 337.5){
    transect_direction <- "Northwest"
  }
  
  #return
  return(transect_direction)
}
