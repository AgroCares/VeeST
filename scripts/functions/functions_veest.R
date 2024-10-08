# Import funs ------------------------------------
## load rtkgps data dxf format -----------------------------------------------------
importGPS <- function(inputdir = paste0(workspace,"./GPS")){
  # load all results from aquo kit from input dir
  gps <- list.files(path= paste0(inputdir), pattern=".dxf", full.names =  T)
  names <- gsub(paste0(inputdir,'/'), "", gps)
  names <- gsub(paste0('.dxf'), "", names)
  dt.names <- data.table(ID = 1:length(names), name = names)
  gps <- lapply(gps, st_read, as_tibble = FALSE, stringsAsFactors = TRUE)
  gps <- rbindlist(gps, fill =T, use.names = T, idcol = "ID")
  gps <- merge(dt.names, gps , by = "ID")
  gps <- st_as_sf(gps, crs = 28992)
  gps <- st_zm(gps, crs = 28992)
  return(gps)
}
## load gps in csv format ----------------------------------------------------------
importGPS2 <- function(inputdir = paste0(workspace,"./GPS"), gpsid = 36){
  # load all results from aquo kit from input dir
  gps2 <- list.files(path= paste0(inputdir), pattern=".csv", full.names =  T)
  names <- gsub(paste0(inputdir,'/'), "", gps2)
  names <- gsub(paste0('.csv'), "", names)
  dt.names <- data.table(ID = 1:length(names), name = names)
  gps2 <- lapply(gps2, fread, stringsAsFactors = FALSE)
  gps2 <- rbindlist(gps2, fill =T, use.names = T, idcol = "ID")
  gps2 <- merge(dt.names, gps2 , by = "ID")
  gps2[,ID := ID+gpsid]
  gps2 <- st_as_sf(gps2, coords = c('x','y','z'), crs = 28992)
  return(gps2)
}
## load gps profielen     ----------------------------------------------------------
importGPSprof <- function(inputdir = paste0(workspace,"./GPS slootprofielen"), gpsid = 36){
  # load all results from aquo kit from input dir
  prof <- list.files(path= paste0(inputdir), pattern=".csv", full.names =  T)
  names <- gsub(paste0(inputdir,'/'), "", prof)
  names <- gsub(paste0('.csv'), "", names)
  dt.names <- data.table(ID = 1:length(names), name = names)
  prof <- lapply( prof, fread, stringsAsFactors = FALSE)
  prof <- rbindlist( prof, fill =T, use.names = T, idcol = "ID")
  prof <- merge(dt.names,  prof , by = "ID")
  prof <- st_as_sf(prof, coords = c('x','y','z'), crs = 28992, remove = FALSE)
  return(prof)
}
## Import penetrometerdata in txt format -------------------------------------------
importPen <- function(inputdir = paste0(workspace,"./Penetrometer")){
  
  pen <- list.files(path= paste0(inputdir), pattern=".TXT", full.names =  T)
  
  # incluse filesname
  names <- gsub(paste0(inputdir,'/'), "", pen)
  names <- gsub(paste0('.TXT'), "", names)
  dt.names <- data.table(ID = 1:length(names), name = names)
  # include metadata
  penheader <- lapply(pen, fread, sep=';', nrows = 13)
  penheader <- rbindlist(penheader, fill =F, use.names = F, idcol = "ID")
  penheader <- penheader[,metadata:= sapply(strsplit(`INSTRUMENT EIJKELKAMP PENETROLOGGER SN       0`,':'), `[`,2)]
  penheader <- penheader[,header:= sapply(strsplit(`INSTRUMENT EIJKELKAMP PENETROLOGGER SN       0`,':'), `[`,1)]
  penheader <- penheader[!is.na(metadata),]
  # trim spaces
  penheader <- penheader[,metadata := gsub(' $','',metadata)]
  penheader <- penheader[,header := gsub(' $','',header)]
  # trim character columns from starting and ending space
  cols <- colnames(penheader)[sapply(penheader, is.character)] # which colnames are character
  penheader <- penheader[,(cols) := lapply(.SD, function(x) gsub("^\\s+|\\s+$", "", x)),.SDcols = cols]
  penheader <- data.table(ID = penheader$ID, header = rep(penheader$header[penheader$ID == 1], uniqueN(penheader$ID)), metadata = penheader$metadata)
  penheader <- dcast(penheader, ID ~ header, value.var = 'metadata')
  penheader <- merge(penheader, dt.names, by = 'ID')
  # import data
  pen <- lapply(pen, fread, sep=';', skip = 14)
  pen <- rbindlist(pen, fill =F, use.names = F, idcol = "ID")
  pen <- pen[, c("Diept", "Pen01" ,"Pen02" ,"Pen03" ,"Pen04", "Pen05", "Pen06", "Pen07", "Pen08", "Pen09" ,"Pen10") := tstrsplit(`Diept	Pen01	Pen02	Pen03	Pen04	Pen05	Pen06	Pen07	Pen08	Pen09	Pen10`, " ", fixed=TRUE)]
  
  # merge with metadata
  pen <- merge(pen, penheader, by = 'ID')
  pen <- pen[,-c('Diept	Pen01	Pen02	Pen03	Pen04	Pen05	Pen06	Pen07	Pen08	Pen09	Pen10')]
  
  # convert character 2 number
  changeCols <- colnames(pen[,2:12])
  pen <- pen[,(changeCols):= lapply(.SD, as.numeric), .SDcols = changeCols]
  
  # change format
  pen <- melt.data.table(pen, id.vars = c('ID','Plotnaam','name','Diept','Conustype','Plotdatum','Penetratie snelheid'), 
                          measure.vars = c( "Pen01" ,"Pen02" ,"Pen03" ,"Pen04", "Pen05", "Pen06", "Pen07", "Pen08", "Pen09" ,"Pen10"),
                          variable.name = 'Pen', value.name = 'indringingsweerstand')
  
  return(pen)
}

# Proces funs-----------------------
# get wind direction
get_cardinal_direction <- function(profiel_nr) {
  #copy the transect of the oever
  dt1 <- setDT(profiel_nr)
  
  #select first and last point
  first_point <- dt1[Puntnummer == min(Puntnummer)] |> st_as_sf() |> st_transform(4326) 
  last_point <- dt1[Puntnummer == max(Puntnummer)] |> st_as_sf() |> st_transform(4326)
  
  #calculate the azimuth in degrees
  azimuth <- st_azimuth(first_point, last_point)
  
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
# calculate different slopes
calc_taludhoek <- function(profiel_nr){
  #copy the trasnect of the oever
  dt1 <- setDT(profiel_nr)
  
  #select first and last point first shore
  min_dist <- min(dt1[Opmerking == "waterlijn",'dist']) - 3
  first_point <- dt1[sectie == 'oever' & dist > min_dist] 
  first_point <- first_point[Puntnummer == min(Puntnummer)]
  last_point <- dt1[sectie == 'water']
  last_point <- last_point[Puntnummer == min(Puntnummer)]
  # calc angle
  tldk_bvwtr_perc_1 <-  100*(first_point$z-last_point$z) / (last_point$dist-first_point$dist)
  tldk_bvwtr_graden_1 <-  atan((first_point$z-last_point$z) / (last_point$dist-first_point$dist))*(180/pi)
  
  #select first and last point second shore
  max_dist <- max(dt1[Opmerking == "waterlijn",'dist']) + 3
  first_point <- dt1[sectie == 'oever' & dist < max_dist]
  first_point <- first_point[Puntnummer == max(Puntnummer)]  
  last_point <- dt1[sectie == 'water']
  last_point <- last_point[Puntnummer == max(Puntnummer)] 
  # calc angle
  tldk_bvwtr_perc_2 <-  100*(first_point$z-last_point$z) / (first_point$dist-last_point$dist)
  tldk_bvwtr_graden_2 <-  atan((first_point$z-last_point$z) / (first_point$dist-last_point$dist))*(180/pi)
  
  #select first and last point first shoreline
  first_point <- dt1[sectie == 'water'] 
  first_point <- first_point[Puntnummer == min(Puntnummer)] 
  last_point <- dt1[sectie == 'water' & dist - first_point$dist < 0.5]
  # selecteer een locatie minder dan 50 cm verder dan de waterlijn
  last_point <- last_point[Puntnummer == max(Puntnummer)] 
  # calc angle bovenkant slib
  tldk_ondwtr_perc_1 <-  100*(first_point$z-last_point$z) / (last_point$dist-first_point$dist)
  tldk_ondwtr_graden_1 <-  atan((first_point$z-last_point$z) / (last_point$dist-first_point$dist))*(180/pi) 
  # calc angle onderkant slib
  tldk_vastbodem_perc_1 <-  100*((first_point$z-first_point$slib) -(last_point$z-last_point$slib)) / (last_point$dist - first_point$dist)
  tldk_vastbodem_graden_1 <-  atan(((first_point$z-first_point$slib) -(last_point$z-last_point$slib)) / (last_point$dist-first_point$dist))*(180/pi) 
  
  #select first and last point second shoreline
  first_point <- dt1[sectie == 'water'] 
  first_point <- first_point[Puntnummer == max(Puntnummer)]  
  last_point <- dt1[sectie == 'water' & first_point$dist - dist < 0.5]
  # selecteer een locatie minder dan 50 cm verder dan de waterlijn
  last_point <- last_point[Puntnummer == min(Puntnummer)] 
    # calc angle
  tldk_ondwtr_perc_2 <-  100*(first_point$z-last_point$z) / (first_point$dist-last_point$dist)
  tldk_ondwtr_graden_2 <-  atan((first_point$z-last_point$z) / (first_point$dist-last_point$dist))*(180/pi) 
  # calc angle onderkant slib
  tldk_vastbodem_perc_2 <-  100*((first_point$z-first_point$slib) -(last_point$z-last_point$slib)) / (first_point$dist-last_point$dis)
  tldk_vastbodem_graden_2 <-  atan(((first_point$z-first_point$slib) -(last_point$z-last_point$slib)) / (first_point$dist-last_point$dis))*(180/pi) 
  
  #select first and last point watersurface
  first_point <- dt1[sectie == 'water'] 
  first_point <- dt1[Puntnummer == min(Puntnummer)] 
  last_point <- dt1[z < first_point$z -0.10 & sectie == 'water']
  first_point <- dt1[z < first_point$z +0.10 & sectie == 'oever']
  
  first_point_2 <- last_point[Puntnummer == max(Puntnummer)]#water
  last_point_2 <- first_point[Puntnummer == max(Puntnummer)]#oever
  first_point <- first_point[Puntnummer == min(Puntnummer)]#oever
  last_point <- last_point[Puntnummer == min(Puntnummer)]#water
  # calc angle
  tldk_wtr_perc_1 <-  100*(first_point$z-last_point$z) / (last_point$dist-first_point$dist)
  tldk_wtr_graden_1 <-  atan((first_point$z-last_point$z) / (last_point$dist-first_point$dist))*(180/pi) 
  # calc second shore
  tldk_wtr_perc_2 <-  100*(first_point_2$z-last_point_2$z) / (first_point_2$dist-last_point_2$dist)
  tldk_wtr_graden_2 <-  atan((first_point_2$z-last_point_2$z) / (first_point_2$dist-last_point_2$dist))*(180/pi)
  
  talud <- data.table(tldk_bvwtr_perc_1,tldk_bvwtr_graden_1,tldk_bvwtr_perc_2,tldk_bvwtr_graden_2,
             tldk_ondwtr_perc_1,tldk_ondwtr_graden_1,tldk_ondwtr_perc_2,tldk_ondwtr_graden_2,
             tldk_wtr_perc_1,tldk_wtr_graden_1,tldk_wtr_perc_2 ,tldk_wtr_graden_2,  
             tldk_vastbodem_perc_1,tldk_vastbodem_graden_1,tldk_vastbodem_perc_2,tldk_vastbodem_graden_2,
             keep.rownames=T)
  
  return(talud)

}


  
    
# Visualise profielen-------------------------------------------------------------
visualise_profiel<- function(proftest){
  proftest <- profiel[ID == i,]
  setDT(proftest)
  proftest <- proftest[sectie %in% c('oever','water')]
  
  zmin <- min(proftest$z-proftest$slib)
  # maxdist_x <- ceiling(max(proftest$midpoint_dist[proftest$sectie == 'oever']))
  # maxdist_y <- ceiling(max(proftest$z) - min(proftest$z))
  # maxdist <- max(maxdist_x,maxdist_y)
  
  ggplot() +
    geom_line(data = proftest, aes(x = midpoint_dist, y = z)) +
    geom_ribbon(data=proftest, aes(x= midpoint_dist,ymin=zmin, ymax=z-slib), fill = 'lightgreen', alpha =0.5)+
    geom_line(data = proftest, aes(x = midpoint_dist, y = z-slib)) +
    geom_ribbon(data=proftest, aes(x= midpoint_dist,ymin=z-slib, ymax=z), fill = 'brown', alpha =0.5)+
    geom_line(data = proftest[!is.na(wl),], aes(x = midpoint_dist, y = wl)) + 
    geom_ribbon(data=proftest[!is.na(wl),], aes(x= midpoint_dist, ymin=z, ymax=wl), fill = 'lightblue', alpha =0.5)+
    coord_fixed(ratio = 1)+
    # annotate('text', x = -4, y = -0.3,label = proftest$azimuth) +
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size=10), 
      strip.text.y = element_text(size = 10), 
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size= 10),
      axis.ticks =  element_line(colour = "black"),
      plot.title = element_text(size =12, face="bold", hjust = 0.5),
      panel.background = element_blank(),
      plot.background = element_blank(),
    )+
    ggtitle(paste0("Dwarsprofiel oever en sloot op locatie ",proftest$name)) +
    labs(x= "afstand in meters",y="diepte in mNAP")
  ggsave(file=paste0('output/profielen/',proftest$name,"_",proftest$ID,'.png'),width = 40,height = 15,units='cm',dpi=1000)
  
}


