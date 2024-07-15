# Import funs
# load rtkgps data dxf format -----------------------------------------------------
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
# load gps in csv format ----------------------------------------------------------
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
# load gps profielen     ----------------------------------------------------------
importGPSprof <- function(inputdir = paste0(workspace,"./GPS slootprofielen"), gpsid = 36){
  # load all results from aquo kit from input dir
  prof <- list.files(path= paste0(inputdir), pattern=".csv", full.names =  T)
  names <- gsub(paste0(inputdir,'/'), "", prof)
  names <- gsub(paste0('.csv'), "", names)
  dt.names <- data.table(ID = 1:length(names), name = names)
  prof <- lapply( prof, fread, stringsAsFactors = FALSE)
  prof <- rbindlist( prof, fill =T, use.names = T, idcol = "ID")
  prof <- merge(dt.names,  prof , by = "ID")
  #extract label waterlijn
  prof[grepl('waterlijn', Opmerking), wl:= z]
  prof[, wl_min := min(wl, na.rm = TRUE), by = "ID"]
  prof[, wl := mean(wl, na.rm = TRUE), by = "ID"]
  prof[grepl('waterlijn', Opmerking), numwl := Puntnummer]
  prof[, numwl_min := min(numwl, na.rm = TRUE), by = "ID"]
  prof[, numwl_max := max(numwl, na.rm = TRUE), by = "ID"]
  prof[Puntnummer > numwl_max | Puntnummer < numwl_min, wl := NA]
  #extract slibdikte
  prof[, slib := as.numeric(Opmerking)/100]
  prof[is.na(slib), slib:= 0]
  
  prof <- st_as_sf(prof, coords = c('x','y','z'), crs = 28992, remove = FALSE)
  return(prof)
}
# Import penetrometerdata in txt format -------------------------------------------
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

# Visualise profielen-------------------------------------------------------------

visualise_profiel<- function(proftest){
  setDT(proftest)
  zmin <- min(proftest$z-proftest$slib)
  
  maxdist_x <- ceiling(max(proftest$dist))
  maxdist_y <- ceiling(max(proftest$z) - min(proftest$z))
  maxdist <- max(maxdist_x,maxdist_y)
  
  ggplot() +
    geom_line(data = proftest, aes(x = dist, y = z)) + 
    geom_ribbon(data=proftest, aes(x= dist,ymin=zmin, ymax=z-slib), fill = 'lightgreen', alpha =0.5)+
    geom_line(data = proftest, aes(x = dist, y = z-slib)) + 
    geom_ribbon(data=proftest, aes(x= dist,ymin=z-slib, ymax=z), fill = 'brown', alpha =0.5)+
    geom_line(data = proftest, aes(x = dist, y = wl)) + 
    geom_ribbon(data=proftest, aes(x= dist,ymin=z, ymax=wl), fill = 'lightblue', alpha =0.5)+
    coord_fixed(ratio =1)+
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
    ggtitle(paste0("Dwarsprofiel perceel en sloot op locatie ",proftest$name)) +
    labs(x= "afstand in meters",y="diepte in mNAP")
  ggsave(file=paste0('output/profielen/',proftest$name,"_",proftest$ID,'.png'),width = 40,height = 15,units='cm',dpi=1000)
  
}


