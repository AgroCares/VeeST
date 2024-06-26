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

# load gps in csv format
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
  
  # names header differ, force one unique name
  dt.names <- data.table(ID = 1:length(names), name = names)
  penheader <- data.table(ID = penheader$ID, header = rep(penheader$header[penheader$ID == 1], uniqueN(penheader$ID)), metadata = penheader$metadata)
  penheader <- dcast(penheader, ID ~ header, value.var = 'metadata')
  penheader <- merge(penheader, dt.names, by = 'ID')
  # import data
  pen <- lapply(pen, fread, sep=';', skip = 14)
  pen <- rbindlist(pen, fill =F, use.names = F, idcol = "ID")
  pen <- pen[, c("Depth", "Pen01" ,"Pen02" ,"Pen03" ,"Pen04", "Pen05", "Pen06", "Pen07", "Pen08", "Pen09" ,"Pen10") := tstrsplit(`Depth	Pen01	Pen02	Pen03	Pen04	Pen05	Pen06	Pen07	Pen08	Pen09	Pen10`, " ", fixed=TRUE)]
  
  # merge with metadata
  pen <- merge(pen, penheader, by = 'ID')
  pen <- pen[,-c('Depth	Pen01	Pen02	Pen03	Pen04	Pen05	Pen06	Pen07	Pen08	Pen09	Pen10')]
  
  # convert character 2 number
  changeCols <- colnames(pen[,2:12])
  pen <- pen[,(changeCols):= lapply(.SD, as.numeric), .SDcols = changeCols]
  
  # change format
  pen <- melt.data.table(pen, id.vars = c('ID','Plotname','name','Depth','Cone type','Plotdate','Penetrationspeed'), 
                          measure.vars = c( "Pen01" ,"Pen02" ,"Pen03" ,"Pen04", "Pen05", "Pen06", "Pen07", "Pen08", "Pen09" ,"Pen10"),
                          variable.name = 'Pen', value.name = 'indringingsweerstand')
  
  return(pen)
}