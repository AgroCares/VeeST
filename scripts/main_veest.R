
# Load packages -----------------------------------------------------------
library(data.table)
library(sf)
library(R.utils)
library(dplyr)

# Settings ----------------------------------------------------------------
workspace <- paste0(Sys.getenv("NMI-SITE"), 'O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/')

# Load custom functions-----------------------------------------------------
source(paste0("scripts/functions/functions_veest.R"))

# import gps dxf files------------------------------------------------------
gps <-  importGPS(inputdir = paste0(workspace,"./GPS"))
gps <- st_collection_extract(gps, type = c("POINT"))

# proces dxf files ---------------------------------------------------------
# filter alleen de coordinaten van penetrometer eruit
setDT(gps)
gps <- gps[Layer == 'apglos_atts' & !is.na(Text) & !Text == "" & grepl('pen', Text),]
gps<- gps[,Text := as.character(Text)]
# remove empty columns
gps <- gps[,-c('PaperSpace','SubClasses','Linetype')]
# filter dubbele coördinaten in Zegveld eruit
gps <- setorder(gps, ID)
gps <- gps[!(name %in% c('ZG3c','ZG3b','ZG3a') & EntityHandle %in% unique(gps$EntityHandle[name == 'ZG3d'])),]
gps <- gps[!(name %in% c('ZG3b','ZG3a') & EntityHandle %in% unique(gps$EntityHandle[name == 'ZG3c'])),]
gps <- gps[!(name %in% c('ZG3b') & EntityHandle %in% unique(gps$EntityHandle[name == 'ZG3a'])),]
gps <- gps[!(name %in% c('ZG2a') & EntityHandle %in% unique(gps$EntityHandle[name == 'ZG1b'])),]
gps <- gps[!(name %in% c('ZG1b') & EntityHandle %in% unique(gps$EntityHandle[name == 'ZG1a'])),]

# rondehoep sloot 1 tm 4, 7, 8, 10, zegveld 1 en 2, spaarnwoude is tweezijdig of twee transecten
setDT(gps)
gps[,trajecten := 2]
gps[,Opmerking := Text]
# voeg volgnummer toe voor bepaling afstand tot sloot
# 1 = perceel, 2 = insteek, 3 tm 12 = oever en insteek en perceel andere oeverzijde
gps <- gps[,dist_id := frank(EntityHandle, ties.method = 'dense'), by = 'ID']

# import gps csv data
gps2 <-  importGPS2(inputdir = paste0(workspace,"./GPS"), gpsid = max(gps$ID))
setDT(gps2)
gps2[,trajecten := 1]
# voeg volgnummer toe voor bepaling afstand tot sloot
# 1 = perceel, 2 = insteek, 3 tm 12 = oever en insteek en perceel andere oeverzijde
gps2 <- gps2[,dist_id := frank(Puntnummer, ties.method = 'dense'), by = 'ID']

# merge csv and dxf
gps <- gps[,c('ID','name','Opmerking','trajecten','dist_id','geometry')]
gps2 <- gps2[,c('ID','name','Opmerking','trajecten','dist_id','geometry')]
gps <- rbind(gps,gps2)

# add location info for filtering and correcting
gps[, gebied:= tstrsplit(name, "_")[1]]
gps[, sloot:= tstrsplit(name, "_")[2]]
gps[ID %in% 29:36, sloot:= sub(".*?(\\d+).*", "\\1", name)]
gps[ID %in% 29:36, gebied:= tstrsplit(name, sloot)[1]]
# onderstaande sloten en opnamen wel eenzijdig (opvallend dat bij sommige sloten soms een, soms twee zijden penetrometer is gedaan)
gps[gebied == 'Rh' & sloot %in% c('1','5','9','10')| name %in% c('Rh_7_M_Af_Kr', 'Rh_4_M_Af','Sw_1_M_Af', 'Sw_3_M')| gebied == "Kw" , trajecten := 1]

# correct dist id for multiple transects (2 oevers or 2 transects)
gps[,max_dis_id := max(dist_id), by = 'name']
gps[trajecten == 2 & dist_id > (floor(max_dis_id/2)), dist_id := dist_id-floor(max_dis_id/2)]

# let op coördinaten Krimpenerwaard missen: moeten uit word bestand worden gehaald en dist id moet handmatig worden toegevoegd
# vragen aan Harm of hij id perceel, insteek en oever en oeverzijde controleert/toevoegt

# make unique name 4 matching
gps[,Opmerking := tolower(Opmerking)]
gps[,Opmerking := lapply(.SD, function(x) gsub("\\s+|\\s+", "",x)),.SDcols = c('Opmerking')]
gps[,plot := gsub('insteek','', Opmerking)]
gps[,plot := gsub('slootafgegraven','', plot)]
gps[,plot := gsub("[<>+]", "", plot)]
gps[,plot := gsub("^p", "plot", plot)]
gps[,plot := gsub("plotlot", "plot", plot)]
gps[,gebied:= tolower(gebied)]
gps[,oever:= tstrsplit(name, "_")[4]]
gps[gebied == 'rh' & sloot %in% c(6,7,8,9,10), extragebied:= 'sloot6tm10']
gps[gebied == 'rh' & sloot %in% c(1,2,3,4,5), extragebied:= 'sloot1tm5']
gps[gebied == 'rh' & sloot %in% c(11), extragebied:= 'sloot11']


# Import penetrometer data ----------------------------------------------
pen <- importPen(inputdir = paste0(workspace,"./Penetrometer"))
# make unique name 4 matching
pen[ ,plot:= as.numeric(gsub("^PLOTX*", "", Plotname))]
pen[ ,pen := as.numeric(gsub("^Pen*", "", as.character(Pen)))]
pen[ ,plot:= paste0('plot',plot,'pen',pen)]
pen[ ,gebied:= tstrsplit(name, "_")[1]]
pen[ ,gebied:=tolower(gebied)]
pen[ ,extragebied:= tstrsplit(name, "_")[3]]
pen[ ,extragebied:= gsub('[<>.]','',extragebied)]

# hoe weet ik wat rondehoep sloot 1noord en 1 zuid is in gps data?

# Import abiotiek ---------------------------------------------------------
# Import planing/behandelingen --------------------------------------------
# Import slootprofielen ---------------------------------------------------
# Import waterbodemdata ---------------------------------------------------
# Import bodemgegevens oever ----------------------------------------------

# Validatie --------------------------------------------------

# 1- validatie regels coördinaten
# check 4 double coordinates
gps <- st_as_sf(gps)
gps2 <- st_zm(gps, crs = 28992)
coords <- as.data.table(st_coordinates(gps2))
dubbel <- gps[which(duplicated(coords)),]
# check of lijnen veldform abiotiek kruisen met penetrometer
# check of lijnen veldform hetzelfde zijn als planningtabel
# visualisatie behandelingen op kaart

# validatieregels locatie(codes) en relatie met beheer/ behandelingen
# check of beheerinvoer odk form == beheer planningstabel


# Preprocessing/ aggregatie ---------------------------------------------
# Preprocess penetrometer data 

# merge gps with pen
penmerge <- merge(pen, gps, by=c('gebied','plot','extragebied'), all.x = TRUE, all.y=FALSE, allow.cartesian = TRUE)
#als ID.y = na : wel penetrometer, geen matching gps
# sorteren op ID.x
penmergecheck <- unique(penmerge[,c('name.y','ID.x','plot','gebied','sloot','extragebied','trajecten','dist_id')])
write.table(penmergecheck, file = paste(workspace,"/penetrometer_gps_check",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
gps <- st_as_sf(gps)
st_write(gps, paste0(workspace,  "gps_penetrometer.gpkg"))

# soms meerdere gps coordinaten bij 1 gebied, plot, extragebied 
# merge met abiotiek kan op oever holling na
# merge met vegetatie hoe? dan moet ik ook oeverzijde hebben

# Visualisaties ---------------------------------------------------------

  
# Export the data ---------------------------------------------------------
saveRDS(report, paste0(workspace, 'products/', snapshot.version, '/report.rds'))

