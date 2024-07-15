
# Load packages -----------------------------------------------------------
library(data.table)
library(sf)
library(R.utils)
library(dplyr)
library(ggplot2)

# Settings ----------------------------------------------------------------
workspace <- paste0(Sys.getenv("NMI-SITE"), 'O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/')

# Load custom functions-----------------------------------------------------
source(paste0("scripts/functions/functions_veest.R"))

# import -------------------------------------------------------------------
## import gps dxf files------------------------------------------------------
gps <-  importGPS(inputdir = paste0(workspace,"./GPS"))
gps <- st_collection_extract(gps, type = c("POINT"))

## proces dxf files ---------------------------------------------------------
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

## import gps csv data--------------------------------------------------------
gps2 <-  importGPS2(inputdir = paste0(workspace,"./GPS"), gpsid = max(gps$ID))
setDT(gps2)
gps2[,trajecten := 1]
# voeg volgnummer toe voor bepaling afstand tot sloot
# 1 = perceel, 2 = insteek, 3 tm 12 = oever en insteek en perceel andere oeverzijde
gps2 <- gps2[,dist_id := frank(Puntnummer, ties.method = 'dense'), by = 'ID']

## merge csv and dxf--------------------------------------------------------
gps <- gps[,c('ID','name','Opmerking','trajecten','dist_id','geometry')]
gps2 <- gps2[,c('ID','name','Opmerking','trajecten','dist_id','geometry')]
gps <- rbind(gps,gps2)
#create id for filtering
coords <- data.frame(st_coordinates(st_zm(gps$geometry)))
gps <- cbind(gps,coords)
gps[,ident:= paste0(X,'_',Y)]

# Process gps files
## add location info for filtering and correcting
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
# nakijken een of tweezijdig coordinaten in relatie tot penetrometerdata
# rondehoep sloot 1 is alleen gps data aan zuidzijde
# let op consistentie naamgeving penetrometerdata gebied_plot_slootX zonder spaties

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
gps[gebied == 'rh' & sloot %in% c(1,6,7,8,9,10), extragebied:= 'sloot6tm10'] #gps bevat alleen data van sloot 1 zuid, die hoort bij sloot6tm10
gps[gebied == 'rh' & sloot %in% c(2,3,4,5), extragebied:= 'sloot1tm5']
gps[gebied == 'rh' & sloot %in% c(11), extragebied:= 'sloot11']
gps[gebied == 'sw' & sloot %in% c(4), extragebied:= 'sloot4']
gps[gebied == 'sw' & sloot %in% c(5), extragebied:= 'sloot5']
gps[gebied == 'sw' & sloot %in% c(6), extragebied:= 'sloot6']
gps[grepl('WP1', name), extragebied:= 'wp1']

# correctie dist_id
gps[name == 'Rh_4_R' & Opmerking == 'plot6pen8', dist_id := 6]
gps[name == 'Rh_4_R' & Opmerking == 'plot6pen9', dist_id := 1]
gps[name == 'Rh_4_R' & Opmerking == 'plot6pen10', dist_id := 2]
gps[name == 'Rh_4_R' & grepl('plot7', Opmerking), dist_id := dist_id-1]
gps[name == 'Rh_11_M-AF_z' & grepl('plot1pen7', Opmerking), dist_id := 1]
gps[name == 'Rh_11_M-AF_z' & grepl('plot1pen8', Opmerking), dist_id := 2]
gps[name == 'Sw_3_M_Af' & grepl('plot5pen1', Opmerking), dist_id := 5]
gps[name == 'Sw_3_M_Af' & grepl('plot5pen2', Opmerking), dist_id := 1]
gps[name == 'Sw_3_M_Af' & grepl('plot5pen3', Opmerking), dist_id := 2]
gps[name == 'Sw_3_M_Af' & grepl('plot5pen4', Opmerking), dist_id := 3]
gps[name == 'Sw_3_M_Af' & grepl('plot5pen5', Opmerking), dist_id := 4]
gps[name == 'Sw_2_M' & grepl('plot3pen9', Opmerking), dist_id := 4]
gps[name == 'Sw_2_M' & grepl('plot3pen10', Opmerking), dist_id := 1]
gps[name == 'Sw_2_M' & ident == '108210.754_489348.47', dist_id := 2]
gps[name == 'Sw_2_M' & ident == '108210.754_489348.47', Opmerking := 'plot4pen1']
gps[name == 'Sw_2_M' & grepl('plot4pen2', Opmerking), dist_id := 3]
#ZG1b ZG1a
#ZG2b ZG2a

## Import penetrometer data ----------------------------------------------
pen <- importPen(inputdir = paste0(workspace,"./Penetrometer"))
pen <- pen[!is.na(indringingsweerstand),]
# make unique name 4 matching
pen[ ,plot:= as.numeric(gsub("^PLOTX*", "", Plotnaam))]
pen[ ,pen := as.numeric(gsub("^Pen*", "", as.character(Pen)))]
pen[ ,plot:= paste0('plot',plot,'pen',pen)]
pen[ ,gebied:= tstrsplit(name, "_")[1]]
pen[ ,gebied:=tolower(gebied)]
pen[ ,extragebied:= tstrsplit(name, "_")[3]]
pen[ ,extragebied:= gsub('[<>.]','',extragebied)]

## Import abiotiek ---------------------------------------------------------
## Import planing/behandelingen --------------------------------------------
## Import slootprofielen ---------------------------------------------------
profiel <-  importGPSprof(inputdir = paste0(workspace,"./GPS slootprofielen"))

## Import waterbodemdata ---------------------------------------------------
## Import bodemgegevens oever ----------------------------------------------

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


# Postprocessing/ aggregatie ---------------------------------------------

## Postprocess penetrometer data ---------------------------------------------

# merge gps with pen
penmerge <- merge(pen, gps, by=c('gebied','plot','extragebied'), all.x = TRUE, all.y=FALSE, allow.cartesian = TRUE)
# check double gps
checkgps <- gps[, nunique := uniqueN(ident), by = c('gebied','plot','extragebied')]
# als ID.y = na : wel penetrometer, geen matching gps
# sorteren op ID.x
penmergecheck <- unique(penmerge[,c('name.y','ID.x','plot','gebied','sloot','extragebied','trajecten','dist_id')])
write.table(penmergecheck, file = paste(workspace,"/penetrometer_gps_check",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
gps <- st_as_sf(gps)
st_write(gps, paste0(workspace,  "gps_penetrometer.gpkg"))
# soms meerdere gps coordinaten bij 1 gebied, plot, extragebied 
# merge met abiotiek aan een verkeerde zijde kan op oever holling na
# merge met vegetatie hoe? dan moet ik ook oeverzijde hebben


## Postprocess profielen-----------------------------------------------------
# Mijnden
# 1: 3-34, 2: 35-50, 3:69-82, 4: 51-68, 5: 83-98, 6: 99-122, 123:154, 155:183, 184:215, 216:246,280:314, 247:279,  315:350, 351:386,387:429, 430:471
setDT(profiel)
profiel[,ID := as.numeric(ID)]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 3 & Puntnummer <= 34, ID := paste0(ID,'.',1) ]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 35 & Puntnummer <= 50, ID := paste0(ID,'.',2) ]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 51 & Puntnummer <= 68, ID := paste0(ID,'.',3) ]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 69 & Puntnummer <= 82, ID := paste0(ID,'.',4) ]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 83 & Puntnummer <= 98, ID := paste0(ID,'.',5) ]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 99 & Puntnummer <= 122, ID := paste0(ID,'.',6) ]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 123 & Puntnummer <= 154, ID := paste0(ID,'.',7) ]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 155 & Puntnummer <= 183, ID := paste0(ID,'.',8) ]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 184 & Puntnummer <= 215, ID := paste0(ID,'.',9) ]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 216 & Puntnummer <= 246, ID := paste0(ID,'.',11) ]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 247 & Puntnummer <= 279, ID := paste0(ID,'.',12) ]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 280 & Puntnummer <= 314, ID := paste0(ID,'.',13) ]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 315 & Puntnummer <= 350, ID := paste0(ID,'.',14) ]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 351 & Puntnummer <= 386, ID := paste0(ID,'.',15) ]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 387 & Puntnummer <= 429, ID := paste0(ID,'.',16) ]
profiel[name == 'polder mijnden zuid' & Puntnummer >= 430 & Puntnummer <= 471, ID := paste0(ID,'.',17) ]

profiel[name == 'polder mijnden zuid' & Puntnummer == 3 , Puntnummer := 1 ]
profiel[name == 'polder mijnden zuid' & Puntnummer == 35 , Puntnummer := 1 ]
profiel[name == 'polder mijnden zuid' & Puntnummer == 51 , Puntnummer := 1 ]
profiel[name == 'polder mijnden zuid' & Puntnummer == 69 , Puntnummer := 1 ]
profiel[name == 'polder mijnden zuid' & Puntnummer == 83 , Puntnummer := 1 ]
profiel[name == 'polder mijnden zuid' & Puntnummer == 99 , Puntnummer := 1 ]
profiel[name == 'polder mijnden zuid' & Puntnummer == 123 , Puntnummer:= 1 ]
profiel[name == 'polder mijnden zuid' & Puntnummer == 155 , Puntnummer := 1 ]
profiel[name == 'polder mijnden zuid' & Puntnummer == 184 , Puntnummer := 1 ]
profiel[name == 'polder mijnden zuid' & Puntnummer == 216 , Puntnummer:= 1 ]
profiel[name == 'polder mijnden zuid' & Puntnummer == 247 , Puntnummer:= 1]
profiel[name == 'polder mijnden zuid' & Puntnummer == 280 , Puntnummer := 1]
profiel[name == 'polder mijnden zuid' & Puntnummer == 315 , Puntnummer:= 1 ]
profiel[name == 'polder mijnden zuid' & Puntnummer == 351 , Puntnummer:= 1 ]
profiel[name == 'polder mijnden zuid' & Puntnummer == 387 , Puntnummer:= 1 ]
profiel[name == 'polder mijnden zuid' & Puntnummer == 430 , Puntnummer:= 1]

# nog checken dit is afstand in meters toch?
profiel[, dist := sqrt((x[Puntnummer == 1]-x)^2+(y[Puntnummer == 1]-y)^2), by ='ID'] 

# Visualisaties ---------------------------------------------------------

# plot profiel
for(i in unique(profiel$ID)){
  setDT(profiel)
  visualise_profiel(profiel[ID == i,])
  print(i)
}

# test voor Mijnden
# install.packages("scatterplot3d") # Install
library("scatterplot3d") # load
proftest <- profiel[ID == 1,]
scatterplot3d(proftest$x,proftest$y, proftest$z)
plot(proftest$dist,proftest$z)  
  

# Export the data ---------------------------------------------------------
saveRDS(report, paste0(workspace, 'products/', snapshot.version, '/report.rds'))

