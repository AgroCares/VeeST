
# Load packages -----------------------------------------------------------
library(data.table)
library(sf)
library(R.utils)
library(dplyr)
library(ggplot2)
library(nngeo) #azimuth

# Settings ----------------------------------------------------------------
workspace <- paste0(Sys.getenv("NMI-SITE"), 'O 1900 - O 2000/1922.N.23 VeeST vwsloot vd toekomst/05. Data/')

# Load custom functions-----------------------------------------------------
source(paste0("scripts/functions/functions_veest.R"))

# Penetrometer----------------------------------------------
## import----------------------------------------------
### import gps dxf files------------------------------------------------------
gps <-  importGPS(inputdir = paste0(workspace,"./GPS"))
gps <- st_collection_extract(gps, type = c("POINT"))
### proces dxf files ---------------------------------------------------------
setDT(gps)
gps[,Text := tolower(Text)]
gps[,Puntnummer := rep(seq_len(.N), each = 15, length.out = .N)]
gps<- gps[,Text := as.character(Text)]
# remove empty columns
gps <- gps[,-c('PaperSpace','SubClasses','Linetype')]

# create gps table with missing penetrometer ref
gps1 <- gps
# filter gps with penetrometer ref
gps <- gps[Layer == 'apglos_atts' & !is.na(Text) & !Text == "" & grepl('p', Text),]
# create gps table with missing penetrometer ref
gps1 <- gps1[!(ID %in% unique(gps$ID) & Puntnummer %in% unique(gps$Puntnummer)),]
gps1 <- gps1[Layer == 'apglos_att_nr',]
# RH_1_noord zou moeten starten bij plot8pen2, maar deze is er niet in de set van die datum, dus deze ontbreekt
gps1[name == 'Rh_10_M_Af' & EntityHandle == 'AAA33', Opmerking := 'plot4pen8']
gps1[name == 'Rh_10_M_Af' & EntityHandle == 'AAA50', Opmerking := 'plot4pen9']
gps1[name == 'Rh_10_M_Af' & EntityHandle == 'AAA67', Opmerking := 'plot4pen10']
gps1[name == 'Rh_10_M_Af' & EntityHandle == 'AAA84', Opmerking := 'plot5pen1']
gps1[name == 'Rh_10_M' & EntityHandle == 'AAA33', Opmerking := 'plot5pen2']
gps1[name == 'Rh_10_M' & EntityHandle == 'AAA50', Opmerking := 'plot5pen3']
gps1[name == 'Rh_10_M' & EntityHandle == 'AAA67', Opmerking := 'plot5pen4']
gps1[name == 'Rh_10_M' & EntityHandle == 'AAA84', Opmerking := 'plot5pen5']
gps1[name == 'Rh_10_M' & EntityHandle == 'AAA101', Opmerking := 'plot5pen6']
gps1[name == 'Rh_10_R' & EntityHandle == 'AAA50', Opmerking := 'plot5pen7']
gps1[name == 'Rh_10_R' & EntityHandle == 'AAA67', Opmerking := 'plot5pen8']
gps1[name == 'Rh_10_R' & EntityHandle == 'AAA84', Opmerking := 'plot5pen9']
gps1[name == 'Rh_10_R' & EntityHandle == 'AAA101', Opmerking := 'plot5pen10']
gps1[name == 'Rh_7_M_Af_Kr' & EntityHandle == 'AAA33', Opmerking := 'plot9pen3']
gps1[name == 'Rh_7_M_Af_Kr' & EntityHandle == 'AAA50', Opmerking := 'plot9pen4']
gps1[name == 'Rh_7_M_Af_Kr' & EntityHandle == 'AAA67', Opmerking := 'plot9pen5']
gps1[name == 'Rh_7_M_Af_Kr' & EntityHandle == 'AAA84', Opmerking := 'plot9pen6']
gps1[name == 'Rh_7_M_Af_Kr' & EntityHandle == 'AAA101', Opmerking := 'plot9pen7']
gps1[name == 'Rh_4_M_Af' & EntityHandle == 'AAA33', Opmerking := 'plot5pen7']
gps1[name == 'Rh_4_M_Af' & EntityHandle == 'AAA50', Opmerking := 'plot5pen8']
gps1[name == 'Rh_4_M_Af' & EntityHandle == 'AAA67', Opmerking := 'plot5pen9']
gps1[name == 'Rh_4_M_Af' & EntityHandle == 'AAA84', Opmerking := 'plot5pen10']
gps1[name == 'Rh_4_M_Af' & EntityHandle == 'AAA101', Opmerking := 'plot6pen1']
gps1[name == 'Rh_4_M_Af' & EntityHandle == 'AAA118', Opmerking := 'plot6pen2']

gps1 <- gps1[!is.na(Opmerking),]
gps[,Opmerking := Text]
gps <- rbind(gps,gps1)

# correct data: filter dubbele coördinaten in Zegveld eruit
gps <- setorder(gps, ID)
gps <- gps[!(name %in% c('ZG3c','ZG3b','ZG3a') & EntityHandle %in% unique(gps$EntityHandle[name == 'ZG3d'])),]
gps <- gps[!(name %in% c('ZG3b','ZG3a') & EntityHandle %in% unique(gps$EntityHandle[name == 'ZG3c'])),]
gps <- gps[!(name %in% c('ZG3b') & EntityHandle %in% unique(gps$EntityHandle[name == 'ZG3a'])),]
gps <- gps[!(name %in% c('ZG2a') & EntityHandle %in% unique(gps$EntityHandle[name == 'ZG1b'])),]
gps <- gps[!(name %in% c('ZG1b') & EntityHandle %in% unique(gps$EntityHandle[name == 'ZG1a'])),]

# add additional info
# rondehoep sloot 1 tm 4, 7, 8, 10, zegveld 1 en 2, spaarnwoude is tweezijdig of twee transecten
setDT(gps)
gps[,trajecten := 2]

# correctie data: correct typos
gps[name == 'Rh_3_R_K' & EntityHandle == 'AAA193', Opmerking := 'plot4pen3']
gps[name == 'Rh_3_R_K' & EntityHandle == 'AAA23', Opmerking := 'plot3pen3']
gps[name == 'Sw_2_M' & EntityHandle == 'AAA125', Opmerking := 'plot4pen3']
# voeg volgnummer toe voor bepaling afstand tot sloot
# 1 = perceel, 2 = insteek, 3 tm 12 = oever en insteek en perceel andere oeverzijde
gps <- gps[,dist_id := frank(EntityHandle, ties.method = 'dense'), by = 'ID']

### import gps csv data--------------------------------------------------------
gps2 <-  importGPS2(inputdir = paste0(workspace,"./GPS"), gpsid = max(gps$ID))
setDT(gps2)
gps2[,trajecten := 1]
#correctie data
gps2[name == 'KW_add_1', name:= Laagnaam]
gps2[name == 'RH_11_R_n' & Puntnummer == 3, Opmerking := 'plot3pen1']
gps2[name == 'SW_5_M_z' & Puntnummer == 3, Opmerking := 'plot1pen8']
gps2[name == 'SW_4_M_zo' & Puntnummer == 3, Opmerking := 'plot3pen1']
gps2[name == 'SW_4_M_zo' & Puntnummer == 4, Opmerking := 'plot3pen2']
gps2[name == 'SW_4_M_zo' & Puntnummer == 5, Opmerking := 'plot3pen3']
gps2[name == 'SW_5_M_z' & Puntnummer == 3, Opmerking := 'plot1pen8']

# voeg volgnummer toe voor bepaling afstand tot sloot
# 1 = perceel, 2 = insteek, 3 tm 12 = oever en insteek en perceel andere oeverzijde
gps2 <- gps2[,dist_id := frank(Puntnummer, ties.method = 'dense'), by = 'ID']

### merge csv and dxf--------------------------------------------------------
gps <- gps[,c('ID','name','Opmerking','trajecten','dist_id','geometry')]
gps2 <- gps2[,c('ID','name','Opmerking','trajecten','dist_id','geometry')]
gps <- rbind(gps,gps2)

# create id for filtering
coords <- data.frame(st_coordinates(st_zm(gps$geometry)))
gps <- cbind(gps,coords)
gps[,ident:= paste0(X,'_',Y)]

### import penetrometer data ----------------------------------------------
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

# proces gps files
## proces ----------------------------------------------
### process gps files--------------------------------------------------------
## add location info for filtering and correcting
gps[, gebied:= tstrsplit(name, "_")[1]]
gps[, sloot:= tstrsplit(name, "_")[2]]
gps[ID %in% 29:36, sloot:= sub(".*?(\\d+).*", "\\1", name)]
gps[ID %in% 29:36, gebied:= tstrsplit(name, sloot)[1]]
# onderstaande sloten en opnamen wel eenzijdig (opvallend dat bij sommige sloten soms een, soms twee zijden penetrometer is gedaan)
gps[gebied == 'Rh' & sloot %in% c('1','5','9')| name %in% c('Sw_1_M_Af', 'Sw_3_M')| gebied == "Kw" , trajecten := 1]
# correct dist id for multiple transects (2 oevers or 2 transects)
gps[,max_dis_id := max(dist_id), by = 'name']
gps[trajecten == 2 & dist_id > (floor(max_dis_id/2)), dist_id := dist_id-floor(max_dis_id/2)]

# vragen aan Harm of hij id perceel, insteek en oever en oeverzijde controleert/toevoegt
# nakijken een of tweezijdig coordinaten in relatie tot penetrometerdata
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
#Sw_2_M_AF, SW_6_wp1_



### postprocess penetrometer data ---------------------------------------------

# merge gps with pen
penmerge <- merge(pen, gps, by=c('gebied','plot','extragebied'), all.x = TRUE, all.y=FALSE, allow.cartesian = TRUE, suffixes = c('_pen','_gps'))
# check double gps
checkgps <- gps[, nunique := uniqueN(ident), by = c('gebied','plot','extragebied')]
# als ID.y = na : wel penetrometer, geen matching gps
# sorteren op ID.x
penmergecheck <- unique(penmerge[,c('name_pen','ID_pen','name_gps','ID_gps','plot','gebied','sloot','extragebied','trajecten','dist_id')])
write.table(penmergecheck, file = paste(workspace,"/output/penetrometer_gps_check",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
gps <- st_as_sf(gps)
st_write(gps, paste0(workspace,  "output/gps_penetrometer.gpkg"))
# soms meerdere gps coordinaten bij 1 gebied, plot, extragebied 
# merge met abiotiek aan een verkeerde zijde kan op oever holling na
# merge met vegetatie hoe? dan moet ik ook oeverzijde hebben




# Abiotiek ---------------------------------------------------------
## import ----------------------------------------------------------
inputdir <- paste0(workspace,"./ODK_abiotiek")
abio <- file.info(list.files(path= paste0(inputdir), pattern=".csv", full.names =  T))
abio <- rownames(abio)[which.max(abio$mtime)]
abio <- fread(abio)
abio <- abio[,gebied := sapply(strsplit(SlootID, '_'), `[`, 1)]
abio <- abio[,sloot := sapply(strsplit(SlootID, '_'), `[`, 2)]
abio <- abio[,behandeling := sapply(strsplit(SlootID, '_'), `[`, 3)]
abio <- abio[,oever := sapply(strsplit(SlootID, '_'), `[`, 4)]
abio[,SlootID_kort := paste0(gebied, '_',sloot, '_',behandeling)]


# planing/behandelingen --------------------------------------------

# Slootprofielen ---------------------------------------------------

profiel <-  importGPSprof(inputdir = paste0(workspace,"./GPS slootprofielen"))
## Postprocess profielen-----------------------------------------------------

### correct Mijnden -------------------------------------
setDT(profiel)
profiel[name == 'MD_1_NVO' & Puntnummer == 11, Opmerking := 'waterlijn']

### process --------------
setDT(profiel)
#extract label waterlijn
profiel[grepl('waterlijn', Opmerking), wl:= z]
profiel[, wl := mean(wl, na.rm = TRUE), by = "ID"]
profiel[grepl('waterlijn', Opmerking), numwl := Puntnummer]
profiel[, numwl_min := min(numwl, na.rm = TRUE), by = "ID"]
profiel[, numwl_max := max(numwl, na.rm = TRUE), by = "ID"]
profiel[Puntnummer > numwl_max | Puntnummer < numwl_min, wl := NA]
#extract slibdikte
profiel[, slib := as.numeric(Opmerking)/100]
profiel[is.na(slib), slib:= 0]
profiel[, c('numwl_min','numwl_max','numwl'):=NULL]

# afstand in meters toevoegen
profiel[, dist := sqrt((x[Puntnummer == 1]-x)^2+(y[Puntnummer == 1]-y)^2), by ='ID'] 
profiel[, rel_dist := dist - shift(dist,-1), by ='ID'] 
profiel[rel_dist > 0 , rel_dist := rel_dist *-1]
profiel[, midpoint := mean(dist[!is.na(wl)]), by ='ID']
profiel[, midpoint_dist := dist - midpoint]
profiel[, mean_rel_dist := mean(rel_dist, na.rm =TRUE), by ='ID']

# add sectie sloot, oever, perceel
profiel[!is.na(wl), sectie := 'water', by ='ID']
profiel[rel_dist <= mean(rel_dist, na.rm =TRUE) & is.na(wl), sectie := 'perceel', by ='ID']
profiel[rel_dist > mean(rel_dist, na.rm =TRUE) & is.na(wl), sectie := 'oever', by ='ID']
# correct first point
profiel[dist == 0, sectie := 'perceel']
profiel[is.na(rel_dist), sectie := 'perceel']
# correct last point
profiel[sectie == 'perceel' & shift(sectie,+1) == 'oever', sectie := 'oever']

# windrichting toevoegen
for(i in unique(profiel$ID)){
  profiel_nr <- profiel[profiel$ID == i,]
  transect_direction <- get_cardinal_direction(profiel_nr)
  profiel[profiel$ID == i, azimuth := transect_direction]
}

# max waterdiepte
profiel[, wtd := wl - z]
profiel[wtd < 0, wtd := 0]
profiel[,max_wtd := max(wtd, na.rm = T), by = 'ID']
# max slibdikte
profiel[,max_slib := max(slib, na.rm = T), by = 'ID']

# taludhoek
for(i in unique(profiel$ID)){
  profiel_nr <- profiel[profiel$ID == i,]
  talud <- calc_taludhoek(profiel_nr)
  profiel[profiel$ID == i, c('tldk_bvwtr_perc_1','tldk_bvwtr_graden_1','tldk_bvwtr_perc_2','tldk_bvwtr_graden_2',
                             'tldk_ondwtr_perc_1','tldk_ondwtr_graden_1','tldk_ondwtr_perc_2','tldk_ondwtr_graden_2',
                             'tldk_wtr_perc_1','tldk_wtr_graden_1','tldk_wtr_perc_2 ','tldk_wtr_graden_2',
                             'tldk_vastbodem_perc_1','tldk_vastbodem_graden_1','tldk_vastbodem_perc_2','tldk_vastbodem_graden_2') := talud]
}

write.table(penmergecheck, file = paste(workspace,"/output/profiel",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
profiel <- st_as_sf(profiel)
st_write(profiel, paste0(workspace,  "output/profiel.gpkg"))

# Waterbodemdata ---------------------------------------------------
# Bodemgegevens oever ----------------------------------------------

# Validatie --------------------------------------------------

# 1- validatie regels coördinaten
# check 4 double coordinates
gps <- st_as_sf(gps)
gps2 <- st_zm(gps, crs = 28992)
coords <- as.data.table(st_coordinates(gps2))
dubbel <- gps[which(duplicated(coords)),]

# to do:
# check of lijnen veldform abiotiek kruisen met penetrometer
# check of lijnen veldform hetzelfde zijn als planningtabel
# visualisatie behandelingen op kaart
# validatieregels locatie(codes) en relatie met beheer/ behandelingen
# check of beheerinvoer odk form == beheer planningstabel

# check voor dubbele locatiecodes in profielen
uniqueN(profiel$name) == uniqueN(profiel$ID)
checkloc <- dcast(profiel, name~., value.var = 'ID', fun.aggregate = uniqueN)
# check 4 floating secties
check_floating_secties <- profiel[!sectie == shift(sectie,-1) & !sectie == shift(sectie,+1)]


# Create database/ merge gegevens ----------------------------------------

prof_abio <- merge(profiel, abio, by.x = 'name', by.y = 'SlootID_kort', all.x = T, all.y = T)
# merge alleen de goede oeverzijde met abiotiek, nu beide oeverzijden
# traject maken van x, y abiotiek


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
# profielen, hoeken, onderholling
prof <- unique(prof_abio[,c('name','max_wtd','max_slib','holleoever','tldk_bvwtr_perc_1','tldk_bvwtr_graden_1','tldk_bvwtr_perc_2','tldk_bvwtr_graden_2',
                    'tldk_ondwtr_perc_1','tldk_ondwtr_graden_1','tldk_ondwtr_perc_2','tldk_ondwtr_graden_2',
                    'tldk_wtr_perc_1','tldk_wtr_graden_1','tldk_wtr_perc_2 ','tldk_wtr_graden_2',
                    'tldk_vastbodem_perc_1','tldk_vastbodem_graden_1','tldk_vastbodem_perc_2','tldk_vastbodem_graden_2')])
write.table(prof, file = paste0('output/profielen_hoek.csv' ), sep = ';',dec = '.', row.names = F)
saveRDS(report, paste0(workspace, 'products/', snapshot.version, '/report.rds'))


