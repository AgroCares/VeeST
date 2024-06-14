
# Load packages -----------------------------------------------------------
library(data.table)
library(sf)
library(R.utils)

# Settings ----------------------------------------------------------------
workspace <- paste0(Sys.getenv("NMI-SITE"), 'O 1900 - O 2000/1971.N.23 DAS Waternet ondersteuning ecologie/03. Data en resultaten/Data_ontsluiten/')

# Load custom functions
source(paste0("scripts/functions/import_snapshot.R"))
source(paste0("scripts/functions/categorize_eag.R"))

# Preprocess subarea/ deelgebied (EAG) data -----------------------------------------------------
# Load base layer with EAGs 
EAG <- st_read(paste0(workspace, 'input/', snapshot.version,'/gebiedsinfo/EAG_20220809.shp')) %>% st_transform(28992)
setnames(EAG,c("Code" ,"Naam","Soort","Oppervlakt","Omschrijvi" ,"Opmerking" , "Wtype_BR"  , "Wtype_WBP",  "geometry"),
         c("EAGIDENT" ,"EAGNAAM","Soort", "EAGOPPVL", "OSMOMSCH" ,"OPMERKINGE" , "OWMTYPE"  , "OWMTYPE_WBP",  "geometry"))
EAG <- setDT(EAG)
# add GAF/ aan afvoergebied code
EAG[,GAFIDENT:= sapply(strsplit(EAG$EAGIDENT, '-'), `[`, 1)]
EAG[,GAFNAAM := sapply(strsplit(EAG$EAGNAAM, ','), `[`, 1)]
# correct gaf names with ',' in their name, this should be corrected in input data
EAG[GAFIDENT == '3070', GAFNAAM := 'Holland, Sticht, Voorburg en Polder het Honderd oost']
EAG[EAGIDENT == '3070-EAG-2', EAGNAAM := 'deelgebied 2']
EAG[EAGIDENT == '3070-EAG-1', EAGNAAM := 'Voorburg']
EAG[GAFIDENT == '7000', GAFNAAM := 'IJmeer, Markermeer, Gooimeer en Eemmeer']
EAG[GAFIDENT == '6000', GAFNAAM := 'Noordzeekanaal, IJ, Amsterdamrijnkanaalboezem']
EAG[GAFIDENT == '8000', GAFNAAM := 'Rijnlands Boezem']
EAG[EAGIDENT == '7000-EAG-1', EAGNAAM := 'IJmeer']
EAG[EAGIDENT == '7000-EAG-2', EAGNAAM := 'Diemerzeedijk']
EAG[EAGIDENT == '7000-EAG-3', EAGNAAM := 'deelgebied 3']
EAG[EAGIDENT == '7000-EAG-3', EAGNAAM := 'deelgebied 4']
# remove unclear names
EAG[,EAGNAAM := gsub(", Nog opknippen in  EAG's", "", EAGNAAM)]
EAG[,EAGNAAM := trimws(EAG$EAGNAAM, whitespace = ",", which = 'right')]
# remove gafname
EAG[,EAGNAAM := sapply(strsplit(EAG$EAGNAAM, ','), `[`, 2)]
EAG[,EAGNAAM := gsub("^\\s+|\\s+$", "", EAGNAAM)]
# add unique names
EAG[,unique.eag := uniqueN(EAGIDENT), by = 'GAFIDENT']
EAG[,unique.eag.nm := uniqueN(EAGNAAM), by = 'GAFIDENT']
EAG[,EAGNAAM2 := sapply(strsplit(EAGIDENT, '-'), `[`, 3)]
EAG[!unique.eag == unique.eag.nm| is.na(EAGNAAM), EAGNAAM := paste0('Deelgebied ',EAGNAAM2)]
# correct different gafs, with same name, this should be corrected in input data
EAG[GAFIDENT == '6110', GAFNAAM := 'Noorder IJpolder 2']
EAG[, EAGNAAM := capitalize(EAGNAAM)]
#correct names on request Waternet
EAG[GAFIDENT == '2540', GAFNAAM := 'Polder Groot Mijdrecht']
EAG[, EAGNAAM := gsub("Staatsbosbheer", "Staatsbosbeheer", EAGNAAM, fixed = TRUE)]
EAG[, EAGNAAM := gsub("Staatbosbheer", "Staatsbosbeheer", EAGNAAM, fixed = TRUE)]
# add trimmed gafname for folders
EAG[, GAFNAAM_trim := gsub(" ", "", GAFNAAM, fixed = TRUE)]
EAG<- EAG[!GAFNAAM == 'Geen EAG',]
EAG<- EAG[!GAFNAAM == '3???',]
EAG <- st_as_sf(EAG) %>% st_transform(28992)

#save EAG's as gpkg
st_write(EAG, paste0(workspace, 'products/', snapshot.version, '/EAG_20220809.gpkg'), append = FALSE)
# additional info on EAGs: relation KRW-EAG, code KRW/ EAG 2 names, watertype, type agricultural area DAW
# not used in reports at the moment
eag_wl <- data.table::fread(paste0(workspace, 'input/',snapshot.version,'/gebiedsinfo/EAG_Opp_kenmerken_20220811.csv'))
eag_wl <- eag_wl[is.na(eag_wl$Einddatum),]
#import brp to add info on land use
brp <- st_read(paste0(Sys.getenv("NMI_DATA"),"landgebruik/brp/raw/brpgewaspercelen_definitief_2021.gpkg")) %>% st_transform(28992)
eag_wl <- addGebiedstype(brp, EAG, eag_wl)

# Preprocess KRW ecological data -----------------------------------------------------
# load location info and data for subsetting EAG and GAF
locaties <- data.table::fread(paste0(workspace, 'input/', snapshot.version, '/gebiedsinfo/Location.csv'))
# add GAF/ aan afvoergebied code (it is not correct in the original table)
locaties$GAFIDENT <- sapply(strsplit(locaties$EAGIDENT, '-'), `[`, 1)
# load 4 import krw - indeling maatlatten en relatie met deelmaatlatten en indicatoren
orderMaatlatten <- fread(paste0(workspace, 'input/domeinen_krw/orderMaatlatten.csv'), encoding = "Latin-1") 
orderMaatlatten$GHPR <- trimws(orderMaatlatten$GHPR)
# create ppr KRW data files
ekrlijst <- importKRW(inputdir = paste0(workspace, 'input/',snapshot.version,'/rapportagefiles/'), locaties = locaties, eag_wl, orderMaatlatten)
# goals per EAG/ water body
doelen <- data.table::fread(paste0(workspace, 'input/',snapshot.version,'/gebiedsinfo/Doelen.csv'))
# load addition paramter/ taxa information from the aquo-kit
sompar <- fread(paste0(workspace, 'input/domeinen_krw/Somparametersamenstelling_updated_LM.csv'), encoding = "Latin-1") 
# add KRW doelen/ goals of waterbodies
ekrlijst <- ppr_ekr(ekrlijst, doelen = doelen, sompar = sompar)  
# export report with chem, krw & map & locations
report <- list(locaties = locaties, EKRlijst = ekrlijst, EAG = EAG, eag_wl =eag_wl, snapshot.version)

# Preprocess Fysical & Chemical data -----------------------------------------------------
# create and preproces (and validate) data
fychem <- importfychem(inputdir = paste0(workspace, 'input/',snapshot.version,'/fychem/'), locaties = locaties)
fychem <- ppr_fychem(fychem, meetnet = "*VMgemalen*", pars = c("Ntot_mgN/l_nf","Ptot_mgP/l",'Ntot_mgN/l'), minjaar = 1999)
  
# Export the data ---------------------------------------------------------
# waarom ppr files wegschrijven als csv en daarna weer inladen voor validatie en rendering?
saveRDS(report, paste0(workspace, 'products/', snapshot.version, '/report.rds'))
saveRDS(fychem, paste0(workspace, 'products/', snapshot.version, '/fychem.rds'))
