# ============================================================
# Data import Mijnden 2026 - 2030
# Importeert: GPS oeverprofielen, GPS penetrometer locaties,
#             penetrometer data, abiotiek (uit VeeST abio object)
# ============================================================

library(data.table)
library(sf)
library(ggplot2)
library(ggspatial)  # annotation_map_tile (OSM achtergrond)

source("scripts/functions/functions_veest.R")

workspace_md <- paste0(
  Sys.getenv("NMI-SITE"),
  "O 2000 - O 2001/2038.N.26 Natuurpotentie Mijnden 2026 - 2030/04. Data en resultaten/"
)

# ============================================================
# 1. GPS OEVERPROFIELEN
# ============================================================

profiel_24 <- importGPSprof(
  inputdir = paste0(workspace_md, "GPS oeverprofielen/2024")
)
setDT(profiel_24)
profiel_24[, jaar := 2024]
profiel_24[, ID := paste0(ID, "_24")]

profiel_26 <- importGPSprof(
  inputdir = paste0(workspace_md, "GPS oeverprofielen/2026")
)
setDT(profiel_26)
profiel_26[, jaar := 2026]
profiel_26[, ID := paste0(ID, "_26")]

profiel_md <- rbind(profiel_24, profiel_26, fill = TRUE)

## 1.1 Postprocessing ------------------------------------------

# Standaardiseer label waterlijn
profiel_md[grepl("Waterlijn", Opmerking, ignore.case = TRUE), Opmerking := "waterlijn"]

# Handmatige correctie MD_1_NVO 2024 (conform VeeST script)
profiel_md[name == "MD_1_NVO" & Puntnummer == 11 & jaar == 2024,
           Opmerking := "waterlijn"]

# Extraheer waterlijn z-waarde
profiel_md[grepl("waterlijn", Opmerking), wl := z]
profiel_md[, wl := mean(wl, na.rm = TRUE), by = "ID"]
profiel_md[grepl("waterlijn", Opmerking), numwl := Puntnummer]
profiel_md[, numwl_min := min(numwl, na.rm = TRUE), by = "ID"]
profiel_md[, numwl_max := max(numwl, na.rm = TRUE), by = "ID"]
profiel_md[Puntnummer > numwl_max | Puntnummer < numwl_min, wl := NA]

# Slibdikte (Opmerking bevat numerieke waarden in cm)
profiel_md[, slib := suppressWarnings(as.numeric(Opmerking)) / 100]

# Afstand in meters
profiel_md[, dist := sqrt(
  (x[Puntnummer == 1] - x)^2 + (y[Puntnummer == 1] - y)^2
), by = "ID"]
profiel_md[, rel_dist := dist - shift(dist, -1), by = "ID"]
profiel_md[rel_dist > 0, rel_dist := rel_dist * -1]
profiel_md[, midpoint := mean(dist[!is.na(wl)]), by = "ID"]
profiel_md[, midpoint_dist := dist - midpoint]
profiel_md[, mean_rel_dist := mean(rel_dist, na.rm = TRUE), by = "ID"]

# Sectie indeling: water / oever / perceel
profiel_md[!is.na(wl), sectie := "water", by = "ID"]
profiel_md[
  rel_dist <= mean(rel_dist, na.rm = TRUE) & is.na(wl),
  sectie := "perceel", by = "ID"
]
profiel_md[
  rel_dist > mean(rel_dist, na.rm = TRUE) & is.na(wl),
  sectie := "oever", by = "ID"
]
profiel_md[, sectie_2 := ifelse(midpoint_dist < 0, 1, 2)]
profiel_md[dist == 0, sectie := "perceel"]
profiel_md[is.na(rel_dist), sectie := "perceel"]
profiel_md[
  sectie == "perceel" & shift(sectie, +1) == "oever",
  sectie := "oever"
]

# Windrichting per transect
for (i in unique(profiel_md$ID)) {
  profiel_nr <- profiel_md[ID == i]
  profiel_md[ID == i, azimuth := get_cardinal_direction(profiel_nr)]
}

# Waterdiepte, slibdikte, drooglegging
profiel_md[, wtd := wl - z]
profiel_md[wtd < 0, wtd := 0]
profiel_md[, max_wtd := max(wtd, na.rm = TRUE), by = "ID"]
profiel_md[!is.na(slib) & slib != 0, slib := slib - wtd]
profiel_md[is.na(slib), slib := 0]
profiel_md[, max_slib := max(slib, na.rm = TRUE), by = "ID"]
profiel_md[, watbte := dist[Puntnummer == numwl_max] - dist[Puntnummer == numwl_min],
           by = "ID"]
profiel_md[sectie == "oever", max_hgt_or := max(z, na.rm = TRUE), by = "ID"]
profiel_md[, drglg := max_hgt_or - mean(wl, na.rm = TRUE), by = "ID"]

# Taludhoek
profiel_md[
  sectie_2 == 1,
  talud := 100 * ((z - shift(z, -1)) / (-1 * (dist - shift(dist, -1)))),
  by = "ID"
]
profiel_md[
  sectie_2 == 2,
  talud := 100 * ((z - shift(z, +1)) / (dist - shift(dist, +1))),
  by = "ID"
]
profiel_md[, mean_talud := mean(talud, na.rm = TRUE), by = c("ID", "sectie", "sectie_2")]

## 1.2 Aggregeer naar wide formaat (één rij per profiel x sectie_2 x jaar) ------

profiel_md_wide <- dcast(
  profiel_md,
  name + sectie_2 + jaar ~ .,
  value.var = c("max_slib", "max_wtd", "watbte", "drglg", "max_hgt_or", "wl"),
  fun.aggregate = mean, na.rm = TRUE, fill = FALSE, drop = TRUE
)

# ============================================================
# 2. KAART: waterlijn z-waarden op OSM achtergrond
# ============================================================

# Één waterlijn z-waarde per profiel (gemiddeld als er meerdere wl-punten zijn)
wl_punten <- profiel_md[
  grepl("waterlijn", Opmerking),
  .(wl = mean(z, na.rm = TRUE), x = mean(x), y = mean(y)),
  by = .(name, jaar)
]

# Converteer naar sf WGS84
wl_sf <- st_as_sf(wl_punten, coords = c("x", "y"), crs = 28992) |>
  st_transform(4326)

# ggrepel plot labels als tekst (geen sf-geometrie); coördinaten losbreken
# zodat geom_label_repel() de labels kan verschuiven en met een lijntje aan
# het bijbehorende punt kan verbinden.
library(ggrepel)
wl_coords <- as.data.table(st_coordinates(wl_sf))
setnames(wl_coords, c("X", "Y"), c("lon", "lat"))
wl_pts <- cbind(st_drop_geometry(wl_sf), wl_coords)

# Punten van hetzelfde profiel in verschillende jaren liggen vrijwel exact op
# elkaar (zelfde GPS-locatie ingemeten in 2024 en 2026), waardoor de vorm
# (cirkel/driehoek) niet te onderscheiden was. Verschuif elk jaar een klein,
# vast stukje in tegengestelde richting (geen willekeurige jitter, zodat
# punten die bij hetzelfde profiel horen dicht bij elkaar herkenbaar blijven).
offset_deg <- 0.00012
wl_pts[, `:=`(
  lon_plot = lon + ifelse(jaar == 2026, offset_deg, -offset_deg),
  lat_plot = lat + ifelse(jaar == 2026, offset_deg, -offset_deg)
)]

# Peilgebieden AGV inladen en beperken tot de bbox van de huidige kaart (met
# een kleine marge), zodat alleen relevante vlakken worden getekend/gelabeld.
peilgeb_pad <- "C:/Users/LauraMoria/NMI/Gerard Ros - NMI-PROJ/VeeST/peilgebieden/AGV/"
peilgeb_praktijk <- st_read(paste0(peilgeb_pad, "PraktijkPeilgebieden_AGV_20241008.shp"), quiet = TRUE)
peilgeb_afwijkend <- st_read(paste0(peilgeb_pad, "AfwijkendePeilgebieden_AGV_20241008.shp"), quiet = TRUE)
# enkele geometrieën in de brondata bevatten ongeldige (zelf-intersecterende)
# vlakken; st_crop/st_intersection breekt daarop af zonder deze correctie
peilgeb_praktijk <- st_make_valid(peilgeb_praktijk)
peilgeb_afwijkend <- st_make_valid(peilgeb_afwijkend)

bbox_kaart <- st_bbox(wl_sf)
# marge van ~10% van de breedte/hoogte van de bbox
# (unname() voorkomt dat de meegeërfde "xmax"/"ymax" namen van marge_x/marge_y
# de namen in c() overschrijven, wat st_bbox() met NA's liet falen)
marge_x <- unname((bbox_kaart["xmax"] - bbox_kaart["xmin"]) * 0.1)
marge_y <- unname((bbox_kaart["ymax"] - bbox_kaart["ymin"]) * 0.1)
bbox_kaart_marge <- st_bbox(c(
  xmin = unname(bbox_kaart["xmin"]) - marge_x, xmax = unname(bbox_kaart["xmax"]) + marge_x,
  ymin = unname(bbox_kaart["ymin"]) - marge_y, ymax = unname(bbox_kaart["ymax"]) + marge_y
), crs = st_crs(wl_sf))

peilgeb_praktijk <- st_transform(peilgeb_praktijk, 4326)
peilgeb_afwijkend <- st_transform(peilgeb_afwijkend, 4326)
peilgeb_praktijk_crop <- suppressWarnings(st_crop(peilgeb_praktijk, bbox_kaart_marge))
peilgeb_afwijkend_crop <- suppressWarnings(st_crop(peilgeb_afwijkend, bbox_kaart_marge))

# Kaart met OSM achtergrond
# NB: annotation_map_tile() bepaalt het aantal te downloaden tiles op basis
# van de bounding box van de *hele plot* en het gekozen zoomniveau. Bij een
# klein studiegebied (zoals hier) leidt een hoog zoomniveau (14+) tot een
# onnodig grote hoeveelheid tiles en een vastlopende/trage render, maar bij
# een klein gebied (zoals dit) blijft het aantal tiles ook op zoom 16 beperkt
# en levert dat een veel scherpere achtergrond op dan zoom 12.
# Punten en shapes worden nu los van de labels getekend (grotere size, dikkere
# stroke) zodat de vorm per jaar duidelijk te onderscheiden is, en labels
# krijgen via ggrepel een verbindingslijntje naar hun punt zodat ze niet meer
# overlappen en toch traceerbaar blijven naar de exacte locatie.
print(
  ggplot() +
    annotation_map_tile(type = "osm", zoomin = 0, zoom = 16, quiet = TRUE, cachedir = tempdir()) +
    geom_sf(
      data = peilgeb_praktijk_crop, aes(linetype = "Praktijkpeilgebied"),
      fill = NA, color = "steelblue4", linewidth = 0.6
    ) +
    geom_sf(
      data = peilgeb_afwijkend_crop, aes(linetype = "Afwijkend peilgebied"),
      fill = NA, color = "firebrick", linewidth = 0.6
    ) +
    geom_point(
      data = wl_pts, aes(x = lon_plot, y = lat_plot, color = wl, shape = factor(jaar)),
      size = 4, stroke = 1.3
    ) +
    geom_label_repel(
      data = wl_pts,
      aes(x = lon_plot, y = lat_plot, label = paste0(name, "\n", round(wl, 2), " m")),
      size = 2.5, label.padding = unit(0.15, "lines"), alpha = 0.85,
      min.segment.length = 0, segment.color = "grey20", segment.size = 0.4,
      box.padding = 0.6, point.padding = 0.3, force = 15, force_pull = 0.5,
      max.overlaps = Inf, max.time = 3, max.iter = 100000, seed = 8341
    ) +
    scale_color_viridis_c(name = "Waterlijn (m NAP)", option = "plasma") +
    scale_shape_manual(name = "Jaar", values = c("2024" = 16, "2026" = 17)) +
    coord_sf(crs = 4326) +
    labs(
      title = "Waterlijn hoogte (m NAP) — Mijnden oeverprofielen",
      subtitle = "z-waarde GPS punt gelabeld als 'waterlijn' (bron: GPS oeverprofielen)",
      x = NULL, y = NULL
    ) +
    theme_minimal()
)
ggsave(
  filename = paste0(workspace_md, "Kaarten/kaart_waterlijn_mijnden.png"),
  width = 8, height = 6, units = "in", dpi = 300
)
# ============================================================
# 3. GPS PENETROMETER LOCATIES
# ============================================================

# Elke GPS-penetrometerfile (per sloot/oever) bevat punten met een
# Opmerking-veld gecodeerd als "plot<n>pen<p>". Dit is dezelfde codering die
# de penetrometerdata gebruikt (Plotnaam = PLOTX00n, Pen = p). De koppeling
# tussen gps_pen_md en pen_md loopt dus via deze code (plot+pen), NIET
# ruimtelijk - zie scripts/data_import_ppr.R sectie 2 voor het analoge
# patroon met de VeeST-data.

gps_pen_md <- importGPS2(
  inputdir = paste0(workspace_md, "GPS penetrometer/2026")
)
coords_pen_md <- st_coordinates(st_zm(gps_pen_md))
gps_pen_md <- st_drop_geometry(gps_pen_md)
setDT(gps_pen_md)
gps_pen_md[, c("x", "y") := list(coords_pen_md[, "X"], coords_pen_md[, "Y"])]
# Extraheer slootnummer en oeverzijde uit bestandsnaam (bijv. MD_1_NVO_n)
gps_pen_md[, sloot_nr := tstrsplit(name, "_")[[2]]]
gps_pen_md[, oever := toupper(tstrsplit(name, "_")[[4]])]
gps_pen_md[, jaar := 2026]

# maak koppelcode identiek aan pen_md$plot (zie hieronder)
gps_pen_md[, Opmerking := tolower(Opmerking)]
gps_pen_md[, plot := gsub("\\s+", "", Opmerking)]

# check op dubbele plot-codes binnen de GPS-set (zou uniek moeten zijn)
dubbel_gps_pen_md <- gps_pen_md[, .N, by = plot][N > 1]

# ============================================================
# 4. PENETROMETER DATA (MD_1_NVO tm MD_8_NVO.txt)
# ============================================================

# Eén .txt bestand bevat alle plots (Eijkelkamp Penetro Viewer v6.08).
# Plotnamen (PLOTX001-004) + Pen (1-10) vormen samen dezelfde "plot<n>pen<p>"
# code als in de GPS-penetrometerbestanden (Opmerking). Koppeling gebeurt dus
# via deze code, niet via coördinaten.

pen_md <- importPen2(
  inputdir = paste0(workspace_md, "Penetrometer/2026")
)
setDT(pen_md)
pen_md[, jaar := 2026]

# Numeriek maken
pen_md[, Diept := as.numeric(Diept)]
pen_md[, indringingsweerstand := as.numeric(indringingsweerstand)]

# maak koppelcode: PLOTX001 -> plot1, Pen 6 -> pen6 => "plot1pen6"
pen_md[, plotnr := as.numeric(gsub("^PLOTX0*", "", Plotnaam))]
pen_md[, plot := paste0("plot", plotnr, "pen", Pen)]

# koppel gps aan pen via de plot-code (niet ruimtelijk)
pen_md <- merge(pen_md, gps_pen_md[, .(plot, name, sloot_nr, oever, x, y)],
                 by = "plot", all.x = TRUE, suffixes = c("_pen", "_gps"))

# check op ontbrekende koppelingen (pen zonder gps of gps zonder pen)
penmd_missing_gps <- unique(pen_md[is.na(name_gps), .(plot, Plotnaam, Pen)])
gpsmd_missing_pen <- gps_pen_md[!plot %in% unique(pen_md$plot), .(plot, name)]

# Diepte bins (identiek aan VeeST script)
pen_md[, diepte_wortels := cut(Diept,
  breaks = c(0, 15, 40, 85),
  include.lowest = TRUE
)]
pen_md[, dieptebin := cut(Diept,
  breaks = c(0, 10, 20, 30, 40, 50, 85),
  include.lowest = TRUE
)]

# ============================================================
# 5. ABIOTIEK MIJNDEN (filter uit VeeST abio object)
# ============================================================

if (!exists("abio")) {
  stop("abio object niet gevonden. Voer eerst scripts/data_import_ppr.R uit.")
}

abio_md <- abio[grepl("^MD", SlootID_abio)]
setDT(abio_md)

# Selecteer relevante abiotische kolommen
abio_cols_keep <- c(
  "SlootID_abio", "SlootID", "jaar",
  "watertemp_C", "water_conductiviteit_uS_cm", "water_pH", "water_redox",
  "water_O2_mgL", "doorzicht1_mid_cm", "waterdiepte1_mid_cm",
  "slib_pH", "slib_redox_mgL", "slib_conductiviteit_uS_cm",
  "holleoever", "landgebruik_traject", "landgebruik_overkant",
  "porievochtmonster_code", "slibmonster_code",
  "bodemmonster_oeverzone2b3_code", "Waarnemer", "SubmissionDate"
)
abio_cols_keep <- abio_cols_keep[abio_cols_keep %in% names(abio_md)]
abio_md <- abio_md[, ..abio_cols_keep]

message("Import Mijnden klaar:")
message("  - Oeverprofielen: ", uniqueN(profiel_md$name), " profielen (",
        nrow(profiel_md), " punten), jaren: ",
        paste(sort(unique(profiel_md$jaar)), collapse = ", "))
message("  - GPS penetrometer: ", nrow(gps_pen_md), " locaties")
message("  - Penetrometer: ", nrow(pen_md), " metingen, ",
        uniqueN(pen_md$sloot_nr), " sloten")
message("  - Abiotiek MD: ", nrow(abio_md), " rijen, ",
        uniqueN(abio_md$SlootID_abio), " unieke sloten")
