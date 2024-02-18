# Anti-AfD protests 2024

library(tidyverse)
library(tidygeocoder)
library(ggplot2)
library(sf)
library(rnaturalearth)
# Support for Spatial objects (`sp`) will be deprecated in {rnaturalearth} and 
# will be removed in a future release of the package. Please use `sf` objects 
# with {rnaturalearth}. For example: `ne_download(returnclass = 'sf')`
library(mapview)
library(dplyr)
library(leafpop)

## VISUALISATIONS
## - overall map
## - table of weeks (weeks 2, 3, 4, 5, 6) and cumulative reported participants
## - sequence over time maps (weeks 2, 3, 4, 5, 6)
## - density map
## - interactive map

setwd("~/Downloads")

antiAfD <- read.csv("demos_past.csv", header = T)
# head(antiAfD)

antiAfD$Land <- "Deutschland"

antiAfD$Datum = ifelse(antiAfD$Ort == "Germersheim" & 
                         antiAfD$Höchstangabe.Teilnehmerzahl == "300",
                       "27.01.2024", antiAfD$Datum)

antiAfD$Datum = ifelse(antiAfD$Ort == "Neustadt am Rennsteig" & 
                         antiAfD$Mindestangabe.Teilnehmerzahl == "100",
                       "24.01.2024", antiAfD$Datum)

# antiAfD$Datum = ifelse(antiAfD$Ort == "Friedrichstadt" & 
#                          antiAfD$Link.zum.Bericht == "https://www.ndr.de/nachrichten/schleswig-holstein/Mehrere-Tausend-Menschen-bei-Demos-gegen-Rechtsextremismus,demo4174.html", 
#                        "02.02.2024", antiAfD$Datum)

antiAfD$Datum <- as.Date(antiAfD$Datum, "%d.%m.%Y")

antiAfD$Woche <- strftime(antiAfD$Datum, format = "%V")

# write.csv(antiAfD, file = "antiAfD_demos.csv")

antiAfD_geo <- antiAfD %>% 
  tidygeocoder::geocode(
    city = Ort,
    country = Land,
    method = "osm"
  )

# replace(is.na(.), 0)
# mutate(dt, x = ifelse(is.na(x), 0, x))

k <- antiAfD %>% 
  mutate(Mindestangabe.Teilnehmerzahl = ifelse(is.na(Mindestangabe.Teilnehmerzahl), 0, Mindestangabe.Teilnehmerzahl)) %>% 
  group_by(Woche) %>% 
  summarise(Veranstaltungen = n(),
            Gesamtteilnehmerzahl = sum(Mindestangabe.Teilnehmerzahl))
k

# # View(antiAfD_geo)
# antiAfD_geo[is.na(antiAfD_geo$lat),]

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort == "Westerland/Sylt", 54.89905576580019, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Westerland/Sylt", 8.33547801856511, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort == "Offenbach a.M.", 50.09510470752502, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Offenbach a.M.", 8.773607955978425, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Ettlingen-Oberweier", 48.91416026634079, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Ettlingen-Oberweier", 8.38270053611117, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Bad Salzuflen-Schötmar", 52.07300267066075, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Bad Salzuflen-Schötmar", 8.760271778384201, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Heide, Dithmarschen ", 54.194885236558406, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Heide, Dithmarschen ", 9.102616365999522, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Husum/Nordfriesland", 54.48558242241668, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Husum/Nordfriesland", 9.056781767618697, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Weißwasser/Oberlausitz", 51.50173607047371, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Weißwasser/Oberlausitz", 14.640865339521486, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Bad Neuen-Ahrweiler", 50.539890322780586, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Bad Neuen-Ahrweiler", 7.119434430943779, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Freienthal (neben AfD Bürgerdialog)", 52.23062871216718, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Freienthal (neben AfD Bürgerdialog)", 12.718701818256582, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Arnsberg/ Neheim", 51.4522198415651, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Arnsberg/ Neheim", 7.96895465962452, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Barnstorf, Niedersachsen", 52.705639935243234, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Barnstorf, Niedersachsen", 8.49782818407366, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Gilching (gegendemo)", 48.10964036073649, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Gilching (gegendemo)", 11.293350151797846, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Bad Berleburg/Wittgenstein", 51.034068464940695, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Bad Berleburg/Wittgenstein", 8.373193438600603, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Bretten - Alice Weidel zu Besuch", 49.03555690367905, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Bretten - Alice Weidel zu Besuch", 8.707028273486904, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Hamburg-St.Pauli", 53.5534468527452, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Hamburg-St.Pauli", 9.96860755690091, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Seefeld (Gegendemo)", 48.03186602852013, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Seefeld (Gegendemo)", 11.214289453088897, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Stederdorf (Peine)", 52.345361577696515, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Stederdorf (Peine)", 10.243674015493275, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Drensteinfrut", 51.794045156101035, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Drensteinfrut", 7.736201683238924, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Legden/Asbeck", 52.03266826671535, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Legden/Asbeck", 7.099798337259099, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Makranstädt", 51.30407035139117, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Makranstädt", 12.223347522187051, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Dortmund-Dorstfeld", 51.50416696272409, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Dortmund-Dorstfeld", 7.421403753953722, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Rheinberg-Ossenberg", 51.56365319765854, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Rheinberg-Ossenberg", 6.580862360765873, antiAfD_geo$long)

antiAfD_geo$lat = ifelse(antiAfD_geo$Ort ==  "Winterberg, Hochsauerlandkreis", 51.19409417194218, antiAfD_geo$lat)
antiAfD_geo$long = ifelse(antiAfD_geo$Ort == "Winterberg, Hochsauerlandkreis", 8.527508159146677, antiAfD_geo$long)

antiAfD_geo[is.na(antiAfD_geo$lat),]

# antiAfD_geo <- antiAfD_geo %>% drop_na(long)

antiAfD_geo$latJIT <- jitter(antiAfD_geo$lat, factor = 300)
antiAfD_geo$longJIT <- jitter(antiAfD_geo$long, factor = 300)

antiAfD_geo$Großenkategorie = NA
antiAfD_geo$Großenkategorie = ifelse(antiAfD_geo$Mindestangabe.Teilnehmerzahl > 49999, 4, antiAfD_geo$Großenkategorie)
antiAfD_geo$Großenkategorie = ifelse(antiAfD_geo$Mindestangabe.Teilnehmerzahl < 49999 & antiAfD_geo$Mindestangabe.Teilnehmerzahl > 9999, 3, antiAfD_geo$Großenkategorie)
antiAfD_geo$Großenkategorie = ifelse(antiAfD_geo$Mindestangabe.Teilnehmerzahl < 9999 & antiAfD_geo$Mindestangabe.Teilnehmerzahl > 999, 2, antiAfD_geo$Großenkategorie)
antiAfD_geo$Großenkategorie = ifelse(antiAfD_geo$Mindestangabe.Teilnehmerzahl < 999, 1, antiAfD_geo$Großenkategorie)
antiAfD_geo$Großenkategorie = ifelse(is.na(antiAfD_geo$Großenkategorie), 1, antiAfD_geo$Großenkategorie)

antiAfD_geo$Großenkategorie_col = NA
antiAfD_geo$Großenkategorie_col = ifelse(antiAfD_geo$Großenkategorie == 4, "red", antiAfD_geo$Großenkategorie_col)
antiAfD_geo$Großenkategorie_col = ifelse(antiAfD_geo$Großenkategorie == 3, "orange", antiAfD_geo$Großenkategorie_col)
antiAfD_geo$Großenkategorie_col = ifelse(antiAfD_geo$Großenkategorie == 2, "yellow", antiAfD_geo$Großenkategorie_col)
antiAfD_geo$Großenkategorie_col = ifelse(antiAfD_geo$Großenkategorie == 1, "green", antiAfD_geo$Großenkategorie_col)

# ## Solution: do character conversion
# antiAfD$Ort <- iconv(antiAfD$Ort, from = "latin1", to = "UTF-8")
# library(readr)
# ## Then everything is fine
# write_csv2(antiAfD, file = "txt.csv")

# write.csv(antiAfD_geo, file = paste0("antiAfD_geo_demos", as.character(Sys.Date()), ".csv"))
# library(xlsx)
# xlsx::write.xlsx(antiAfD, file = paste0("antiAfD_geo_demos", as.character(Sys.Date(), ".xlsx")))


antiAfD_geo_sf <- antiAfD_geo %>% 
  st_as_sf(
    coords = c("longJIT", "latJIT"),
    crs = st_crs("EPSG:32632") # CRS for Germany
  )

de = ne_countries(scale = 50, returnclass = "sf") |>
  filter(admin == "Germany") 
plot(st_geometry(de), border = 'red')
plot(st_geometry(antiAfD_geo_sf), add = TRUE, cex = .5)

mapData <- ne_countries(scale = 10, continent = c("Europe"), returnclass = "sf")
de_states <- ne_states(country = "germany", returnclass = "sf")
plot(st_geometry(de_states), border = 'red')
plot(st_geometry(antiAfD_geo_sf), add = TRUE, cex = .5)
de_states <- dplyr::select(de_states, name, geometry)

st_crs(antiAfD_geo_sf) <- st_crs(de)

pp = st_geometry(antiAfD_geo_sf)
window = st_geometry(de)
# wt = as.ppp(c(window, pp)) # won’t work; have to project data first, e.g. to UTM zone 32N

crs = st_crs("EPSG:32632") # CRS for Germany
pp = st_transform(pp, crs)[!st_is_empty(pp)]
window = st_transform(window, crs)
wt = as.ppp(c(window, pp))

plot(density(wt, bw = "SJ"))
plot(window, add = TRUE)

# ggplot() +
#   stat_density_2d(data = antiAfD_geo_sf, 
#                   mapping = aes(x = purrr::map_dbl(geometry, ~.[1]),
#                                 y = purrr::map_dbl(geometry, ~.[2]),
#                                 fill = stat(density)),
#                   geom = 'tile',
#                   contour = FALSE,
#                   alpha = 0.8) +
#   geom_sf(data = de, fill = NA) +
#   # geom_sf(data = antiAfD_geo_sf, color = 'green') + 
#   scale_fill_viridis_c(option = 'magma', direction = -1) +
#   theme_test()

# Smooth points
density_spatstat <- density(wt, dimyx = 500)
# Convert density_spatstat into a stars object.
density_stars <- stars::st_as_stars(density_spatstat)
# Convert density_stars into an sf object
density_sf <- st_as_sf(density_stars) %>% st_set_crs(32632)


# overall map -------------------------------------------------------------

antiAfD_density <- ggplot() +
  geom_sf(data = density_sf, aes(fill = v), col = NA) + 
  # scale_fill_viridis_c(option = "magma") +
  # scale_fill_gradientn(colours = c("grey80", "grey10")) +
  geom_sf(data = st_boundary(de_states)) +
  geom_sf(data = subset(antiAfD_geo_sf, Großenkategorie %in% "1"), shape = 1,
          size = 1, colour = "green", stroke = 0.5, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf, Großenkategorie %in% "2"), shape = 1,
          size = 2, colour = "yellow", stroke = 0.5, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf, Großenkategorie %in% "3"), shape = 1,
          size = 3, colour = "orange", stroke = 1, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf, Großenkategorie %in% "4"), shape = 1,
          size = 5, colour = "red", stroke = 1, fill = NA) + 
  theme_void() + 
  theme(legend.position="none")
antiAfD_density

# png(file=paste0(as.character(Sys.Date()), "antiAfD_density", ".png"), width = 150, height = 150, units = 'mm', res = 600)
# plot(antiAfD_density)
# dev.off()

# sequence over time maps -------------------------------------------------

antiAfD_geo_sf_w2 <- subset(antiAfD_geo_sf, Woche == "02")
antiAfD_geo_sf_w3 <- subset(antiAfD_geo_sf, Woche == "03")
antiAfD_geo_sf_w4 <- subset(antiAfD_geo_sf, Woche == "04")
antiAfD_geo_sf_w5 <- subset(antiAfD_geo_sf, Woche == "05")
antiAfD_geo_sf_w6 <- subset(antiAfD_geo_sf, Woche == "06")
# antiAfD_geo_sf_w7

antiAfD_density_w2 <- ggplot() +
  geom_sf(data = de_states, fill = "grey50", col = NA) + 
  # geom_sf(data = de, aes(fill = "grey"), col = NA) + 
  # scale_fill_viridis_c(option = "magma") +
  # scale_fill_gradientn(colours = c("grey50", "grey10")) +
  geom_sf(data = st_boundary(de_states)) +
  geom_sf(data = subset(antiAfD_geo_sf_w2, Großenkategorie %in% "1"), shape = 1,
          size = 1, colour = "green", stroke = 0.5, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf_w2, Großenkategorie %in% "2"), shape = 1,
          size = 2, colour = "yellow", stroke = 0.5, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf_w2, Großenkategorie %in% "3"), shape = 1,
          size = 3, colour = "orange", stroke = 1, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf_w2, Großenkategorie %in% "4"), shape = 1,
          size = 5, colour = "red", stroke = 1, fill = NA) + 
  theme_void() + labs(title="11-14 January") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")
antiAfD_density_w2

antiAfD_density_w3 <- ggplot() +
  geom_sf(data = de_states, fill = "grey50", col = NA) + 
  # geom_sf(data = de, aes(fill = "grey"), col = NA) + 
  # scale_fill_viridis_c(option = "magma") +
  # scale_fill_gradientn(colours = c("grey50", "grey10")) +
  geom_sf(data = st_boundary(de_states)) +
  geom_sf(data = subset(antiAfD_geo_sf_w3, Großenkategorie %in% "1"), shape = 1,
          size = 1, colour = "green", stroke = 0.5, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf_w3, Großenkategorie %in% "2"), shape = 1,
          size = 2, colour = "yellow", stroke = 0.5, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf_w3, Großenkategorie %in% "3"), shape = 1,
          size = 3, colour = "orange", stroke = 1, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf_w3, Großenkategorie %in% "4"), shape = 1,
          size = 5, colour = "red", stroke = 1, fill = NA) + 
  theme_void() + labs(title="15-21 January") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")
antiAfD_density_w3

antiAfD_density_w4 <- ggplot() +
  geom_sf(data = de_states, fill = "grey50", col = NA) + 
  # geom_sf(data = de, aes(fill = "grey"), col = NA) + 
  # scale_fill_viridis_c(option = "magma") +
  # scale_fill_gradientn(colours = c("grey50", "grey10")) +
  geom_sf(data = st_boundary(de_states)) +
  geom_sf(data = subset(antiAfD_geo_sf_w4, Großenkategorie %in% "1"), shape = 1,
          size = 1, colour = "green", stroke = 0.5, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf_w4, Großenkategorie %in% "2"), shape = 1,
          size = 2, colour = "yellow", stroke = 0.5, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf_w4, Großenkategorie %in% "3"), shape = 1,
          size = 3, colour = "orange", stroke = 1, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf_w4, Großenkategorie %in% "4"), shape = 1,
          size = 5, colour = "red", stroke = 1, fill = NA) + 
  theme_void() + labs(title="22-28 January") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")
antiAfD_density_w4

antiAfD_density_w5 <- ggplot() +
  geom_sf(data = de_states, fill = "grey50", col = NA) + 
  # geom_sf(data = de, aes(fill = "grey"), col = NA) + 
  # scale_fill_viridis_c(option = "magma") +
  # scale_fill_gradientn(colours = c("grey50", "grey10")) +
  geom_sf(data = st_boundary(de_states)) +
  geom_sf(data = subset(antiAfD_geo_sf_w5, Großenkategorie %in% "1"), shape = 1,
          size = 1, colour = "green", stroke = 0.5, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf_w5, Großenkategorie %in% "2"), shape = 1,
          size = 2, colour = "yellow", stroke = 0.5, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf_w5, Großenkategorie %in% "3"), shape = 1,
          size = 3, colour = "orange", stroke = 1, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf_w5, Großenkategorie %in% "4"), shape = 1,
          size = 5, colour = "red", stroke = 1, fill = NA) + 
  theme_void() + labs(title="29 January-4 February") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")
antiAfD_density_w5

antiAfD_density_w6 <- ggplot() +
  geom_sf(data = de_states, fill = "grey50", col = NA) + 
  # geom_sf(data = de, aes(fill = "grey"), col = NA) + 
  # scale_fill_viridis_c(option = "magma") +
  # scale_fill_gradientn(colours = c("grey50", "grey10")) +
  geom_sf(data = st_boundary(de_states)) +
  geom_sf(data = subset(antiAfD_geo_sf_w6, Großenkategorie %in% "1"), shape = 1,
          size = 1, colour = "green", stroke = 0.5, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf_w6, Großenkategorie %in% "2"), shape = 1,
          size = 2, colour = "yellow", stroke = 0.5, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf_w6, Großenkategorie %in% "3"), shape = 1,
          size = 3, colour = "orange", stroke = 1, fill = NA) + 
  geom_sf(data = subset(antiAfD_geo_sf_w6, Großenkategorie %in% "4"), shape = 1,
          size = 5, colour = "red", stroke = 1, fill = NA) + 
  theme_void() + labs(title="5-11 February") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")
antiAfD_density_w6

library(cowplot)
maps.combined <- plot_grid(antiAfD_density_w2, antiAfD_density_w3,
                           antiAfD_density_w4, antiAfD_density_w5,
                           antiAfD_density_w6, nrow = 2, align = "h")
maps.combined

png(file=paste0("antiAfD_maps_combined", as.character(Sys.Date()), ".png"), width = 300, height = 150, units = 'mm', res = 600)
plot(maps.combined)
dev.off()


# density map -------------------------------------------------------------

antiAfD_density_no_points <- ggplot() +
  geom_sf(data = density_sf, aes(fill = v), col = NA) + 
  scale_fill_viridis_c(option = "magma") +
  # scale_fill_gradientn(colours = c("grey80", "grey10")) +
  geom_sf(data = st_boundary(de_states)) +
  # geom_sf(data = subset(antiAfD_geo_sf, Großenkategorie %in% "1"), shape = 1,
  #         size = 1, colour = "green", stroke = 0.5, fill = NA) + 
  # geom_sf(data = subset(antiAfD_geo_sf, Großenkategorie %in% "2"), shape = 1,
  #         size = 2, colour = "yellow", stroke = 0.5, fill = NA) + 
  # geom_sf(data = subset(antiAfD_geo_sf, Großenkategorie %in% "3"), shape = 1,
  #         size = 3, colour = "orange", stroke = 1, fill = NA) + 
  # geom_sf(data = subset(antiAfD_geo_sf, Großenkategorie %in% "4"), shape = 1,
  #         size = 5, colour = "red", stroke = 1, fill = NA) + 
  theme_void() + 
  theme(legend.position="none")
antiAfD_density_no_points

# interactive map ---------------------------------------------------------

# antiAfD_geo_sf %>% mapview(color = 'red', col.regions = "black")
# 
# # mapview(antiAfD_geo_sf, color = 'red', col.regions = "black")

# antiAfD_geo_sf %>% mapview(color = 'black', col.regions = antiAfD_geo_sf$Großenkategorie_col,
#                            label = "Ort", legend = F, cex = "Großenkategorie",
#                            map.types = c("CartoDB.DarkMatter","CartoDB.Positron"),
#                            popup = popupTable(antiAfD_geo_sf,
#                                               zcol = c("Ort","Datum",
#                                                        "Mindestangabe.Teilnehmerzahl",
#                                                        "Höchstangabe.Teilnehmerzahl",
#                                                        "Link.zum.Bericht")))


# antiAfD_geo_sf %>% mapview(zcol = "Woche", color = 'black', label = "Ort",
#                            legend = T, cex = "Großenkategorie",
#                            map.types = c("CartoDB.Positron","CartoDB.DarkMatter"),
#                            popup = popupTable(antiAfD_geo_sf,
#                                               zcol = c("Ort","Datum",
#                                                        "Mindestangabe.Teilnehmerzahl",
#                                                        "Höchstangabe.Teilnehmerzahl",
#                                                        "Link.zum.Bericht")))


mapview(antiAfD_geo_sf_w2, col.regions = "blue", label = "Ort",
        legend = T, layer.name = '11-14 January', cex = "Großenkategorie",
        map.types = c("CartoDB.DarkMatter","CartoDB.Positron"),
        popup = popupTable(antiAfD_geo_sf,
                           zcol = c("Ort","Datum",
                                    "Mindestangabe.Teilnehmerzahl",
                                    "Höchstangabe.Teilnehmerzahl",
                                    "Link.zum.Bericht"))) + 
  mapview(antiAfD_geo_sf_w3, col.regions = "green", label = "Ort",
          legend = T, layer.name = '15-21 January', cex = "Großenkategorie",
          map.types = c("CartoDB.DarkMatter","CartoDB.Positron"),
          popup = popupTable(antiAfD_geo_sf,
                             zcol = c("Ort","Datum",
                                      "Mindestangabe.Teilnehmerzahl",
                                      "Höchstangabe.Teilnehmerzahl",
                                      "Link.zum.Bericht"))) +
  mapview(antiAfD_geo_sf_w4, col.regions = "yellow", label = "Ort",
          legend = T, layer.name = '22-28 January', cex = "Großenkategorie",
          map.types = c("CartoDB.DarkMatter","CartoDB.Positron"),
          popup = popupTable(antiAfD_geo_sf,
                             zcol = c("Ort","Datum",
                                      "Mindestangabe.Teilnehmerzahl",
                                      "Höchstangabe.Teilnehmerzahl",
                                      "Link.zum.Bericht"))) +
  mapview(antiAfD_geo_sf_w5, col.regions = "orange", label = "Ort",
          legend = T, layer.name = '29 Jan.-4 Feb.', cex = "Großenkategorie",
          map.types = c("CartoDB.DarkMatter","CartoDB.Positron"),
          popup = popupTable(antiAfD_geo_sf,
                             zcol = c("Ort","Datum",
                                      "Mindestangabe.Teilnehmerzahl",
                                      "Höchstangabe.Teilnehmerzahl",
                                      "Link.zum.Bericht"))) +
  mapview(antiAfD_geo_sf_w6, col.regions = "red", label = "Ort",
          legend = T, layer.name = '5-11 February', cex = "Großenkategorie",
          map.types = c("CartoDB.DarkMatter","CartoDB.Positron"),
          popup = popupTable(antiAfD_geo_sf,
                             zcol = c("Ort","Datum",
                                      "Mindestangabe.Teilnehmerzahl",
                                      "Höchstangabe.Teilnehmerzahl",
                                      "Link.zum.Bericht")))




# scratch -----------------------------------------------------------------


# Load data
library(readxl)
preo_loc <- read_excel("preo_data.xlsx")

# BANdf <- read_excel("de_reos_latest.xlsx", sheet = "REOs_trimmed")

preo_loc_reverse <- preo_loc %>% 
  tidygeocoder::reverse_geocode(
    lat = LAT,
    long = LON,
    method = "osm",
    full_results = TRUE
  )

preo_loc_add <- subset(preo_loc_reverse, select = c(NAME, COUNTRY, P_YEAR, LOCAL, REGION, TYPE, EXTENT, city, state))


write.csv(preo_loc_add, file = "preo_loc_add.csv")



