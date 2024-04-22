library(data.table)
library(dplyr)
library(dygraphs)
library(leafem)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(plotly)
library(sf)
library(shiny)
library(shinyWidgets)
library(stars)
library(stringr)
library(tidyr)
library(xts)

# # merge watersheds
# 
# pilot1 <- st_read("data/boundaries/Pilot watersheds/Babile-Gutu CW.shp") %>%
#   st_transform(4326) %>%
#   select(geometry) %>%
#   mutate(NAME = "Babile Gutu",
#          TYPE = "Pilot",
#          AGRO = "Lowland")
# pilot2 <- st_read("data/boundaries/Pilot watersheds/Jarso-Worra-Chella CW.shp") %>%
#   st_transform(4326) %>%
#   select(geometry) %>%
#   mutate(NAME = "Jarso Worra Chella",
#          TYPE = "Pilot",
#          AGRO = "Highland")
# pilot3 <- st_read("data/boundaries/Pilot watersheds/Meta-Hawi Gudina CW.shp") %>%
#   st_transform(4326) %>%
#   select(geometry) %>%
#   mutate(NAME = "Meta Hawi Gudina",
#          TYPE = "Pilot",
#          AGRO = "Midland")
# pilot4 <- st_read("data/boundaries/Pilot watersheds/Midhega Tolla- Umer Bula CW.shp") %>%
#   st_transform(4326) %>%
#   select(geometry) %>%
#   mutate(NAME = "Midega Tola Umer Bula",
#          TYPE = "Pilot",
#          AGRO = "Lowland")
# control1 <- st_read("data/boundaries/Control Sites for pilots/Finxabas MWS.shp") %>%
#   st_transform(4326) %>%
#   select(geometry) %>%
#   mutate(NAME = "Meta Finxabas",
#          TYPE = "Control",
#          AGRO = "Midland")
# control2 <- st_read("data/boundaries/Control Sites for pilots/QUUFA MWS.shp") %>%
#   st_transform(4326) %>%
#   select(geometry) %>%
#   mutate(NAME = "Midega Tola Quufa",
#          TYPE = "Control",
#          AGRO = "Lowland")
# 
# ws <- rbind(pilot1, pilot2, pilot3, pilot4, control1, control2)
# 
# plot(st_geometry(ws))
# 
# st_write(ws, "data/ws-pilot-control.shp", driver = "ESRI Shapefile")

ws <- st_read("data/ws-pilot-control.shp")

# lulc <- fread("data/rfsa-lulc.csv")
# lulc <- lulc[!is.na(lulc$X), ]
# write.csv(lulc, "data/rfsa-lulc.csv", row.names = FALSE)
# lulc <- fread("data/rfsa-lulc.csv")
# saveRDS(lulc, file = "data/rfsa-lulc.rds")
# lulc <- lulc %>%
#   filter(MONTH %in% c(7, 10, 1, 4))
# lulc$SEASON <- NA
# lulc$SEASON[lulc$MONTH == 7] <- "Kiremt or Meher (Summer)"
# lulc$SEASON[lulc$MONTH == 10] <- "Belg (Autumn)"
# lulc$SEASON[lulc$MONTH == 1] <- "Bega (Winter)"
# lulc$SEASON[lulc$MONTH == 4] <- "Tseday (Spring)"
# write.csv(lulc, "data/rfsa-lulc-season.csv", row.names = FALSE)
lulc <- fread("data/rfsa-lulc-season.csv")

# ndvi <- fread("data/rfsa-ndvi.csv")
# saveRDS(ndvi, file = "data/rfsa-ndvi.rds")
# ndvi$SEASON <- NA
# ndvi$SEASON[ndvi$MONTH %in% c(6, 7, 8)] <- "Kiremt or Meher (Summer)"
# ndvi$SEASON[ndvi$MONTH %in% c(9, 10, 11)] <- "Belg (Autumn)"
# ndvi$SEASON[ndvi$MONTH %in% c(12, 1, 2)] <- "Bega (Winter)"
# ndvi$SEASON[ndvi$MONTH %in% c(3, 4, 5)] <- "Tseday (Spring)"
# sort(unique(ndvi$YEAR))
# ndvi$YEAR[ndvi$MONTH == 12] <- ndvi$YEAR[ndvi$MONTH == 12] + 1
# sort(unique(ndvi$YEAR))
# ndvi_season <- ndvi %>%
#   mutate(X_Y = paste(X, Y, sep = "_")) %>%
#   group_by(NAME, YEAR, SEASON, X_Y) %>%
#   summarise(NDVI = mean(NDVI, na.rm = TRUE)) %>%
#   separate(X_Y, into = c("X", "Y"), sep = "_")
# type_agro <- ndvi %>%
#   dplyr::select(NAME, TYPE, AGRO) %>%
#   distinct(NAME, .keep_all = TRUE)
# ndvi_season <- ndvi_season %>%
#   left_join(type_agro, by = "NAME") %>%
#   dplyr::select(NAME, TYPE, AGRO, YEAR, SEASON, NDVI, X, Y)
# write.csv(ndvi_season, "data/rfsa-ndvi-season.csv", row.names = FALSE)
ndvi <- fread("data/rfsa-ndvi-season.csv")

# ndvi4table <- fread("data/rfsa-ndvi.csv")
# ndvi4table <- ndvi4table %>%
#   group_by(NAME, YEAR, MONTH) %>%
#   summarise(NDVI = mean(NDVI, na.rm = TRUE))
# ndvi4table$SEASON <- NA
# ndvi4table$SEASON[ndvi4table$MONTH %in% c(6, 7, 8)] <- "Kiremt or Meher (Summer)"
# ndvi4table$SEASON[ndvi4table$MONTH %in% c(9, 10, 11)] <- "Belg (Autumn)"
# ndvi4table$SEASON[ndvi4table$MONTH %in% c(12, 1, 2)] <- "Bega (Winter)"
# ndvi4table$SEASON[ndvi4table$MONTH %in% c(3, 4, 5)] <- "Tseday (Spring)"
# ndvi4table$MONTH_NAME <- month.name[ndvi4table$MONTH]
# ndvi4table$NDVI <- round(ndvi4table$NDVI, 2)
# write.csv(ndvi4table, "data/rfsa-ndvi-table.csv", row.names = FALSE)
ndvi4table <- fread("data/rfsa-ndvi-table.csv")

# precip <- fread("data/rfsa-precip.csv")
# precip$SEASON <- NA
# precip$SEASON[precip$MONTH %in% c(6, 7, 8)] <- "Kiremt or Meher (Summer)"
# precip$SEASON[precip$MONTH %in% c(9, 10, 11)] <- "Belg (Autumn)"
# precip$SEASON[precip$MONTH %in% c(12, 1, 2)] <- "Bega (Winter)"
# precip$SEASON[precip$MONTH %in% c(3, 4, 5)] <- "Tseday (Spring)"
# precip$MONTH_NAME <- month.name[precip$MONTH]
# precip$P_MM <- round(precip$P_MM, 2)
# write.csv(precip, "data/rfsa-precip.csv", row.names = FALSE)
precip <- fread("data/rfsa-precip.csv")
