library(data.table)
library(dplyr)
library(leafem)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(sf)
library(shiny)
library(shinyWidgets)
library(stars)
library(stringr)
library(tidyr)

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

lulc <- fread("data/babile-gutu-lulc.csv")

ndvi <- fread("data/babile-gutu-ndvi.csv")