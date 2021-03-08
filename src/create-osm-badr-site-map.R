#-----------------------------------------------------------------------------------------------------------------------

# Load packages
library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(leaflet)
library(leaflet.extras)
library(here)

root <- "G:/Shared drives/ABMI Camera Mammals/projects/OSM-BADR/"

#-----------------------------------------------------------------------------------------------------------------------

# Load data:

# Alberta
sf_ab <- st_read("G:/Shared drives/ABMI Camera Mammals/data/spatial/ab_prov.shp") %>%
  st_transform(4326)
# Landscape Units (2021)
sf_lu <- st_read(paste0(root, "official-layers/tbm_lu_2021.shp")) %>%
  st_transform(4326)
# JEM points
sf_jem <- st_read(paste0(root, "official-layers/ABMI_OSM_Feb22_NAD8310TM.shp")) %>%
  st_transform(4326) %>%
  filter(Site.Type == "TBM")
# JEMs w/ 1000m buffer
sf_jem_buffer <- st_read(paste0(root, "official-layers/tbm_jem_2021_1000m_buffer.shp")) %>%
  st_transform(4326) %>%
  select(Site_Name) %>%
  separate(Site_Name, sep = "-", into = c("LU", "Habitat", "Treatment", "Extra"), remove = FALSE) %>%
  mutate(Habitat = case_when(
    Habitat == "DM" ~ "DecidMix40+",
    Habitat == "TL" ~ "TreedLow20+"
  )) %>%
  mutate(Treatment = case_when(
    Treatment == "C" ~ "Reference",
    Treatment == "Rd" ~ "Road Buffer",
    Treatment == "SL" ~ "Dense Linear Features",
    Treatment == "HW" ~ "High Activity Insitu Well Pads",
    Treatment == "LW" ~ "Low Activity Well Pads",
    Treatment == "PM" ~ "Plant/Mine Buffer",
    Treatment == "PI" ~ "Pre-Insitu"
  ))
# Jake's Stations
sf_jake <- st_read(paste0(root, "official-layers/tbm_jake_2021_grouping.shp")) %>%
  st_transform(4326) %>%
  mutate(groups = factor(groups, levels = c("0-3", "3-6", "+6", "Not considered")))
# Decidmix all treatments
sf_decidmix_all <- st_read(paste0(root, "official-layers/tbm_decidmixed40_jem1500clip_alltreatments.shp")) %>%
  st_transform(4326) %>%
  mutate(Treatment = ifelse(Treatment == "Roads", "Road Buffer", Treatment),
         Treatment = ifelse(Treatment == "Low Disturbance/Reference", "Reference", Treatment))
# Treedlow all treatments
sf_treedlow_all <- st_read(paste0(root, "official-layers/tbm_treedlow20_jem1500clip_alltreatments.shp")) %>%
  st_transform(4326) %>%
  mutate(Treatment = ifelse(Treatment == "Roads", "Road Buffer", Treatment),
         Treatment = ifelse(Treatment == "Low Disturbance/Reference", "Reference", Treatment))
# Proposed Cam/ARU sites
sf_cam_aru <- read_csv(paste0(root, "camera_site_selection_2021.csv")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# Palettes
pal_decid <- colorFactor(
  palette = "Dark2",
  domain = sf_decidmix_all$Treatment
)
pal_treedlow <-colorFactor(
  palette = "Dark2",
  domain = sf_treedlow_all$Treatment
)
pal_jake <- colorFactor(palette = c("#fff7bc", "#fec44f", "#d95f0e", "#808080"),
                        reverse = FALSE,
                        domain = sf_jake$groups)
# Icons
cam_abmi <- makeAwesomeIcon(
  icon = "camera",
  iconColor = "black",
  library = "ion",
  markerColor = "white"
)

#-----------------------------------------------------------------------------------------------------------------------

# Create map:

map <- sf_ab %>%
  #st_transform(4326) %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite Imagery") %>%
  addFullscreenControl() %>%
  addResetMapButton() %>%
  addScaleBar(position = "bottomleft",
              options = scaleBarOptions(imperial = FALSE)) %>%
  addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters",
             secondaryLengthUnit = "kilometers", secondaryAreaUnit = "sqkilometers",
             activeColor = "cornflowerblue", completedColor = "cornflowerblue",
             position = "topleft") %>%
  addDrawToolbar(position = "topleft", polylineOptions = FALSE, polygonOptions = FALSE,
                 circleOptions = FALSE, rectangleOptions = FALSE, circleMarkerOptions = FALSE,
                 markerOptions = drawMarkerOptions(repeatMode = TRUE, markerIcon = cam_abmi),
                 editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)) %>%
  addMapPane(name = "Boundaries LU", zIndex = 410) %>%
  addMapPane(name = "Boundaries JEM", zIndex = 415) %>%
  addMapPane(name = "Habitat Treatment Data", zIndex = 420) %>%
  addMapPane(name = "Camera Sites", zIndex = 430) %>%

  # Add polygon layers

  addPolygons(color = "#070707", weight = 1, smoothFactor = 0.2, opacity = 2, fill = FALSE,
              group = "None", options = leafletOptions(pane = "Boundaries LU")) %>%

  addPolygons(data = sf_lu, color = "white", weight = 1, smoothFactor = 0.2, opacity = 1,
              fillOpacity = 0.05, group = "Landscape Units (2021)", options = leafletOptions(pane = "Boundaries LU"),
              popup = paste("Treatment: ", "<b>", sf_lu$LUTreatmnt, "</b>",
                            "<br>", "<br>",
                            "LU Code: ", "<b>", sf_lu$LU, "</b>")) %>%

  addPolygons(data = sf_jem_buffer, color = "#6baed6", fillColor = "black",
              weight = 1, smoothFactor = 0.2, opacity = 1, fillOpacity = 0.1, group = "JEM Sites",
              options = leafletOptions(pane = "Boundaries JEM"),
              popup = paste("Habitat Target: ", "<b>", sf_jem_buffer$Habitat, "</b>", "<br>",
                            "<br>",
                            "Treatment Target: ", "<b>", sf_jem_buffer$Treatment, "</b>", "<br>",
                            "<br>",
                            "JEM Site Code: ", "<b>", sf_jem_buffer$Site_Name, "</b>"),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%

  addPolygons(data = sf_decidmix_all, color = "grey50", fillColor = ~ pal_decid(Treatment),
              weight = 1, smoothFactor = 0.2, opacity = 1, fillOpacity = 0.5, group = "Habitat: DecidMix40+",
              options = leafletOptions(pane = "Habitat Treatment Data"),
              popup = paste("Treatment: ", "<b>", sf_decidmix_all$Treatment, "</b>"),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%

  addPolygons(data = sf_treedlow_all, color = "grey50", fillColor = ~ pal_treedlow(Treatment),
              weight = 1, smoothFactor = 0.2, opacity = 1, fillOpacity = 0.5, group = "Habitat: TreedLow20+",
              options = leafletOptions(pane = "Habitat Treatment Data"),
              popup = paste("Treatment: ", "<b>", sf_treedlow_all$Treatment, "</b>"),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%

  addCircleMarkers(data = sf_jake,
                   color = ~ pal_jake(groups), stroke = FALSE, fillOpacity = 1,
                   radius = 6, group = "Jake Camera Sites",
                   options = leafletOptions(pane = "Camera Sites"),
                   popup = paste("Station: ", "<b>", sf_jake$Station, "</b>")) %>%

  addAwesomeMarkers(data = sf_cam_aru,
                    icon = cam_abmi,
                    group = "ABMI CAM/ARU Sites (Proposed)",
                    options = leafletOptions(pane = "Camera Sites"),
                    popup = paste("ID: ", "<b>", sf_cam_aru$camera_code, "</b>",
                                 "<br>", "<br>",
                                 "Comment:", "<br>",
                                 sf_cam_aru$notes)) %>%

  # Layers control
  addLayersControl(overlayGroups = c("Landscape Units (2021)", "JEM Sites", "ABMI CAM/ARU Sites (Proposed)",
                                     "Jake Camera Sites", "Satellite Imagery"),
                   baseGroups = c("None", "Habitat: DecidMix40+", "Habitat: TreedLow20+"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topright") %>%

  # Legend
  addLegend(data = sf_decidmix_all, position = "topright", pal = pal_decid,
            values = ~ Treatment,
            opacity = 1) %>%

  addLegend(data = sf_jake, position = "bottomright", pal = pal_jake, values = ~ groups, opacity = 1,
            title = "% Surrounding Area of LF",
            group = "Jake Camera Sites") %>%

  hideGroup(c("Jake Camera Sites", "ABMI CAM/ARU Sites (Proposed)"))

map

#-----------------------------------------------------------------------------------------------------------------------

# Save map:

htmlwidgets::saveWidget(map, file = here("docs/proposed_camaru_site_map.html"), selfcontained = FALSE)



















