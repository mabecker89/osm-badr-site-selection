#-----------------------------------------------------------------------------------------------------------------------

# Load packages
library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(readr)
library(leaflet)
library(leaflet.extras)
library(janitor)

root <- "G:/Shared drives/ABMI Camera Mammals/projects/osm-badr-site-selection/"

#-----------------------------------------------------------------------------------------------------------------------

# Load data

# Alberta
sf_ab <- st_read(paste0(root, "spatial/ab_prov.shp")) %>% st_transform(4326)

# Landscape Units (LUs)
sf_lu <- st_read(paste0(root, "spatial/LU_2021_2022.shp")) %>%
  st_transform(4326) %>%
  # Clean
  clean_names() %>%
  select(lu, label, year, deciles, shape_area)

# Joint Environmental Monitoring sites (JEMs) w/ 1000m buffer
sf_jem <- st_read(paste0(root, "spatial/JEMs_2021_2022_1000m_buffer.shp")) %>%
  st_transform(4326) %>%
  # Clean
  clean_names() %>%
  mutate(site_name = ifelse(is.na(site_name), site_name_3, site_name),
         lu = str_extract(site_name, "[^-]+"),
         veg_type = case_when(
           str_detect(site_name, "DM") ~ "DecidMix40",
           str_detect(site_name, "TL") ~ "TreedLow20",
           TRUE ~ type),
         hf_treatment = case_when(
           str_detect(site_name, "-C") ~ "Low Disturbance/Reference",
           str_detect(site_name, "-Rd") ~ "Roads",
           str_detect(site_name, "-SL") ~ "Dense Linear Features",
           str_detect(site_name, "-HW") ~ "High Activity Insitu Well Pads",
           str_detect(site_name, "-LW") ~ "Low Activity Well Pads",
           str_detect(site_name, "-PM") ~ "Plant/Mine Buffer",
           str_detect(site_name, "-PI") ~ "Pre-Insitu",
           TRUE ~ treatment),
         year = ifelse(lu == "2" | lu == "3" | lu == "4" | lu == "8", "2021", "2022")) %>%
  select(lu, year, veg_type, hf_treatment, site_name)

# Camera locations from 2021 sampling
sf_cam_loc_2021 <- st_read(paste0(root, "spatial/osm_camera_locations_2021.shp")) %>%
  st_transform(4326) %>%
  clean_names() %>%
  select(site_id, year, project)

# Camera locations selected in LU 1 last year - to use this year.
sf_cam_loc_2022_lu1 <- st_read(paste0(root, "original-spatial/2022_23-cam-aru-20211008T181256Z-001/2022_23-cam-aru/tbm_backup-cam-aru_2021_v2_projected.shp")) %>%
  st_transform(4326) %>%
  clean_names() %>%
  rename(site_id = cam_aru) %>%
  mutate(year = 2022,
         habitat = case_when(
           habitat == "decid" ~ "decidmix40",
           habitat == "treedlow" ~ "treedlow20",
           TRUE ~ habitat),
         treatment = case_when(
           treatment == "road buffer" ~ "roads",
           treatment == "dense linear" ~ "dense linear features",
           treatment == "high activity insitu" ~ "high activity insitu well pads",
           treatment == "reference" ~ "low disturbance/reference",
           TRUE ~ treatment
         )) %>%
  select(-objectid)

# New proposed 2022 camera locations
sf_cam_loc_2022 <- read_csv(paste0(root, "proposed_sites_2022_v1.csv")) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  # Join in 'old' LU1 sites (chosen as backup last year)
  bind_rows(sf_cam_loc_2022_lu1)

# Decidmix40 - all HF treatments
sf_decidmix_all <- st_read(paste0(root, "spatial/decidmix40_HFtreatment_intersect_JEMbufferclip.shp")) %>%
  st_transform(4326) %>%
  clean_names() %>%
  select(type, treatment)

# Treedlow20 - all HF treatments
sf_treedlow_all <- st_read(paste0(root, "spatial/treedlow20_HFtreatment_intersect_JEMbufferclip.shp")) %>%
  st_transform(4326) %>%
  clean_names() %>%
  select(type, treatment)

# Palettes

pal_decid <- colorFactor(
  palette = "Dark2",
  domain = sf_decidmix_all$Treatment
)
pal_treedlow <-colorFactor(
  palette = "Dark2",
  domain = sf_treedlow_all$Treatment
)

# Icons
cam_abmi_2021 <- makeAwesomeIcon(
  icon = "camera",
  iconColor = "white",
  library = "ion",
  markerColor = "lightgray"
)

cam_abmi_2022 <- makeAwesomeIcon(
  icon = "camera",
  iconColor = "black",
  library = "ion",
  markerColor = "white"
)

#-----------------------------------------------------------------------------------------------------------------------

# Create interactive map:

map <- sf_ab %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery",
                   group = "Satellite Imagery") %>%
  addFullscreenControl() %>%
  addResetMapButton() %>%
  addScaleBar(position = "bottomleft",
              options = scaleBarOptions(imperial = FALSE)) %>%
  addMeasure(position = "topleft",
             primaryLengthUnit = "meters",
             primaryAreaUnit = "sqmeters",
             secondaryLengthUnit = "kilometers",
             secondaryAreaUnit = "sqkilometers",
             activeColor = "cornflowerblue",
             completedColor = "cornflowerblue") %>%
  addDrawToolbar(position = "topleft",
                 polylineOptions = FALSE,
                 polygonOptions = FALSE,
                 circleOptions = FALSE,
                 rectangleOptions = FALSE,
                 circleMarkerOptions = FALSE,
                 markerOptions = drawMarkerOptions(repeatMode = TRUE, markerIcon = cam_abmi_2022),
                 editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)) %>%
  addMapPane(name = "Boundaries LU", zIndex = 410) %>%
  addMapPane(name = "Boundaries JEM", zIndex = 415) %>%
  addMapPane(name = "Habitat Treatment Data", zIndex = 420) %>%
  addMapPane(name = "2021 Camera Sites", zIndex = 430) %>%

  # Add polygon layers:

  # Alberta provice polygon
  addPolygons(color = "#070707", weight = 1, smoothFactor = 0.2, opacity = 2, fill = FALSE,
              group = "None", options = leafletOptions(pane = "Boundaries LU")) %>%

  # Landscape Units
  addPolygons(data = sf_lu, color = "white", weight = 1, smoothFactor = 0.2, opacity = 1,
              fillOpacity = 0.05, group = "Landscape Units", options = leafletOptions(pane = "Boundaries LU"),
              popup = paste("Treatment: ", "<b>", sf_lu$label, "</b>",
                            "<br>",
                            "Sampling Year: ", "<b>", sf_lu$year, "</b>",
                            "<br>",
                            "LU Code: ", "<b>", sf_lu$lu, "</b>")) %>%

  # JEM sites + 1500m buffer
  addPolygons(data = sf_jem, color = "#6baed6", fillColor = "black",
              weight = 1, smoothFactor = 0.2, opacity = 1, fillOpacity = 0.1, group = "JEM Sites",
              options = leafletOptions(pane = "Boundaries JEM"),
              popup = paste("Habitat Target: ", "<b>", sf_jem$veg_type, "</b>", "<br>",
                            "<br>",
                            "Treatment Target: ", "<b>", sf_jem$hf_treatment, "</b>", "<br>",
                            "<br>",
                            "JEM Site Code: ", "<b>", sf_jem$site_name, "</b>"),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%

  # Decidmix40 + HF treatments layer
  addPolygons(data = sf_decidmix_all, color = "grey50", fillColor = ~ pal_decid(treatment),
              weight = 1, smoothFactor = 0.2, opacity = 1, fillOpacity = 0.5, group = "Habitat: DecidMix40+",
              options = leafletOptions(pane = "Habitat Treatment Data"),
              popup = paste("Treatment: ", "<b>", sf_decidmix_all$treatment, "</b>"),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%

  # Treedlow + HF treatments layer
  addPolygons(data = sf_treedlow_all, color = "grey50", fillColor = ~ pal_treedlow(treatment),
              weight = 1, smoothFactor = 0.2, opacity = 1, fillOpacity = 0.5, group = "Habitat: TreedLow20+",
              options = leafletOptions(pane = "Habitat Treatment Data"),
              popup = paste("Treatment: ", "<b>", sf_treedlow_all$treatment, "</b>"),
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%

  # 2021 Camera Sites
  addAwesomeMarkers(data = sf_cam_loc_2021,
                    icon = cam_abmi_2021,
                    group = "2021 Camera Sites",
                    options = leafletOptions(pane = "2021 Camera Sites"),
                    popup = paste("ID: ", "<b>", sf_cam_loc_2021$site_id, "</b>",
                                  "<br>", "<br>",
                                  "Project:", "<br>",
                                  sf_cam_loc_2021$project)) %>%

  # Proposed 2022 Camera Sites
  addAwesomeMarkers(data = sf_cam_loc_2022,
                    icon = cam_abmi_2022,
                    group = "2022 Camera Sites (proposed)",
                    options = leafletOptions(pane = "2021 Camera Sites"),
                    popup = paste("ID: ", "<b>", sf_cam_loc_2022$site_id, "</b>",
                                  "<br>", "<br>",
                                  "Treatment: ", "<b>", sf_cam_loc_2022$treatment, "</b>", "<br>",
                                  "<br>",
                                  "Notes: ", sf_cam_loc_2022$notes)) %>%

  # Layers control
  addLayersControl(overlayGroups = c("Landscape Units", "JEM Sites", "2021 Camera Sites", "2022 Camera Sites (proposed)",
                                     "Satellite Imagery"),
                   baseGroups = c("None", "Habitat: DecidMix40+", "Habitat: TreedLow20+"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "topright") %>%

  # Legend
  addLegend(data = sf_decidmix_all, position = "topright", pal = pal_decid,
            values = ~ treatment,
            opacity = 1) %>%

  hideGroup(c("2021 Camera Sites", "2022 Camera Sites (proposed)"))

map

#-----------------------------------------------------------------------------------------------------------------------

# Save map:

htmlwidgets::saveWidget(map, file = here("docs/proposed_camaru_site_map_2022.html"), selfcontained = FALSE)












































