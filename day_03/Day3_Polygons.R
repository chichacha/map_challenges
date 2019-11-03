## Day 3 - Polygons


############ Packages I Need
library(osmdata)
library(tidyverse)
library(visdat)
library(skimr)
library(ggthemes)
library(janitor)
library(sf)
library(wordcloud2)


############ Obtaining Dataset with Polygon 
## GeoJson with Polygon  
# https://opendata.vancouver.ca/explore/dataset/local-area-boundary/download/?format=geojson&timezone=America/Los_Angeles

url <- "https://opendata.vancouver.ca/explore/dataset/local-area-boundary/download/?format=geojson&timezone=America/Los_Angeles"
download.file(url, "day_03/data/local-area-boundary.json")


########### Reading Dataset 
van_boundary <- read_sf("day_03/data/local-area-boundary.json")

## There are 22 neighbourhood 
van_boundary 
# mapid, name, geometry = polygon, geo_point_2d = centeroid?



########### Prepping Colour Palettes
## Sticking to same colour themes I've been using... 
col_pal <- c("#F44336", "#E91E63", "#9C27B0", "#673AB7", "#3F51B5", "#2196F3", "#03A9F4", "#00BCD4", "#009688", "#4CAF50", "#8BC34A", "#CDDC39", "#FFEB3B", "#FFC107", "#FF9800", "#FF5722", "#795548", "#9E9E9E", "#607D8B") 
col_pal_40 <- colorRampPalette(col_pal)(40)


########### Prepare Basemap
van_bbox <- st_bbox(van_boundary)

base_map <- opq(bbox=van_bbox) %>% ## bild an overpass query
  add_osm_feature(key="highway") %>%  ## highway will get road. 
  osmdata_sf() %>%
  osm_poly2line() ## convert polygon shaped highway to line

road_base <-base_map$osm_lines %>% select(highway) 

building_type <-osmdata::available_tags("building")
base_map_2 <- opq(bbox=van_bbox) %>% ## bild an overpass query
  add_osm_feature(key="building") %>%  
  osmdata_sf() 



van_buildings <-base_map_2$osm_polygons %>% select("building") %>%
  mutate(building=ifelse(building %in% building_type,building,"unknown"))

van_buildings %>% as_tibble() %>% 
  count(building) %>%
  mutate(n=log(n+1)) %>% 
  wordcloud2(rotateRatio=0, 
             fontFamily="Roboto Condensed", 
             color=col_pal_40,
             backgroundColor = "gray8")

van_buildings
van_boundary


## Land Use 
base_map_3 <- opq(bbox=van_bbox) %>% ## bild an overpass query
  add_osm_feature(key="landuse") %>%  
  osmdata_sf() 

base_map_3$osm_polygons %>% as_tibble() %>% count(landuse, sort=T)
base_map_3$osm_polygons %>% select(name, landuse) %>% plot()



############ Just exploring...
all_van <-van_buildings %>%
  ggplot() +
  geom_sf(data=road_base, size=0.1, color="#ffffff60") +
  geom_sf(aes(fill=building, color=building), size=0.1) +
  scale_fill_manual(values=col_pal_40, guide="none") +
  scale_color_manual(values=col_pal_40, guide="none") +
  theme_map() +
  geom_sf(data=van_boundary, fill="00000000") +
  geom_sf_text(data=van_boundary, aes(label=name), 
               family="Roboto Condensed", color="#ffffffde", size=4) +
  theme(plot.background=element_rect("gray8")) 

all_van
ggsave("day_03/output/BuildingTypes.svg", width=16, height=9)
ggsave("day_03/output/BuildingTypes.png", width=16, height=9)
xopen::xopen("day_03/output/BuildingTypes.svg", app="google chrome")


## zoom-in?
downtown_bbox <-van_boundary %>% filter(name %in% c("Downtown","West End")) %>% st_bbox()

all_van +
  coord_sf(xlim=downtown_bbox[c(1,3)], ylim=downtown_bbox[c(2,4)])

ggsave("day_03/output/BuildingTypes_Zoomed.svg", width=16, height=9)
xopen::xopen("day_03/output/BuildingTypes_Zoomed.svg", app="google chrome")





