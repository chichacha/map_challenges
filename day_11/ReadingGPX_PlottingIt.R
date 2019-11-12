
## Elevation....  
### gpx file

library(tidyverse)
library(sf)
library(leaflet)
library(osmdata)
library(ggthemes)
library(ggforce)
library(lubridate)
library(patchwork)


## Check what types of layers exists in GPX? 
st_layers("day_11/whistler.gpx")

a_bike_ride <- st_read("day_11/whistler.gpx", 
                       layer="tracks")

## I want elevation data in the file! so I think below is what i want. 
a_bike_ride_dtl <- st_read("day_11/whistler.gpx", 
                       layer="track_points")


## How to plot using leaflet
leaflet(a_bike_ride) %>%
  addProviderTiles(providers$OpenTopoMap) %>%
  addPolylines(color = "tomato", weight = 3, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))


my_bbox <- st_bbox(a_bike_ride)
base_map <- osmdata::opq(my_bbox) %>%
  add_osm_feature(key="highway") %>%
  osmdata_sf()

base_map2 <- osmdata::opq(my_bbox) %>%
  add_osm_feature(key="aerialway") %>%
  osmdata_sf()

base_map$osm_lines
base_map2$osm_lines

ggplot() +
  geom_sf(data = base_map$osm_lines, color="#000000ae", size=0.1) +
  geom_sf(data = base_map2$osm_lines, color="#000000ae", size=0.5, linetype=3) +
  geom_sf(aes(fill=ele), size=2, data=a_bike_ride_dtl, shape=21, color="#ffffff00") +
  geom_sf_text(data = base_map2$osm_lines %>% filter(aerialway=="gondola"), 
                color="#000000ae", aes(label=str_remove(name,"Gondola")), size=2,
               nudge_y=0.001) +
  scale_fill_gradient_tableau("Red-Gold")+
  theme_map()

d <-a_bike_ride_dtl %>%
  ggplot(aes(x=time,y=ele)) +
  geom_area(fill="#00000020") +
  geom_col(aes(fill=ele), size=0.001, color="#00000000") +
  theme_tufte(base_family="Roboto Condensed") +
  scale_fill_gradient_tableau("Red-Gold") +
  labs(title="Time x Elevation", x="time", y="elevation") +
  scale_x_datetime(breaks=NULL)




## gpx - can i calculate the speed, slope since i know xyz + time?

a_bike_ride_dtl <- a_bike_ride_dtl %>%
  mutate(lng = map_dbl(map(geometry, st_coordinates),1),
         lat = map_dbl(map(geometry, st_coordinates),2)) 

a_bike_ride_dtl <- a_bike_ride_dtl %>% 
  mutate(time.s= as.numeric(as.period(time - min(time), unit = "seconds")),
         idx = row_number())

a <-a_bike_ride_dtl %>%
  ggplot(aes(x=lng, y=lat)) +
  geom_path(aes(color=ele), size=1.5) +
  scale_color_gradient_tableau("Red-Gold", guide="none") +
  theme_map()+
  coord_map() +
  theme(legend.position="right")

b <-a_bike_ride_dtl %>%
  ggplot(aes(x=lng, y=lat)) +
  geom_delaunay_segment(size=0.1,aes(color=ele)) +
  scale_color_gradient_tableau("Red-Gold", guide="none") +
  theme_map()+
  coord_map() +
  theme(legend.position="right")

c <-a_bike_ride_dtl %>%
  ggplot(aes(x=lng, y=lat)) +
  geom_voronoi_segment(size=0.1, aes(color=ele)) +
  scale_color_gradient_tableau("Red-Gold", guide="none") +
  theme_map()+
  coord_map() +
  theme(legend.position="right")

## Using patchwork packages to put chart on one page! 
# https://github.com/thomasp85/patchwork
(a | b | c) / d



