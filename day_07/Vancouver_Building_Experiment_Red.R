library(tidyverse)
library(sf)
library(ggthemes) ## I love the themes for ggplot2
library(rayshader) ## because I've been wanting to just use it
library(patchwork)


########### Reading Dataset on Vancouver Neighbourhood Boundary 
van_boundary <- read_sf("day_03/data/local-area-boundary.json")

## There are 22 neighbourhood 
van_boundary 
# mapid, name, geometry = polygon, geo_point_2d = centeroid?


########### I just want bounding box for downtown vancouver
van_bbox <- st_bbox(van_boundary %>% filter(str_detect(name,"Downtown")))


#######  Vancouver Open Data
# https://opendata.vancouver.ca/explore/dataset/building-footprints-2009/information/

## Download json file from the above... Iright now it's sitting in downloads folder...
van_b <- st_read("/Users/chisato/Downloads/building-footprints-2009.json")


## Theme is "Red" :)  I love the red used in Tableau
van_b %>% ggplot() +
  geom_sf(aes(fill=base_m,color=topelev_m), size=0.05) +
  coord_sf() +
  theme_map() +
  scale_fill_continuous_tableau("Red", guide="none") +
  scale_color_continuous_tableau("Red", guide="none") +
  labs(caption="Data Source: Vancouer Open Data Portal")

ggsave("day_07/Vancouver_Building_Red.png", width=16*1.5, height=12*1.5)


## I just want to try rayshader package...
# all of vancouver is just too big of file.. so I'm going to crop only the downtown
van_b_cropped <- st_crop(van_b,van_bbox)

##  I get a warning, so maybe that's not recommended to do...

## first I think I want center of each polygon (building)
xy_df <-van_b_cropped %>% mutate(geometry = st_centroid(geometry))  %>% st_coordinates()
xyz_df <-cbind(xy_df,van_b_cropped$topelev_m, van_b_cropped$area_m2)

## X, Y, V3 - elevation of building at the top., V4 - area of building
xyz_df %>% as_tibble() 

test <-xyz_df %>%
  as_tibble() %>%
  ggplot(aes(x=X,y=Y)) +
  geom_point(aes(color=V3, size=V4), shape=19) +
  theme_void() +
  scale_color_continuous_tableau("Red", guide="none") +
  scale_size_area(max_size=6, guide="none")

test

## below
test %>% plot_gg(width = 12, height=8, multicore = TRUE, windowsize = c(1600, 900), 
                 zoom = 0.8, phi = 35, theta = 30, sunangle = 225, soliddepth = -100,scale=300)

render_snapshot(clear = TRUE)

