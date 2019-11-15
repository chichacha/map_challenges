library(tidyverse)
library(VancouvR)  ## making it so much easier to access Vancouver Open Data Portal!
library(ggthemes)
library(gganimate)
library(sf)
library(lwgeom) ## lightweight geom

col_pal <- c("#F44336", "#E91E63", "#9C27B0", "#673AB7", "#3F51B5", "#2196F3", "#03A9F4", "#00BCD4", "#009688", "#4CAF50", "#8BC34A", "#CDDC39", "#FFEB3B", "#FFC107", "#FF9800", "#FF5722", "#795548", "#9E9E9E", "#607D8B") 

search_cov_datasets("boundary") %>% select(dataset_id)

df <- get_cov_data("block-numbers", format="geojson") 
local_area <- get_cov_data("local-area-boundary", format="geojson")



test <- get_cov_data("block-outlines", format="geojson") 

test <- test %>% mutate(area=as.numeric(st_area(geometry)))

test
test %>%
  ggplot() +
  geom_sf(aes(fill=log(area+1)), color="white", size=0.1) +
  theme_map() +
  scale_fill_gradientn(colors=rev(col_pal), guide="none") 



bbox_w <- function(bbox){
  abs(bbox[3]-bbox[1])
}

bbox_h <- function(bbox){
  abs(bbox[4]-bbox[2])
}

voting_sf <- voting %>%
  mutate(area=as.numeric(st_area(geometry)),
         area2 = as.numeric(st_geod_area(geometry)),
         geom_centre = st_centroid(geometry),
         geom_shape= geometry-st_centroid(geometry),
         bbox=map(geom_shape,st_bbox),
         bbox_w = map_dbl(bbox,bbox_w),
         bbox_h = map_dbl(bbox,bbox_h),
         idx=row_number(-area),
         circle = lwgeom::st_minimum_bounding_circle(geom_shape),
         radius = sqrt(st_area(circle)/pi)) %>%
  arrange(-area)  %>%
  mutate(x_shift=cumsum(radius*2),
         y_shift=cumsum(radius*2),
         shift = pmap(list(x=x_shift,y=y_shift),~c(.x,.y)))


voting_sf %>%
  ggplot() +
  geom_sf(aes(fill=area), color="white", size=0.5) +
  geom_sf(data=test,color="white",size=0.1, fill="#ffffff00") +
  geom_sf_text(aes(label=division), family="Roboto Condensed", color="#ffffffde") +
  theme_map(base_family="Roboto Condensed") +
  scale_fill_gradient_tableau("Red") +
  labs(title="Vancouver Voting Boundary 2008")

voting_sf %>%
  mutate(geom_shifted=geom_shape + shift) %>%
  ggplot() +
  geom_sf(aes(fill=area, geometry=geom_shifted), color="white", size=0.5) +
  theme_map(base_family="Roboto Condensed") +
  scale_fill_gradient_tableau("Red") 
  
ggsave("day_14/Vertical.svg", width=5, height=20)
xopen::xopen("day_14/Vertical.svg")


library(packcircles)

packing <- circleProgressiveLayout(st_area(voting_sf$circle)) 
packing_sf <- packing %>% st_as_sf(coords=c("x","y")) %>% rename(geom_pack=geometry) %>% select(-radius)

voting_sf2 <-bind_cols(voting_sf,packing_sf)

voting_sf2 %>%
  ggplot() +
  geom_sf(aes(fill=area, geometry=geom_shape+geom_pack), color="white", size=0.5) +
  geom_sf_text(aes(label=division, geometry=geom_shape+geom_pack, family="Roboto Condensed")) +
  theme_map(base_family="Roboto Condensed") +
  scale_fill_gradient_tableau("Red")

voting_sf$geom_centre
voting
voting %>%
  arrange(-area) %>%
  
  ggplot() +
  geom_sf(aes(geometry=geom_shape, fill=area), color="white", size=0.5)+
  theme_map(base_family="Roboto Condensed") +
  scale_fill_gradientn(colors=rev(col_pal), guide="none") 




df %>%
  ggplot() +
  geom_sf_text(aes(label=label, color=geo_local_area), size=3, family="Roboto Condensed") +
  theme_map() +
  scale_color_hue(guide="none") +
  geom_sf(data=test)
  





