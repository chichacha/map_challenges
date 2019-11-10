
## Yellow - Street Light
library(tidyverse)
library(sf)
library(ggthemes)
library(hrbrthemes)


# https://opendata.vancouver.ca/explore/dataset/street-lighting-poles/
# Data Set containing over 56K lighting poles

van_light <-tibble(
  data_name = c("street-lighting-poles","street-lighting-conduits","street-lighting-junction-boxes","street-lighting-service-panels"),
  url = str_glue("https://opendata.vancouver.ca/api/v2/catalog/datasets/{data_name}/exports/geojson?rows=-1&timezone=UTC&pretty=false"),
  short_name = str_remove(data_name,"street-lighting-")
) 

## Read all 4 datasets
van_light_df <- van_light %>%
  mutate(data=map(url, st_read))

## Inspect 4 different datasets 
van_light_df$data[[1]]  ## pole = point  - Not sure what node number is... most have under 20, but there are handful over 200 
van_light_df$data[[2]]  ## conduit = line
van_light_df$data[[3]]  ## junction box = point (material attribute, cast iron, concrete, fibreglass, plastic, NA)
van_light_df$data[[4]] ## Servie Panel - 1371 features with panel number 

van_light_df$data[[2]] %>% as_tibble() %>% count(wireset_type, sort=T) ## most common 3-12, 3-10, but there are 167 different wireset types recorded.

van_light_df$data[[1]] %>%
  st_coordinates() %>%
  as_tibble() %>%
  ggplot(aes(x=X,y=Y)) +
  geom_hex(bins=40) +
  scale_fill_gradient(low="white", high="goldenrod1") +
  theme_map() +
  theme(plot.background=element_rect(fill="#1e1e1e"),
   text = element_text(color="white"))



van_light_df$data[[1]] %>%
  ggplot() +
  geom_sf(color="goldenrod1", size=0.01, alpha=0.8, shape=8) +
  theme_map(base_family="Roboto Condensed") +
  theme(plot.background=element_rect(fill="#1e1e1e"),
        text = element_text(color="white")) +
  labs(x="",y="",
       caption="Data from Vancouver Open Data Portal:https://opendata.vancouver.ca/explore/dataset/street-lighting-poles/information/",
       title=str_glue("{nrow(van_light_df$data[[1]])} Street Light Poles Total!")) 

ggsave("day_09/Vancouver_with_56K_Dots.png", width=16, height=9)
xopen::xopen("day_09/Vancouver_with_56K_Dots.png")



van_light_df$data[[1]] %>%
  as_tibble() %>%
  mutate(geo_local_area=fct_explicit_na(geo_local_area)) %>%
  count(geo_local_area,node_number,block_number, sort=T) %>%
  ggplot(aes(x=fct_reorder(geo_local_area,n,sum),y=n)) +
  geom_col(fill="goldenrod1", color="white", size=0.1) +
  geom_text(data=. %>% count(geo_local_area,wt=n), aes(label=n), 
            hjust=0, family="Roboto Condensed", color="white") +
  coord_flip(ylim=c(0,5000)) +
  theme_modern_rc() +
  labs(x="",y="Number of Light Poles by Neighbourhood",
       caption="Data from Vancouver Open Data Portal:https://opendata.vancouver.ca/explore/dataset/street-lighting-poles/information/",
       title=str_glue("{nrow(van_light_df$data[[1]])} Street Light Poles Total!")) 
ggsave("day_09/Vancouver_Street_LightPole.png", width=16, height=9)


edges <-van_light_df$data[[2]] %>%
  mutate(idx=row_number(),
         gtype = st_geometry_type(geometry))

edges %>%
  as_tibble() %>%
  count(gtype)

nodes <-edges %>%
  filter(gtype=="LINESTRING") %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L1) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))

nodes %>%
  ggplot(aes(x=X,y=Y)) +
  geom_point(size=0.5, aes(color=start_end)) +
  geom_line(aes(group=edgeID), size=0.01,color="lightgoldenrod1") +
  scale_color_manual(values=c("goldenrod1","goldenrod3")) +
  theme_modern_rc() +
  coord_sf()
  



