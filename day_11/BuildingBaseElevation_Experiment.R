
## Thanks to @vb_jens for sharing Vancouver package! 
#remotes::install_github("mountainmath/VancouvR")

library(VancouvR)
library(tidyverse)
library(sf)
#install.packages("ggridges")
library(ggridges)
library(ggthemes)


search_cov_datasets("heritage") %>%
  select(dataset_id)

#elevation-contour-lines-2-metre-contours

d_id = "building-footprints-2009"
tmp <-get_cov_data(dataset_id = "building-footprints-2009",rows=10)
tmp
get_cov_metadata(d_id)

bb <- get_cov_data(dataset_id=d_id, format="geojson")
bb_detail <- get_cov_data(dataset_id ="heritage-sites", format="geojson")

bb_overlap <- st_intersection(bb, bb_detail)
bb_mini <- bb %>% filter(bldgid %in% bb_overlap$bldgid)

bb_mini %>% ggplot() +
  geom_sf(aes(fill=orient8, color=orient8)) +
  theme_map() +
  geom_sf_text(data=bb_detail %>% filter(str_detect(street,"BEACH")), 
               aes(label=buildingname), size=3)


bb_simp <- bb %>% select(baseelev_m) %>%
  mutate(geom=st_centroid(geometry))

bb_simp %>%
  ggplot() +
  geom_sf(aes(fill=baseelev_m, color=baseelev_m), size=0.1) +
  scale_fill_gradient2_tableau("Sunset-Sunrise Diverging", guide="none") +
  scale_color_gradient2_tableau("Sunset-Sunrise Diverging", guide="none") +
  theme_map() 

ggsave("day_11/BaseElevation_BuildingsVancouver.png", width=16, height=12)


## buildings on high base elevation
bb_simp <-bb_simp %>%
  mutate(elev_rank = dense_rank(-baseelev_m)) %>%
  arrange(elev_rank)



top25 <- bb %>% 
  mutate(ranking = row_number(-area_m2)) %>%
  filter(ranking <=100) %>%
  mutate(shape_geom = geometry - st_centroid(geometry)) #%>%
  #left_join(bb_detail %>% select(bldgid = bi_id, address, projectvalue) %>% 
             # mutate(bldgid=as.integer(bldgid)))

top25 %>% ggplot() +
  geom_sf(aes(geometry=shape_geom,fill=baseelev_m), color="#ffffffde", size=0.1) +
  geom_sf_text(aes(geometry=shape_geom,label=str_c("@",baseelev_m," m"))) +
  facet_wrap(~ranking, ncol=10) +
  theme_void() +
  scale_fill_gradient2_tableau("Sunset-Sunrise Diverging", guide="none") +
  theme(strip.text=element_blank())





bb_simp_df <- bb_simp %>%
  st_coordinates()

bb_simp_df2 <-bb_simp_df %>% 
  as_tibble() %>%
  group_by(L2) %>%
  summarise(long=mean(X), lat=mean(Y)) %>%
  ungroup() %>%
  rename(idx=L2)

bb_simp_df <-bb_simp_df2 %>%
  inner_join(bb_simp %>% as_tibble() %>% mutate(idx=row_number()))


bb_simp_mini <- bb_simp_df %>%
  mutate(x = round(long,3),
         y = round(lat,3)) %>%
  group_by(x, y) %>%
  summarise(z = mean(baseelev_m)) %>%
  ungroup()

bb_simp_mini %>% 
  arrange(-z) %>%
  ggplot(aes(x=x,y=y)) +
  geom_tile(aes(fill=z)) +
  theme_map()

bb_simp_mini %>%
  add_count(y) %>%
  filter(n>3) %>%
  ggplot(aes(x=x, y=factor(y))) +
  geom_density_ridges_gradient(aes(height=z), 
                               fill="#000000ae", 
                               stat="identity", size=0.01,
                               color="#ffffff20") +
  theme_void() 
  
  

elev10 <-get_cov_data(dataset_id=d_id, format = "geojson", select = "*",
             where = NULL, apikey = getOption("VancouverOpenDataApiKey"),
             rows = NULL, cast_types = TRUE, refresh = FALSE)

elev10 <- elev10 %>% mutate(L1 = row_number())

elev10_df <- elev10 %>%
  st_coordinates() %>%
  as_tibble() %>%
  inner_join(elev10 %>% as_tibble(), by="L1")

elev10_df_mini <- elev10_df %>% select(x=X,y=Y,z=elevation,idx=L1)

elev10_small<-elev10_df_mini %>% 
  mutate(y_round = round(y,3),
         x_round=round(x,3)) %>%
  group_by(x_round, y_round) %>%
  summarise(z_round = median(z)) %>%
  ungroup()


elev10_small %>%
  ggplot(aes(x=x_round, y=factor(y_round), group=factor(y_round))) +
  geom_density_ridges_gradient(aes(height=z_round), fill="#000000ae", 
                      stat="identity", size=0.01, panel_scaling = F,
                      color="#ffffff20") +
  theme_void() +
  scale_fill_viridis_c(option="B", alpha=0.5)

elev10_small %>%
  ggplot(aes(x=x_round, y=y_round)) +
  geom_tile(aes(fill=z_round)) +
  scale_fill_viridis_c(option="B", alpha=0.5) +
  theme_map()

elev10 %>%
  ggplot() +
  geom_sf(aes(color=elevation)) +
  theme_map() +
  scale_color_viridis_c(option="B")


elev10_small2 <-elev10_small %>% 
  complete(nesting())

