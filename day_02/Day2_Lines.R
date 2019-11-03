## Day 2 - Lines 

library(osmdata)
library(tidyverse)
library(visdat)
library(skimr)
library(ggthemes)
library(janitor)


## Get Open Street Data as First Layer
van_bbox <- c(-123.27,49.195,-123.020,49.315)
van <- opq(bbox=van_bbox) %>% ## bild an overpass query
  add_osm_feature(key="highway") %>%  ## highway for street data 
  osmdata_sf() %>%
  osm_poly2line() ## convert polygon shaped highway to line

## I missed Day 1 - Point Assignment...
bus_stop <-van$osm_points %>%
  filter(highway=="bus_stop") 

bus_stop_skim <-bus_stop %>% as_tibble() %>% skim()
bus_stop_skim %>% filter(stat=="missing") %>% filter(value<1992) %>% arrange(value) %>%
  DT::datatable()

bus_stop <-bus_stop %>% 
  select(osm_id, highway, name, geometry, ref, public_transport, shelter, wheelchair)

bus_stop %>% plot()


## Streets are lines! :)
van_street <- van$osm_lines


### Exploring.....  
## osm_id, geometry, highway seems to have data.
## only 67% of rows don't have name, but 7912 data point with name.
## 5776 row has lanes, 5558 surface, 4487 source, 3854 oneway, 3168 maxspeed, 2882 service

tmp_skimmed <-van_street %>% as_tibble() %>% skim()


tmp_skimmed %>%
  filter(stat=="missing") %>%
  arrange(value) %>%
  select(variable, missing=value) %>%
  mutate(missingness=round(missing/nrow(van_street),2),
         hasdata=nrow(van_street)-missing) %>%
  left_join(tmp_skimmed %>% filter(stat=="n_unique") %>% select(variable, n_unique=value)) %>%
  DT::datatable()

var_to_use <- tmp_skimmed %>% 
  filter(stat=="missing") %>% 
  top_n(n=10, wt=-value) %>%
  pull(variable)

van_street <-van_street %>% 
  select_at(vars(var_to_use))

van_street %>% select_at(vars(var_to_use[c(3:6)])) %>% plot()

van_street %>% as_tibble() %>% 
  mutate(name_exist=!is.na(name),
         highway=fct_infreq(highway)) %>%
  count(highway, name_exist) %>%
  ggplot(aes(x=highway, y=name_exist)) +
  geom_tile(aes(fill=n)) +
  geom_text(aes(label=n), color="white") +
  coord_flip() +
  scale_fill_viridis_c()

van_street_name <- van_street %>%
  filter(!is.na(name) & !is.na(highway))

van_street_name %>% 
  count(name, sort=T) %>%
  ggplot() +
  geom_sf(aes(color=str_sub(name,1L,1L))) +
  geom_sf(data=bus_stop, color="tomato", size=0.5) +
  coord_sf(xlim=van_bbox[c(1,3)], ylim=van_bbox[c(2,4)]) +
  theme_map()


van_street_df <-van_street_name %>%
  mutate(highway=fct_infreq(highway)) %>%
  group_by(name) %>%
  summarise(highway_list = paste(unique(highway), collapse="|"),
            highway_type=n_distinct(highway),
            n = n(),
            resid_or_major = sum(highway %in% c("residential","primary","secondary","tertiary")))

van_street_df %>% 
  ggplot() +
  geom_sf(aes(color=highway_type)) +
  coord_sf(xlim=van_bbox[c(1,3)], ylim=van_bbox[c(2,4)]) +
  theme_map() +
  scale_color_viridis_c(end=0.8)


  

st_suffix <- str_to_title(c("street","avenue","drive","place", "road","crescent","lane","court","way","mews","boulevard","highway","bridge"))

st_direction <-str_to_title(c("west","east","south","north"))

van_street_df <- van_street_df %>%
  ungroup() %>%
  mutate(street_type=replace_na(str_extract(name,str_c(st_suffix,collapse="|")),""),
         direction = replace_na(str_extract(name,str_c(st_direction,collapse="|")),""),
         num_street =replace_na(as.numeric(str_extract(name,"\\d+")),0))


library(tidytext)
wordcloud_df <- van_street_df %>%
  as_tibble() %>%
  select(name, street_type) %>%
  unnest_tokens(word, name) %>%
  count(word,street_type, sort=T) %>% 
  mutate(n=log(n))  %>%
  mutate(color=case_when(street_type=="Avenue" ~ col_pal[1],
                         street_type=="Drive" ~ col_pal[4],
                         street_type=="Lane" ~ col_pal[7],
                         street_type=="Street" ~ col_pal[10],
                         TRUE ~ col_pal[13]))



wordcloud_df %>% select(word, n) %>% 
  wordcloud2::wordcloud2(rotateRatio=0, 
                         color=wordcloud_df$color,
                         fontFamily="Roboto Condensed",
                         backgroundColor="gray8") 

col_pal <- c("#F44336", "#E91E63", "#9C27B0", "#673AB7", "#3F51B5", "#2196F3", "#03A9F4", "#00BCD4", "#009688", "#4CAF50", "#8BC34A", "#CDDC39", "#FFEB3B", "#FFC107", "#FF9800", "#FF5722", "#795548", "#9E9E9E", "#607D8B") 

col_pal %>% scales::show_col()

van_street_df %>% count(street_type, sort=T)


van_street_df %>%
  mutate(street_type=(fct_other(street_type,
                                keep=c("Street","Avenue","Drive","Lane")))) %>%
  ggplot() +
  geom_sf(data=van_street, color="#ffffff80", size=0.1) +
  geom_sf(aes(color=street_type), size=0.8) +
  scale_color_manual(values=col_pal[c(1,4,7,10,13,16)], 
                     name="street type", guide="none") +
  coord_sf(xlim=van_bbox[c(1,3)], ylim=van_bbox[c(2,4)]) +
  theme_map() +
  theme(plot.background=element_rect(fill="gray8")) 

ggsave("day_02/output/Vancouver_Street_Map.svg", width=16, height=12)






