## Day1 Points

library(jsonlite)
library(tidyverse)
library(sf)
library(here)
library(lubridate)
library(osmdata)
library(hrbrthemes)
library(ggrepel)

## Vancouver Tree Data
# https://opendata.vancouver.ca/explore/dataset/street-trees/information/?disjunctive.species_name&disjunctive.common_name&disjunctive.height_range_id&q=&rows=10000&refine.neighbourhood_name=DOWNTOWN

downtown_trees <- st_read("day_01/data/downtown.json")
westend_trees <- st_read("day_01/data/westend.json")

## combine the two
trees <-rbind(downtown_trees, westend_trees)

glimpse(trees)

st_bbox(trees)

base_map <- opq(bbox=st_bbox(trees)) %>% ## bild an overpass query
  add_osm_feature(key="highway") %>%  ## highway for street data 
  osmdata_sf() %>%
  osm_poly2line() ## convert polygon shaped highway to line

base_map$osm_lines

trees %>%
  mutate(yr=year(date_planted)) %>%
  select(species_name, diameter, yr, genus_name, plant_area, common_name, height_range_id) %>%
  plot()


col_pal <- c("#F44336", "#E91E63", "#9C27B0", "#673AB7", "#3F51B5", "#2196F3", "#03A9F4", "#00BCD4", "#009688", "#4CAF50", "#8BC34A", "#CDDC39", "#FFEB3B", "#FFC107", "#FF9800", "#FF5722", "#795548", "#9E9E9E", "#607D8B") 

col_pal_60 <- colorRampPalette(col_pal)(60)

##
base_map$osm_lines %>%
  ggplot() +
  geom_sf(size=0.1, color="#ffffffde") +
  geom_sf(data=trees %>% arrange(date_planted), 
          aes(color=genus_name),size=1) +
  theme_map() +
  coord_sf(xlim=st_bbox(trees)[c(1,3)], ylim=st_bbox(trees)[c(2,4)]) +
  scale_color_manual(values=col_pal_60, guide="none") +
  theme(plot.background = element_rect(fill="gray8")) 

ggsave("day_01/output/Vancouver_TreeAsPoint.svg", width=16, height=9)
xopen::xopen("day_01/output/Vancouver_TreeAsPoint.svg", app="google chrome")


base_map$osm_lines %>%
  ggplot() +
  geom_sf(size=0.1, color="#ffffffde") +
  geom_sf(data=trees %>% arrange(date_planted), 
          aes(color=genus_name),size=1) + 
  geom_sf_text(data=trees %>% filter(diameter>50),
               aes(label=common_name, color=genus_name), size=3, family="Roboto Condensed") +
  theme_map() +
  coord_sf(xlim=st_bbox(trees)[c(1,3)], ylim=st_bbox(trees)[c(2,4)]) +
  scale_color_manual(values=col_pal_60, guide="none") +
  theme(plot.background = element_rect(fill="gray8")) 

ggsave("day_01/output/Vancouver_TreeAsPoint2.svg", width=16, height=9)
xopen::xopen("day_01/output/Vancouver_TreeAsPoint2.svg", app="google chrome")

?geom_sf_text


trees %>%
  ggplot() +
  #geom_sf(data=base_map$osm_lines, size=0.1, color="#ffffff80") +
  geom_sf(data=trees, aes(color=genus_name), size=1) +
  theme_void() +
  coord_sf(xlim=st_bbox(trees)[c(1,3)], ylim=st_bbox(trees)[c(2,4)]) +
  scale_color_manual(values=col_pal_60, guide="none") +
  theme(plot.background = element_rect(fill="gray8"),
        strip.text = element_text(family="Roboto Condensed", colour="#ffffffae")) +
  facet_wrap(~fct_lump(fct_infreq(genus_name),n=19)) 

ggsave("day_01/output/Vancouver_TreeAsPoint_facet.svg", width=16, height=9)
xopen::xopen("day_01/output/Vancouver_TreeAsPoint_facet.svg", app="google chrome")


## Height Range x Diameter
trees %>%
  ggplot(aes(x=height_range_id, y=diameter, color=genus_name)) +
  geom_point(aes(color=genus_name)) +
  geom_text_repel(aes(label=str_wrap(str_c(common_name,std_street,sep=" "), width=20)),
            data= . %>% filter(diameter>50), size=3, min.segment.length = 0, nudge_y=3) +
  scale_color_manual(values=col_pal_60, guide="none") +
  theme_modern_rc() 


trees %>%
  ggplot(aes(x=fct_rev(fct_lump(fct_infreq(genus_name),n=19)))) +
  stat_count(aes(fill=genus_name, group=common_name), size=0.1, color="white") + 
  scale_fill_manual(values=col_pal_60, guide="none") +
  scale_color_manual(values=col_pal_60, guide="none") +
  theme_modern_rc() +
  coord_flip() +
  labs(y="# of trees recorded",x="")

ggsave("day_01/output/Vancouver_Tree_Genus_counts.svg", width=3*2, height=4*2)
xopen::xopen("day_01/output/Vancouver_Tree_Genus_counts.svg", app="google chrome")



### wordcloud of tree genus name
trees %>% as_tibble() %>%
  count(genus_name) %>%
  mutate(n=log(n)) %>%
  wordcloud2::wordcloud2(fontFamily="Roboto Condensed",
                         rotateRatio=0, color = col_pal_60,
                         backgroundColor = "grey8")




