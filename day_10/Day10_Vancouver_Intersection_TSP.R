library(tidyverse)
library(ggplot2)
library(sf)


## Black & White

street_datasets <- tibble(
  data_name = c("public-streets", "lanes", "non-city-streets", "one-way-streets", "street-intersections"),
  url = str_glue("https://opendata.vancouver.ca/api/v2/catalog/datasets/{data_name}/exports/geojson?rows=-1&timezone=UTC&pretty=false")
) %>% mutate(sf_data = map(url,st_read))


street_datasets$data_name
street_datasets$sf_data[[1]]

street_datasets$sf_data[[1]] 
street_datasets$sf_data[[2]] 
street_datasets$sf_data[[3]] 
xstreet <-street_datasets$sf_data[[5]] %>%
  separate(xstreet,into=c("x1","x2"), sep=" AND ", remove=F)

xstreet %>% count(x1,x2,geo_local_area, sort=T) %>%
  arrange(-n) %>%
  ggplot() +
  geom_sf(aes(color=geo_local_area, size=n)) +
  theme_map() +
  scale_size_area(max_size=1)


library(TSP)
tmp <- xstreet %>%
  st_coordinates() %>%
  as_tibble()

x_df <- bind_cols(tmp, xstreet %>% select(x1,x2,xstreet))

?dist
tmp_dist <- dist(tmp, method="manhattan")
tmp_tsp <- TSP(tmp_dist)
n_of_cities(tmp_tsp)
tsp_sol <-solve_TSP(tmp_tsp, start=4510) ## i wanted starting point to be Denman x Georgia in downtown


x_df_arranged <-x_df[tsp_sol,]  ## reorder the dataset
x_df_arranged %>%
  mutate(idx=row_number()) %>%
  ggplot() +
  geom_polygon(fill="#000000", size=1,aes(x=X,y=Y)) +
  geom_path(color="#ffffffde", size=1.2,aes(x=X,y=Y)) +
  geom_point(color="#ffffffde", size=1,aes(x=X,y=Y)) +
  geom_sf(data=street_datasets$sf_data[[1]], size=0.2, color="#ffffff20") +
  theme_map() +
  scale_color_viridis_c() +
  theme(plot.background = element_rect("gray8"))

ggsave("day_10/Black_And_White_TSP.png", width=16, height=12)
xopen::xopen("day_10/Black_And_White_TSP.png")
