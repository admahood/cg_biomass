# precip extract for main models
library(sf)
library(tidyverse)
library(raster)



bils <- list.files(path = "data/bil/", pattern = "*.bil$", full.names = TRUE)

years <- 2016:2018

ppt2016 <- raster::stack(bils[6:17]) %>%
  raster::calc(sum)
ppt2017 <- raster::stack(bils[18:29]) %>%
  raster::calc(sum)
ppt2018 <- raster::stack(bils[30:41]) %>%
  raster::calc(sum)


js_plots <- st_read("data/plots_w_elevation/") %>%
  filter(study == "js") %>%
  dplyr::select(plot, study, elevation = Elevation) %>%
  mutate(plot = str_to_lower(plot),
         year = 2016,
         ppt = raster::extract(x=ppt2016,y=.))

bm17_plots <- st_read("data/plots_w_elevation/") %>%
  filter(study == "bm") %>%
  dplyr::select(plot, study, elevation = Elevation) %>%
  mutate(plot = str_to_lower(plot),
         year = 2017,
         ppt = raster::extract(x=ppt2017,y=.))

bm18_plots <- st_read("data/plots_w_elevation/") %>%
  filter(study == "bm") %>%
  dplyr::select(plot, study, elevation = Elevation) %>%
  mutate(plot = str_to_lower(plot),
         year = 2018,
         ppt = raster::extract(x=ppt2018,y=.))

plots_w_ppt_elv<- bind_rows(js_plots, bm17_plots, bm18_plots)
st_write(plots_w_ppt_elv, "data/plots_w_elv_ppt.gpkg")

ppt_summary <- plots_w_ppt_elv %>%
  st_set_geometry(NULL) %>%
  group_by(year) %>%
  summarise(ppt_mean = mean(ppt),
            ppt_sd = sd(ppt)) %>%
  ungroup()

