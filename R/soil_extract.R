# extracting soil data to points

library(raster)
library(sf)
library(tidyverse)

get_folded_aspect <- function(aspect) {
  abs(180 - abs(aspect - 225))
}


plot_file <- "data/all_plot_locations.gpkg"
soil_tax <- "/home/a/data/soil/soilgrids/TAXNWRB_250m.tif"
soil_meanings <- '/home/a/data/soil/soilgrids/TAXNWRB_250m_ll.tif.csv'
elevation_file <- '/home/a/data/background/elevation/PRISM_us_dem_800m_bil.bil'

soil <- raster(soil_tax)
points <- st_read(plot_file)
meanings <- read.csv(soil_meanings) %>%
  select(soil_num=Number, Group)
dem <- raster(elevation_file)
slope<-terrain(dem, opt="slope", unit = 'degrees')
aspect <- terrain(dem, opt="aspect", unit = 'degrees')

points$soil_num <- raster::extract(soil, points)
points$elevation<- raster::extract(dem, points)
points$aspect <- round(raster::extract(aspect, points))
points$slope <- round(raster::extract(slope, points))

points <- left_join(points, meanings) %>%
  mutate(soil_num =as.factor(soil_num),
         Group = as.factor(as.character(Group)),
         plot = replace(as.character(plot), as.character(study) == "cg", 
                        paste(as.character(study), as.character(plot), sep="_")))

st_write(points,"data/all_plots_w_soil.gpkg", delete_dsn = TRUE)

big_table <- points %>%
  rename("Soil type" = Group) %>%
  mutate(Latitude = round(st_coordinates(.)[,2],2),
         Longitude = round(st_coordinates(.)[,1],2)) %>%
  st_set_geometry(NULL) %>%
  select(-soil_num,-study) %>%
  write_csv("data/big_table.csv")

topo_sum <- points %>%
  st_set_geometry(NULL) %>%
  mutate("Folded aspect" = get_folded_aspect(aspect))%>%
  select(study, Elevation = elevation, Slope = slope, `Folded aspect`) %>%
  group_by(study) %>%
  summarise_all(function(x) round(mean(x)))%>%
  ungroup() %>%
  gather(key = Variable, value = value, -study) %>%
  spread(key = study, value= value)
  

soil_summary <- points %>% 
  count(study, Group) %>%
  st_set_geometry(NULL) %>%
  spread(study, n, fill=0) %>%
  rename("Variable" = Group) %>%
  rbind(topo_sum) %>%
  write_csv("data/soil_summary.csv")


