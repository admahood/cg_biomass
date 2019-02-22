library("sf")
library("tidyverse")
library("ggpubr")

plot_file <- "data/all_plot_locations.gpkg"

if(!file.exists(plot_file)){
  utm11nad<- "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs"
  latlong<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
  
  ff <- st_read("/home/a/projects/FF_Study/Data/Plots/FF_plots.shp") %>%
    mutate(study = "ff") %>%
    select(plot = Name, study)
  
  js <- st_read("/home/a/projects/FF_Study/Data/Plots/AllPlots20160728.shp") %>%
    filter(PlotType == "Jones_Transects" & substr(Name,4,8)!= "STAKE") %>%
    mutate(study = "js") %>%
    select(plot = Name, study)
  
  bm <- read.csv("/home/a/projects/biomass/data/biomass_2017 - site.csv") %>%
    st_as_sf(coords=c("longitude","latitude"), crs = latlong) %>%
    mutate(study = "bm") %>%
    select(plot, study)
  
  id <- read.csv("data/idaho_cheatgrass_bm_2018 - Sheet1.csv") %>%
    st_as_sf( coords = c("UTMx..NAD.83.zone.11.", "UTMy..NAD.83.zone.11."), 
              crs = utm11nad) %>%
    filter(region == "idaho") %>%
    mutate(study = "cg") %>%
    select(plot, study) %>%
    st_transform(crs=st_crs(bm))
  
  all<- list(id,ff,bm,js) %>%
    do.call("rbind",.)
  
  st_write(all,plot_file, delete_dsn = TRUE)
}else{all<-st_read(plot_file)}

extent <- st_bbox(all, dist = 1) %>%
  st_as_sfc

states <- st_read("/home/a/data/background/CUS/") %>%
  st_transform(st_crs(all))# %>% st_intersection(extent)


p1 <- ggplot() +
  geom_sf(data = all, aes(color=study),alpha=0.8,show.legend = "point")  +
  geom_sf(data = states, fill="transparent") +
  coord_sf(xlim = c(-116,-118.5), ylim = c(40.5,43.25)) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "transparent"),
        legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.background = element_rect(fill = "transparent"))

p2 <- ggplot() +
  geom_sf(data = states, fill = "white", color="grey") +
  theme_bw() +
  geom_sf(data=extent, fill="transparent")+
  coord_sf(xlim=c(-125,-108),ylim=c(30,50)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())


ggdraw(p1) +
  cowplot::draw_plot(p2, x=0.23, y=0.58, width = 0.4, height = 0.4) +
  draw_plot_label("NV",x=.6, y= .4, colour = "grey40") +
  draw_plot_label("ID", x = .6, y= .7, colour = "grey40") +
  ggsave("map.png", dpi = 600)
