# devtools::install_github("eliocamp/ggnewscale")

libs<- c("sf", "ggpubr", "raster", "cowplot", "tidyverse","rgdal", "ggnewscale")

iini <-function(x){
  #stands for install if not installed
  if (!x %in% rownames(installed.packages())) install.packages(x)
}
lapply(libs, iini)
lapply(libs, library, character.only = TRUE, verbose = FALSE)


source("R/a_data_prep.R")

plot_file <- "data/all_plot_locations.gpkg"
# download elevation @ 800m from http://www.prism.oregonstate.edu/normals/
elev_file <- "/home/a/data/elevation/PRISM_us_dem_800m_bil/PRISM_us_dem_800m_bil.bil"
hs_file <- "/home/a/data/background/hillshade.tif"

if(!file.exists(plot_file)){
  utm11nad<- "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs"
  latlong<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
  
  ff <- st_read("/home/a/projects/archive/FF_Study/Data/Plots/FF_plots.shp") %>%
    st_zm(drop=TRUE) %>%
    mutate(study = "ff",
           scale = "plot") %>%
    dplyr::select(plot = Name, study, scale)
  
  js <- st_read("/home/a/projects/archive/FF_Study/Data/Plots/AllPlots20160728.shp") %>%
    filter(PlotType == "Jones_Transects" & substr(Name,4,8)!= "STAKE") %>%
    st_zm(drop=TRUE) %>%
    mutate(study = "js",
           scale = "plot") %>%
    dplyr::select(plot = Name, study, scale)
  
  bm <- read.csv("/home/a/projects/biomass/data/biomass_2017 - site.csv") %>%
    st_as_sf(coords=c("longitude","latitude"), crs = latlong) %>%
    st_zm(drop=TRUE) %>%
    mutate(study = "bm",
           scale = "plot") %>%
    dplyr::select(plot, study, scale)
  
  cg18 <- read.csv("data/idaho_cheatgrass_bm_2018 - Sheet1.csv") %>%
    st_as_sf(coords = c("UTMx..NAD.83.zone.11.",
                        "UTMy..NAD.83.zone.11."), 
             crs = utm11nad) %>%
    mutate(scale = "subplot",
           study = region) %>%
    dplyr::select(plot, study, scale) %>%
    st_transform(crs=st_crs(bm))
  
  cg17 <- read.csv("data/cg_mass_cover_2017 - Sheet1.csv") %>%
    st_as_sf(coords = c("UTMx..NAD11.ID.and.west..NAD12.UT.", 
                        "UTMx..NAD11.ID.and.west..NAD12.UT..1"),
             crs = utm11nad)%>%
    mutate(study = region,
           scale = "subplot") %>%
    dplyr::select(plot, study, scale) %>%
    st_transform(crs=st_crs(bm))
  
  cg16 <-rbind(readxl::read_xlsx("data/cheatgrass_sent.xlsx",sheet = 1) %>%
                 mutate(study = "western"),
               readxl::read_xlsx("data/cheatgrass_sent.xlsx",sheet = 2) %>%
                 mutate(study = "northern"),
               readxl::read_xlsx("data/cheatgrass_sent.xlsx",sheet = 3)%>%
                 mutate(study = "eastern")) %>%
    filter(position == "center") %>%
    mutate(plot = as.numeric(str_sub(point, 3, 4))) %>%
    st_as_sf(coords = c("UTMx", "UTMy"),
             crs = utm11nad) %>%
    left_join(read_csv("data/BRTE_mass-cover-ht_10_11_16 - Sheet1.csv"),
              by="plot") %>%
    mutate(scale = "subplot") %>%
    dplyr::select(plot, study, scale) %>%
    st_transform(crs=st_crs(bm))
  
  cg19 <- read_csv("data/cheatgrass_2019.csv") %>%
    st_as_sf(coords = c("UTMx", "UTMy"),
             crs = utm11nad) %>%
    mutate(study = region)%>%
    mutate(scale = "subplot") %>%
    dplyr::select(plot, study, scale) %>%
    st_transform(crs=st_crs(bm))
    
  
  
  all<- list(ff,bm,js, cg16,cg17,cg18,cg19) %>%
    do.call("rbind",.) 
  
  st_write(all,plot_file, delete_dsn = TRUE)
  st_write(all, "all_plot_locations.shp")
  
}else{all<-st_read(plot_file)}


if(!file.exists(hs_file)){
  elev<- raster(elev_file)
  slope <- terrain(elev, opt="slope")
  aspect <- terrain(elev, opt = "aspect")
  hs<- raster::hillShade(slope=slope, aspect=aspect) 
  writeRaster(hs, hs_file)
}else{hs<-raster(hs_file)}

all <- all %>% st_transform(crs=crs(hs, asText=T))
extent <- st_bbox(all, dist = 1) %>%
  st_as_sfc

states <- st_read("/home/a/data/background/CUS/") %>%
  st_transform(st_crs(all))# %>% st_intersection(extent)

hs_c<- crop(hs, as(st_buffer(extent, dist = 2), "Spatial"))


p1 <- ggplot() +
  geom_raster(data = as.data.frame(hs_c,xy=TRUE),
              aes(x=x,y=y, fill=layer), show.legend = F) +
  scale_fill_gradient("hillshade", low = "black", high = "white") +
  new_scale("fill") +
  geom_sf(data = all, aes(fill=study, shape = scale),
          size = 2,show.legend = "point")  +
  scale_shape_manual(values = c(21,22)) +
  scale_fill_discrete("study", guide = guide_legend(override.aes = list(shape = 21))) +
  geom_sf(data = states, fill="transparent") +
  coord_sf(xlim = c(-116,-119.5), ylim = c(38,43.25)) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "transparent"),
        legend.position = "right",
        legend.background = element_rect(fill = "transparent"),
        legend.justification = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

p2 <- ggplot() +
  geom_sf(data = states, fill = "white", color="grey") +
  theme_bw() +
  geom_sf(data=extent, fill="transparent")+
  coord_sf(xlim=c(-125,-108),ylim=c(30,50)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(color = "black", fill=NA))


ggdraw() +
  cowplot::draw_plot(p1,x=0,y=0)+
  cowplot::draw_plot(p2, x=0.6, y=0.0, width = 0.4, height = 0.4) +
  ggsave("figures/map.png", dpi = 600, height = 7, width = 8)

# ggarrange(p1,p2, widths=c(3,1), common.legend = T, legend="bottom") +
#   ggsave("figures/map.png", dpi = 600)

