library(sf)
library(tidyverse)
all_plots <- st_read("data/plot_shp/")
# all_plots_before <- st_read("data/all_plot_locations.gpkg")
# 
# all_plots[c(1:28,89:nrow(all_plots)), 6] <- 0.1
# all_plots[c(1:28,89:nrow(all_plots)), 7] <- 1
# 
# summary(all_plots)
# 
# 
#   ggplot() +
#   geom_sf() +
#   geom_sf(data = all_plots)
# 
# 
# new_plots<-all_plots_before %>%
#   st_set_geometry(st_geometry(all_plots)) 
# 

# data reading in process

utm11nad<- "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs"
latlong<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 

ff <- st_read("/home/a/projects/archive/FF_Study/Data/Plots/FF_plots.shp") %>%
  st_zm(drop=TRUE) %>%
  mutate(study = "ff",
         scale = "plot",
         date = as.Date(str_sub(DateTimeS,1,10), "%Y-%m-%d"),
         month = lubridate::month(date),
         year = lubridate::year(date),
         bioplot_m2 = 0.1,
         covplot_m2 = 1) %>%
  dplyr::select(plot = Name, study, scale, date,month,bioplot_m2,covplot_m2,year)

js <- st_read("/home/a/projects/archive/FF_Study/Data/Plots/AllPlots20160728.shp") %>%
  filter(PlotType == "Jones_Transects" & substr(Name,4,8)!= "STAKE") %>%
  st_zm(drop=TRUE) %>%
  mutate(study = "js",
         scale = "plot",
         date = as.Date(str_sub(DateTimeS,1,10), "%Y-%m-%d"),
         month = lubridate::month(date),
         year = lubridate::year(date),
         bioplot_m2 = 0.1,
         covplot_m2 = 0.1) %>%
  dplyr::select(plot = Name, study, scale, date,month,bioplot_m2,covplot_m2,year)

bm <- read.csv("/home/a/projects/biomass/data/biomass_2017 - site.csv") %>%
  st_as_sf(coords=c("longitude","latitude"), crs = latlong) %>%
  st_zm(drop=TRUE) %>% 
  mutate(study = "bm",
         scale = "plot",
         date = as.Date(date, "%m/%d/%Y"),
         month = lubridate::month(date),
         year = lubridate::year(date),
         bioplot_m2 = 0.1,
         covplot_m2 = 0.1) %>%
  dplyr::select(plot, study, scale, date,month,bioplot_m2,covplot_m2,year)

cg18 <- read.csv("data/idaho_cheatgrass_bm_2018 - Sheet1.csv") %>%
  st_as_sf(coords = c("UTMx..NAD.83.zone.11.",
                      "UTMy..NAD.83.zone.11."), 
           crs = utm11nad) %>%
  mutate(scale = "subplot",
         study = region,
         date = as.Date(sampling.date, "%d-%b-%Y"),
         month = lubridate::month(date),
         year = lubridate::year(date),
         bioplot_m2 = 0.1,
         covplot_m2 = 1) %>%
  dplyr::select(plot, study, scale, date,month,bioplot_m2,covplot_m2,year) %>%
  st_transform(crs=st_crs(bm))

cg17 <- read.csv("data/cg_mass_cover_2017 - Sheet1.csv") %>%
  st_as_sf(coords = c("UTMx..NAD11.ID.and.west..NAD12.UT.", 
                      "UTMx..NAD11.ID.and.west..NAD12.UT..1"),
           crs = utm11nad)%>% 
  mutate(study = region,
         scale = "subplot",
         date = as.Date(date, "%m-%d-%Y"),
         month = lubridate::month(date),
         year = lubridate::year(date),
         bioplot_m2 = 0.1,
         covplot_m2 = 1) %>%
  dplyr::select(plot, study, scale, date,month,bioplot_m2,covplot_m2,year) %>%
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
  mutate(scale = "subplot",
         date = as.Date(date, "%m/%d/%Y"),
         month = lubridate::month(date),
         year = lubridate::year(date),
         bioplot_m2 = 0.1,
         covplot_m2 = 1) %>%
  dplyr::select(plot, study, scale, date,month,bioplot_m2,covplot_m2,year) %>%
  st_transform(crs=st_crs(bm))

cg19 <- read_csv("data/cheatgrass_2019.csv") %>%
  st_as_sf(coords = c("UTMx", "UTMy"),
           crs = utm11nad) %>%
  mutate(study = region)%>%
  mutate(scale = "subplot",
         date = as.Date(date, "%d-%b-%Y"),
         month = lubridate::month(date),
         year = lubridate::year(date),
         bioplot_m2 = 0.1,
         covplot_m2 = 1) %>%
  dplyr::select(plot, study, scale, date,month,bioplot_m2,covplot_m2,year) %>%
  st_transform(crs=st_crs(bm))

all<- list(ff,bm,js, cg16,cg17,cg18,cg19) %>%
  do.call("rbind",.) 

new_plots<-all %>%
  st_set_geometry(st_geometry(all_plots))
dir.create("data/new_plots")
st_write(new_plots, "data/new_plots/new_plots.shp", delete_dsn = TRUE)

st_read("~/data/background/CUS/") %>%
  st_transform(crs = st_crs(new_plots)) %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = new_plots)

st_read("data/plots_w_elevation/") -> xxx

xxx <- xxx %>%
  mutate(plot = str_to_lower(plot),
         plot = str_replace_all(plot,"-", "_"))
all <- all %>%
  mutate(plot = str_to_lower(plot),
         plot = str_replace_all(plot,"-", "_"))
yyy <- left_join(dplyr::select(xxx, plot, Elevation),all) %>%
  filter(study %in% c("js", "bm17", "bm18"))

st_write(yyy, "data/plots_w_elevation.gpkg")
