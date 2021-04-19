library(tidyverse)
library(lubridate)
library(sf)

sample_dates_js <- read_csv("/home/a/projects/Jones_Study/data/data_2016/Jones_Cover_16.csv") %>%
  mutate(sample_date = as.Date(Date, "%m/%d/%y"),
         plot = str_c(`Plot#`, TransectPair),
         year = year(sample_date)) %>%
  dplyr::select(plot, sample_date, year) %>%
  unique

sample_dates <- read_csv("data/bm_site_data_w_biomass.csv") %>%
  mutate(sample_date = as.Date(date, "%m/%d/%Y"),
         year = year(sample_date)) %>%
  dplyr::select(plot, sample_date, year)  %>%
  rbind(sample_dates_js)


if(!file.exists("data/ee/tr_aq.Rda")){aqua <-st_read("data/ee/ndvi-sequence_modis_aqua_cg-plots.geojson")
  terra <-st_read("data/ee/ndvi-sequence_modis_terra_cg-plots.geojson")
  
  aq <- aqua %>%
    dplyr::select(id, ndvi = ndvi_aqua, plot, study) %>%
    mutate(date = str_sub(id, 1, 10) %>% str_remove_all("_") %>% lubridate::ymd()) %>%
    dplyr::select(-id) %>%
    mutate(doy = lubridate::yday(date),
           year = lubridate::year(date)) 
  
  tr <- terra %>%
    dplyr::select(id, ndvi = ndvi_terra, plot, study) %>%
    mutate(date = str_sub(id, 1, 10) %>% str_remove_all("_") %>% lubridate::ymd()) %>%
    dplyr::select(-id) %>%
    mutate(doy = lubridate::yday(date),
           year = lubridate::year(date)) 
 
  save(aq, tr, file="data/ee/tr_aq.Rda")
}else{load("data/ee/tr_aq.Rda")}

# figuring out peak green dates

mean_by_day <- st_set_geometry(tr, NULL) %>%
  rbind(st_set_geometry(aq, NULL)) %>%
  filter(study == "bm" | study == "js") %>%
  group_by(year, doy) %>%
  summarise(ndvi = mean(ndvi)) %>%
  ungroup 

maxes <- mean_by_day %>%
  group_by(year) %>%
  summarise(ndvi = max(ndvi)) %>%
  ungroup


peak_green <- left_join(maxes, 
                        mean_by_day, 
                        by = c("year", "ndvi")) %>%
  mutate(peak_green_date = as.Date(paste(year, doy), format = "%Y %j")) %>%
  left_join(x=sample_dates, y=., by="year") %>%
  mutate(days_after_green = (sample_date - peak_green_date) %>% as.numeric)

save(peak_green, file= "data/peak_green.Rda")
# # plotting ===================================================================
# aq %>%
#   filter( year<2020, year>2015)%>%
#   ggplot(aes(x=doy, y=ndvi)) +
#   geom_smooth() +
#   geom_vline(data = peak_green %>%
#                filter(year %in% 2016:2019), 
#              aes(xintercept = doy), lty=2)+
#   facet_wrap(~year)
# 
# tr %>%
#   filter(study == "bm", str_sub(plot,7,7) == "S", year<2020, year>2015)%>%
#   ggplot(aes(x=doy, y=ndvi)) +
#   geom_smooth(aes(group=plot)) +
#   facet_wrap(~year)+
#   geom_vline(data = peak_green %>%
#                filter(year %in% 2016:2019), 
#              aes(xintercept = doy), lty=2)





