# setup ------------------------------------------------------------------------
library(tidyverse)

options(stringsAsFactors = FALSE)
# load in the data -------------------------------------------------------------
if(!file.exists( "data/all_4_years.csv")){
  pers <- read.csv("data/cheatgrass_personnel - Sheet1.csv") %>% 
    as_tibble() %>%
    rename(western_gb = western.Great.Basin,
           idaho = Idaho,
           utah = Utah,
           central_gb = central.Great.Basin) %>%
    gather(key = region, value = observers,-X) %>%
    rename(year = X)
    
  d_19<- read_csv("data/cheatgrass_2019.csv") %>%
      dplyr::select(plot, region, cover_pct= cover_hoop, mass_g) %>%
      mutate(year = 2019)
  
  d_18 <- read_csv("data/idaho_cheatgrass_bm_2018 - Sheet1.csv") %>% 
    as_tibble() %>%
    dplyr::select(plot, region, cover_pct, mass_g) %>%
    group_by(region, plot) %>%
    summarise_all(mean) %>%
    mutate(year = 2018)%>%
    ungroup()
    #mutate(max_ht_cm = max_ht_cm *100) #%>%
    #left_join(filter(pers, year == 2018))
  
  d_17 <- read_csv("data/cg_mass_cover_2017 - Sheet1.csv") %>% 
    as_tibble() %>%
    dplyr::select(-position, -date,-starts_with("UTM"),-max_ht_cm) %>%
    group_by(region, plot) %>%
    summarise_all(mean) %>%
    ungroup() %>%
    mutate(year = 2017)
   # left_join(filter(pers,year==2017))
  
  d_16 <- read_csv("data/BRTE_mass-cover-ht_10_11_16 - Sheet1.csv") %>% 
    as_tibble() %>%
    dplyr::select(-position, -`Date(mm/dd/yy)`,-max_ht_cm) %>%
    group_by(region, plot) %>%
    summarise_all(mean) %>%
    ungroup() %>%
    #left_join(filter(pers, year == 2016)) %>%
    mutate(plot = as.character(plot),
           year = 2016)
  
  beautiful_clean_thing <- rbind(d_16, d_17,d_18, d_19) %>%
    na.omit() %>%
    mutate(year = as.character(year))
  
  write_csv(beautiful_clean_thing, "data/all_4_years.csv")
}else{
 beautiful_clean_thing <- read_csv("data/all_4_years.csv")
}

