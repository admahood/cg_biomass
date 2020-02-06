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

# data wrangling ---------------------------------------------------------------

idaho_2018 <- read.csv("data/all_3_years.csv") %>%
  filter(region == "idaho", year == "2018") %>%
  mutate(mass_gm2 = mass_g*10)

not_idaho_only2018 <- read.csv("data/all_3_years.csv") %>%
  filter(year == "2018" & region != "idaho") %>%
  mutate(mass_gm2 = mass_g*10)

not_idaho_2018 <- read.csv("data/all_3_years.csv") %>%
  filter(year != "2018")%>%
  mutate(mass_gm2 = mass_g*10) %>%
  rbind(not_idaho_only2018)

western_2019 <- read_csv("data/all_4_years.csv") %>%
  filter(year == "2019", region == "western") %>%
  mutate(mass_gm2 = mass_g*10)

central_2019 <- read_csv("data/all_4_years.csv") %>%
  filter(year == "2019", region == "central") %>%
  mutate(mass_gm2 = mass_g*10)


ff <- read.csv("data/BRTE_mass_x_plot_pt9_m2_per_sample_2016.csv") %>%
  mutate(mass_gm2 = BRTE_mass/0.9) %>%
  rename(cover_pct = BRTE_cover)
js <- read.csv("data/BRTE_cover_mass_2pt2_m2_per_sample_2016.csv") %>%
  mutate(mass_gm2 = Mass/2.2) %>%
  rename(cover_pct = BRTE)

bm17 <- read.csv("data/bm_site_data_w_biomass.csv") %>%
  rename(mass_gm2 = a_grass_gm2,
         cover_pct = a_grass_cover) %>%
  filter(sample_year == 2017)

bm18 <- read.csv("data/bm_site_data_w_biomass.csv") %>%
  rename(mass_gm2 = a_grass_gm2,
         cover_pct = a_grass_cover) %>%
  filter(sample_year == 2018)

bm19 <- read.csv("data/site_bm_data_w_biomass_19.csv") %>%
  rename(mass_gm2 = a_grass_gm2,
         cover_pct = a_grass_cover) %>%
  filter(sample_year == 2019)

bm <- read.csv("data/site_bm_data_w_biomass_19.csv") %>%
  rename(mass_gm2 = a_grass_gm2,
         cover_pct = a_grass_cover) %>%
  mutate(Year = as.factor(sample_year))

bm_sp <- read_csv("data/bm_subplot_2019.csv") %>%
  rename(cover_pct = cover) %>%
  mutate(mass_gm2 = biomass *10, 
         plot = str_c(plot, "_", subplot))

all <- rbind(mutate(dplyr::select(ff, cover_pct, mass_gm2),study = "ff16"),
             mutate(dplyr::select(js, cover_pct, mass_gm2),study = "js16"),
             mutate(dplyr::select(bm17, cover_pct, mass_gm2),study = "bm17"),
             mutate(dplyr::select(bm18, cover_pct, mass_gm2),study = "bm18"),
             mutate(dplyr::select(bm_sp, cover_pct, mass_gm2),study = "bm19"),
             mutate(dplyr::select(idaho_2018, cover_pct, mass_gm2),study = "id18"),
             mutate(dplyr::select(western_2019, cover_pct, mass_gm2),study = "w19"),
             mutate(dplyr::select(central_2019, cover_pct, mass_gm2),study = "c19"))

