# setup ------------------------------------------------------------------------
library(tidyverse)

options(stringsAsFactors = FALSE)
# load in the data -------------------------------------------------------------
pers <- read.csv("data/cheatgrass_personnel - Sheet1.csv") %>% 
  as_tibble() %>%
  rename(western_gb = western.Great.Basin,
         idaho = Idaho,
         utah = Utah,
         central_gb = central.Great.Basin) %>%
  gather(key = region, value = observers,-X) %>%
  rename(year = X)

d_18 <- read.csv("data/idaho_cheatgrass_bm_2018 - Sheet1.csv") %>% 
  as_tibble() %>%
  dplyr::select(-date, -starts_with("sampling"), -starts_with("local"),
                -plot.1, -starts_with("UTM"),-notes, -canyon, 
                -starts_with("direction")) %>%
  group_by(region, plot) %>%
  summarise_all(mean) %>%
  mutate(max_ht_cm = max_ht_cm *100) %>%
  left_join(filter(pers, year == 2018))

d_17 <- read.csv("data/cg_mass_cover_2017 - Sheet1.csv") %>% 
  as_tibble() %>%
  dplyr::select(-position, -date,-starts_with("UTM")) %>%
  group_by(region, plot) %>%
  summarise_all(mean) %>%
  left_join(filter(pers,year==2017))

d_16 <- read.csv("data/BRTE_mass-cover-ht_10_11_16 - Sheet1.csv") %>% 
  as_tibble() %>%
  dplyr::select(-position, -Date.mm.dd.yy.) %>%
  group_by(region, plot) %>%
  summarise_all(mean) %>%
  left_join(filter(pers, year == 2016)) %>%
  mutate(plot = as.character(plot))

beautiful_clean_thing <- rbind(d_16, d_17,d_18) %>%
  na.omit() %>%
  mutate(year = as.factor(year))
write.csv(beautiful_clean_thing, "data/all_3_years.csv")

