# figuring out how many quadrat-level plots there are

newplots<- st_read("data/plots_w_elevation/") %>%
  filter(study != "ff" & study != "bm" & study != "js")

st_coordinates(newplots) %>% unique() %>% nrow