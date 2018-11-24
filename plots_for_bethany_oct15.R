libs<- c("ggplot2", "ggpubr")

lapply(libs, library, character.only = TRUE)

source("lmms.R")

idaho_2018 <- read.csv("data/all_3_years.csv") %>%
  filter(region == "idaho", year == "2018") %>%
  mutate(mass_gm2 = mass_g*10)

ff <- read.csv("data/BRTE_mass_x_plot_pt9_m2_per_sample_2016.csv") %>%
  mutate(mass_gm2 = BRTE_mass/0.9)
js <- read.csv("data/BRTE_cover_mass_2pt2_m2_per_sample_2016.csv") %>%
  mutate(mass_gm2 = Mass/2.2)
bm <- read.csv("data/bm_site_data_w_biomass.csv")

s1<-summary(lm(mass_gm2~BRTE_cover,ff))
s2<-summary(lm(mass_gm2~BRTE,js))
s3<-summary(lm(a_grass_gm2~a_grass_cover,bm))
s4<-summary(lm(mass_gm2~cover_pct,idaho_2018))
#s4<-summary(lm(log(mass_g)~cover_pct*observers +max_ht_cm,beautiful_clean_thing))

p1 <- ggplot(ff, aes(x=BRTE_cover, y=mass_gm2)) +
  geom_point() +
  ggtitle(paste("Cover: 9 m2, Mass 0.9 m2. R2 = ", round(s1$r.squared,2))) +
  ylab("Cheatgrass Biomass (g)") +
  xlab("Cheatgrass Cover (%)")+
  theme_bw() +
  theme(plot.title = element_text(size = 12))

p2 <- ggplot(js, aes(x=BRTE, y = mass_gm2)) +
  geom_point() +
  ggtitle(paste("Cover: 2.2 m2, Mass 2.2 m2. R2 = ", round(s2$r.squared,2))) +
  ylab("Cheatgrass Biomass (g)") +
  xlab("Cheatgrass Cover (%)") +
  theme_bw() +
  theme(plot.title = element_text(size = 12))


p3 <- ggplot(bm, aes(x=a_grass_cover, y=a_grass_gm2)) +
  geom_point() +
  ggtitle(paste("Cover: 0.5 m2, Mass 0.5 m2. R2 = ", round(s3$r.squared,2))) +
  ylab("Annual Grass Biomass (g)") +
  xlab("Annual Grass Cover (%)")+
  theme_bw() +
  theme(plot.title = element_text(size = 12))

p4 <- ggplot(beautiful_clean_thing, aes(x=cover_pct, y=mass_gm2)) +
  geom_point() +
  #geom_smooth(method="lm") +
  #geom_line(aes(y=preds))+
  ggtitle(paste("Cover 1 m2, Mass 0.1 m2. R2 = ", round(s4$r.squared,2),
                "\n0.67 with height+cover*observers")) +
  theme_bw() +
  theme(plot.title = element_text(size = 12))


ggarrange(p1,p2,p3,p4)
ggsave("panel.png",limitsize = FALSE, width = 7.5, height = 6)


summary(lm(mass_gm2~poly(cover_pct,2), data = idaho_2018))
