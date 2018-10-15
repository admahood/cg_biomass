libs<- c("ggplot2", "ggpubr")

lapply(libs, library, character.only = TRUE)

source("lmms.R")


three_year <- read.csv("data/all_3_years.csv")
ff <- read.csv("data/BRTE_mass_x_plot_pt9_m2_per_sample_2016.csv")
js <- read.csv("data/BRTE_cover_mass_2pt2_m2_per_sample_2016.csv")
bm <- read.csv("data/bm_site_data_w_biomass.csv")

s1<-summary(lm(BRTE_mass~BRTE_cover,ff))
s2<-summary(lm(Mass~BRTE,js))
s3<-summary(lm(a_grass_gm2~a_grass_cover,bm))
s4<-summary(lm(log(mass_g)~cover_pct,beautiful_clean_thing))
#s4<-summary(lm(log(mass_g)~cover_pct*observers +max_ht_cm,beautiful_clean_thing))

p1 <- ggplot(ff, aes(x=BRTE_cover, y=BRTE_mass)) +
  geom_point() +
  ggtitle(paste("Cover: 9 m2, Mass 0.9 m2. R2 = ", round(s1$r.squared,2))) +
  ylab("Cheatgrass Biomass (g)") +
  xlab("Cheatgrass Cover (%)")+
  theme_bw()

p2 <- ggplot(js, aes(x=BRTE, y = Mass)) +
  geom_point() +
  ggtitle(paste("Cover: 2.2 m2, Mass 2.2 m2. R2 = ", round(s2$r.squared,2))) +
  ylab("Cheatgrass Biomass (g)") +
  xlab("Cheatgrass Cover (%)")+
  theme_bw()

p3 <- ggplot(bm, aes(x=a_grass_cover, y=a_grass_gm2)) +
  geom_point() +
  ggtitle(paste("Cover: 0.5 m2, Mass 0.5 m2. R2 = ", round(s3$r.squared,2))) +
  ylab("Annual Grass Biomass (g)") +
  xlab("Annual Grass Cover (%)")+
  theme_bw()

p4 <- ggplot(beautiful_clean_thing, aes(x=cover_pct, y=sqrt(mass_g))) +
  geom_point() +
  #geom_smooth(method="lm") +
  #geom_line(aes(y=preds))+
  ggtitle(paste("Cover 1 m2, Mass 0.1 m2. R2 = ", round(s4$r.squared,2),
                "\n0.67 with height+cover*observers")) +
  theme_bw()

ggarrange(p1,p2,p3,p4)
ggsave("panel.png",limitsize = FALSE)
