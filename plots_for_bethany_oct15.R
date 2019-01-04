libs<- c("ggplot2", "ggpubr", "lme4", "lmerTest")

lapply(libs, library, character.only = TRUE)

source("data_prep.R")

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

bm <- read.csv("data/bm_site_data_w_biomass.csv") %>%
  rename(mass_gm2 = a_grass_gm2,
         cover_pct = a_grass_cover) %>%
  mutate(Year = as.factor(sample_year))

all <- rbind(mutate(dplyr::select(ff, cover_pct, mass_gm2),study = "ff16"),
             mutate(dplyr::select(js, cover_pct, mass_gm2),study = "js16"),
             mutate(dplyr::select(bm17, cover_pct, mass_gm2),study = "bm17"),
             mutate(dplyr::select(bm18, cover_pct, mass_gm2),study = "bm18"),
             mutate(dplyr::select(idaho_2018, cover_pct, mass_gm2),study = "id18"))

s1<-summary(lm(mass_gm2~cover_pct,ff))
s2<-summary(lm(mass_gm2~cover_pct,js))
s3<-summary(lm(mass_gm2~cover_pct,bm17))
s33<-summary(lm(mass_gm2~cover_pct,bm18))
l3 <- lm(mass_gm2~0+cover_pct*Year, bm)
s4<-summary(lm(mass_gm2~cover_pct,idaho_2018))
s5<-summary(lm(mass_gm2~cover_pct,not_idaho_2018))
#s4<-summary(lm(log(mass_g)~cover_pct*observers +max_ht_cm,beautiful_clean_thing))

p1 <- ggplot(ff, aes(x=cover_pct, y=mass_gm2)) +
  geom_point() +
  ggtitle(paste("Biomass from 0.1 m2 Subset of 1m2 Cover Quadrats.\nCover 9m2, Biomass 0.9 m2. R2 = ", round(s1$r.squared,2))) +
  #ylab("Cheatgrass Biomass (g/m2)") +
  #xlab("Cheatgrass Cover (%)")+
  xlab(NULL)+
  ylab(NULL)+
  theme_bw() +
  theme(plot.title = element_text(size = 12))

p2 <- ggplot(js, aes(x=cover_pct, y = mass_gm2)) +
  geom_point() +
  ggtitle(paste("Cover, Biomass from identical Quadrats.\n(2.2 m2)  R2 = ", round(s2$r.squared,2))) +
  ylab("Cheatgrass Biomass (g/m2)") +
  #xlab("Cheatgrass Cover (%)") +
  xlab(NULL)+
  # ylab(NULL)+
  theme_bw() +
  theme(plot.title = element_text(size = 12))

p3 <- ggplot(bm, aes(x=cover_pct, y=mass_gm2, color = Year, shape=Year)) +
  geom_point(size=2) +
  #geom_smooth(aes(), method="lm", se=F)+
  scale_color_manual(values = c("black", "grey40"))+
  ggtitle(paste("(0.5 m2)  2017 R2 = ", round(s3$r.squared,2),
                "   2018 R2 = ", round(s33$r.squared,2))) +
  ylab("Cheatgrass Biomass (g/m2)") +
  xlab("Cheatgrass Cover (%)")+
  theme_bw() +
  theme(plot.title = element_text(size = 12)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = 'transparent'), 
        legend.title = element_blank())

p4 <- ggplot(idaho_2018, aes(x=cover_pct, y=mass_gm2)) +
  geom_point() +
  ggtitle(paste("Cover 1 m2, Mass 0.1 m2. R2 = ", round(s4$r.squared,2))) +
  #ylab("Cheatgrass Biomass (g/m2)") +
  xlab("Cheatgrass Cover (%)") +
  #xlab(NULL)+
  ylab(NULL)+
  theme_bw() +
  theme(plot.title = element_text(size = 12))

ggarrange(p2,p1,p3,p4)
ggsave("panel.png",limitsize = FALSE, width = 7.5, height = 6)

p5 <- ggplot(not_idaho_2018, aes(x=cover_pct, y=mass_gm2)) +
  geom_point() +
  geom_smooth(method="lm") +
  #geom_line(aes(y=preds))+
  ggtitle(paste("Cover 1 m2, Mass 0.1 m2. R2 = ", round(s4$r.squared,2),
                "\nRandom Placement of Cover Frame")) +
  theme_bw() +
  theme(plot.title = element_text(size = 12)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = 'transparent'), 
        legend.title = element_blank())


w <- lm(mass_gm2~0+cover_pct, data = na.omit(all))
sw <- summary(w)
p6 <- ggplot(na.omit(all), aes(x=cover_pct, y=mass_gm2, color = study)) +
  # geom_abline(slope=1, intercept = 0)+
  geom_point() +
  #geom_smooth(method="lm") +
  geom_line(aes(y=predict(w)))+
  ggtitle(paste("All Studies. R2 = ", round(sw$r.squared,2), "slope = ",as.numeric(w$coefficients),
                "\nLinear Model with No Interactions")) +
  theme_bw() +
  theme(plot.title = element_text(size = 12)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = 'transparent'))+
  ggsave("all_together_oneline.png", width=6,height=6,limitsize = FALSE)

x <- lmer(mass_gm2 ~ 0 + cover_pct +(cover_pct -1| study), na.omit(all))
#x <- lm(mass_gm2~0+cover_pct*study, data = na.omit(all))
sx <- summary(x)
rx <- r.squaredLR(x)
p7 <- ggplot(na.omit(all), aes(x=cover_pct, y=mass_gm2, color = study)) +
  # geom_abline(slope=1, intercept = 0)+
  geom_point() +
  #geom_smooth(method="lm") +
  geom_line(aes(y=predict(x)))+
  ggtitle(paste("All studies. pseudo R2 = ", signif(rx[1],2),
                "\nLMM with random slopes, fixed intercept")) +
  theme_bw() +
  theme(plot.title = element_text(size = 12))  +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = 'transparent'))+
  ggsave("all_together_manylines.png",width=6,height=6, limitsize = FALSE)

y <- lm(mass_gm2~0+poly(cover_pct,2)*study, data = na.omit(all))
sy <- summary(y)
p8 <- ggplot(na.omit(all), aes(x=cover_pct, y=mass_gm2, color = study)) +
  # geom_abline(slope=1, intercept = 0)+
  geom_point() +
  #geom_smooth(method="lm") +
  geom_line(aes(y=predict(y)))+
  ggtitle(paste("All studies. R2 = ", signif(sy$r.squared,2),
                "\nLinear Model with Percent Cover:Study Interaction")) +
  theme_bw() +
  theme(plot.title = element_text(size = 12)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = 'transparent'))+
  ggsave("all_together_manycurves.png",width=6,height=6, limitsize = FALSE)


polymod <- lm(mass_gm2~poly(cover_pct,2), data = na.omit(all))


bmmass <- bm %>%
  select(-X, -latitude,-longitude,-slope,-aspect,-date,-sample_year) %>%
  rename(forb = forb_gm2, p_grass = p_grass_gm2, a_grass = mass_gm2) %>%
  gather(key=life_form, value = biomass_gm2, -plot,-Year,-type) %>%
  filter(life_form == "forb" | life_form == "p_grass" | life_form == "a_grass")

bmshort <- bm %>%
  select(-X, -latitude,-longitude,-slope,-aspect,-date,-sample_year) %>%
  rename(forb = forb_cover, p_grass = p_grass_cover, a_grass = cover_pct) %>%
  gather(key=life_form, value = cover_pct, -plot,-Year,-type) %>%
  filter(life_form == "a_grass" | life_form == "p_grass" | life_form == "forb") %>%
  left_join(bmmass)



p9 <- ggplot(bmshort, aes(x=cover_pct, y=biomass_gm2, color=life_form, shape=Year)) +
  # geom_abline(slope=1, intercept = 0)+
  geom_smooth(method="lm") +
  facet_wrap(~type,scales = "free")+
  geom_point(size=2) +
  ggtitle(paste("Biomass study, all functional groups")) +
  theme_bw() ;p9

# for next time
# enter data for bm 2018 check
# fig with all data diff col points but one line - through 0 w/R2 check
# fig with all data and 5 lines - through 0 w/R2 kinda check
# pretty map -- only idaho 2018 and my 4 not yet but whatever that's easy

# pilliod & arkles papers on the performance of different sampling methods
# could be good to bring in
# underestimation of cover leads to higher slopes, underestimation of biomass 
# leads to lower slopes -- esp interesting given the higher cover estimates tend
# to go back up towards the 1:1 curves. also suggests that there may be a tentency
# to underestiate the influence of bare patches on overall cover. 
# also, the three 1:1 studies are all just Adam Mahood estimating the cover, so 
# if Adam Mahood always goes low on his cover estimates that could be a prob.
# that said the biomass was clipped by 4-5 different people in my studies, so 
# a systematic bias like that is less likely
