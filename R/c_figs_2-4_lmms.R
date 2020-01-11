libs<- c("ggplot2", "ggpubr", "lme4", "lmerTest", "MuMIn", "broom")

iini <-function(x){
  #stands for install if not installed
  if (!x %in% rownames(installed.packages())) install.packages(x)
}
lapply(libs, iini)
lapply(libs, library, character.only = TRUE, verbose = FALSE)
source("R/a_data_prep.R")

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


ggplot(all, aes(x = cover_pct, y=mass_gm2, color = study)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap (~study)

ggplot(all, aes(x = cover_pct, y=mass_gm2, color = study)) +
  geom_point() +
  geom_smooth(method = "lm", se=F) +
  geom_segment(aes(x=0, xend = 100, y=0, yend=500), color = "black") +
  theme_pubr() +
  annotate("text", x=10, y=400, label = "underestimation of cover") +
  annotate("text", x=75, y=50, label = "overestimation of cover") +
  annotate("text", x = 75, y=450, label = "accurate?")

# figure 2 ---------------------------------------------------------------------

s1<-summary(lm(mass_gm2~cover_pct,ff))
s2<-summary(lm(mass_gm2~cover_pct,js))
s3<-summary(lm(mass_gm2~cover_pct,bm17))
s33<-summary(lm(mass_gm2~cover_pct,bm18))
s333<-summary(lm(mass_gm2~cover_pct,bm_sp))
l3 <- lm(mass_gm2~0+cover_pct*Year, bm)
s4<-summary(lm(mass_gm2~cover_pct,idaho_2018))
s5<-summary(lm(mass_gm2~cover_pct,not_idaho_2018))
s6<-summary(lm(mass_gm2~cover_pct,central_2019))
s7<-summary(lm(mass_gm2~cover_pct,western_2019))
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
  ggtitle(paste("Cover, Biomass from identical Quadrats.\nEach point represents 2.2 m2 R2 = ", round(s2$r.squared,2))) +
  ylab("Cheatgrass Biomass (g/m2)") +
  #xlab("Cheatgrass Cover (%)") +
  xlab(NULL)+
  # ylab(NULL)+
  theme_bw() +
  theme(plot.title = element_text(size = 12))

p3 <- ggplot(bm, aes(x=cover_pct, y=mass_gm2, color = Year, shape=Year)) +
  geom_point(size=2) +
  #geom_smooth(aes(), method="lm", se=F)+
  scale_color_manual(values = c("black", "grey40", "grey70"))+
  ggtitle(paste("Each point represents 0.5 m2.\n2017 R2 = ", round(s3$r.squared,2),
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
  ggtitle(paste("Cover 1 m2, Mass 0.1 m2.\n R2 = ", round(s4$r.squared,2))) +
  #ylab("Cheatgrass Biomass (g/m2)") +
  xlab("Cheatgrass Cover (%)") +
  #xlab(NULL)+
  ylab(NULL)+
  theme_bw() +
  theme(plot.title = element_text(size = 12))

ggarrange(p2,p1,p3,p4) +
ggsave("figures/figure_2_panel.png",limitsize = FALSE, width = 7.5, height = 6)

# figure 3 ---------------------------------------------------------------------
all_ps <- filter(all, study != "id18", study != "c19")

x <- lmer(mass_gm2 ~ 0 + cover_pct +(cover_pct -1| study), na.omit(all_ps))
#x <- lm(mass_gm2~0+cover_pct*study, data = na.omit(all))
sx <- summary(x)
rx <- r.squaredLR(x)
p7 <- ggplot(na.omit(all_ps), aes(x=cover_pct, y=mass_gm2, color = study)) +
  # geom_abline(slope=1, intercept = 0)+
  geom_point() +
  #geom_smooth(method="lm") +
  geom_line(aes(y=predict(x)))+
  ggtitle(paste("All studies. pseudo R2 = ", signif(rx[1],2),
                "\nLMM with random slopes, fixed intercept")) +
  theme_pubr() +
  theme(plot.title = element_text(size = 12))  +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = 'transparent'))+
  ggsave("figures/f3_whole_hog_manylines.png",width=6,height=6, limitsize = FALSE)

# figure 4 ---------------------------------------------------------------------
w <- lm(mass_gm2~0+cover_pct, data = na.omit(all_ps))
sw <- summary(w)
p6 <- ggplot(na.omit(all_ps), aes(x=cover_pct, y=mass_gm2, color = study)) +
  # geom_abline(slope=1, intercept = 0)+
  geom_point() +
  #geom_smooth(method="lm") +
  geom_line(aes(y=predict(w)), color = "red")+
  ggtitle(paste("All Studies aggregated. R2 = ", round(sw$r.squared,2), 
                ", slope = ",round(as.numeric(w$coefficients),2),
                "\nLinear Model with No Interactions")) +
  theme_bw() +
  theme(plot.title = element_text(size = 12)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = 'transparent'))+
  ggsave("figures/f4_whole_hog.png", width=6,height=6,limitsize = FALSE)

# figure s1 --------------------------------------------------------------------

id_wgb_2018 <- read.csv("data/all_3_years.csv") %>%
  filter(region == "idaho" | region == "western_gb") %>%
  mutate(region_year = paste(region, year)) %>%
  mutate(mass_gm2 = mass_g*10)

stats <- id_wgb_2018 %>%
  nest(-region_year) %>%
  mutate(fit = map(data, ~lm(mass_gm2 ~ cover_pct, data=.)),
         results = map(fit, glance)) %>%
  unnest(results)

xx = data.frame(region_year = stats$region_year, r2 = round(stats$r.squared,2))
id_wgb_2018 <- left_join(id_wgb_2018, xx)


ggplot(id_wgb_2018, aes(x=cover_pct, y=mass_gm2, color = paste(region, year))) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  scale_color_discrete(name = "Region, Year") +
  ggsave("figures/s1.png")

# figure s2

ggplot(id_wgb_2018, aes(x=cover_pct, y=mass_gm2, color = paste(region, year))) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  scale_color_discrete(name = "Region, Year") + 
  facet_wrap(~region_year) +
  ggtitle(expression(1~m^2~Samples~with~Representative~Clips))+
  geom_text(aes(label = paste("R2 =",(r2)), group=NULL), 
            size = 4, x = 15, y = 400, color = "black")+
  ggsave("figures/s2.png")

# figure s3
s3s <-summary(lm(mass_gm2~cover_pct,id_wgb_2018))

ggplot(id_wgb_2018, aes(x=cover_pct, y=mass_gm2)) +
  geom_point() +
  ggtitle(paste("All representative clips. R2 = ", round(s3s$r.squared,2)))+
  geom_smooth(method = "lm", se = F) +
  ggtitle(expression(1~m^2~Samples~with~Representative~Clips))+
  scale_color_discrete(name = "Region, Year") + 
  ggsave("figures/s3.png")

# other stuff ------------------------------------------------------------------

y <- lmer(mass_gm2 ~ 0 + poly(cover_pct,2) +(cover_pct -1| study), na.omit(all))
# y <- lm(mass_gm2~0+poly(cover_pct,2)*study, data = na.omit(all))
sy <- summary(y)
ry <- r.squaredLR(y)
p8 <- ggplot(na.omit(all), aes(x=cover_pct, y=mass_gm2, color = study)) +
  # geom_abline(slope=1, intercept = 0)+
  geom_point() +
  #geom_smooth(method="lm") +
  geom_line(aes(y=predict(y)))+
   ggtitle(paste("All studies. pseudo R2 = ", signif(ry[1],2),
                 "\nLMM with fixed intercept and random slope")) +
  theme_bw() +
  theme(plot.title = element_text(size = 12)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = 'transparent'),
        legend.title = element_blank())+
  ggsave("all_together_manycurves.png",width=6,height=6, limitsize = FALSE)

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

# all over again but with <40% -------------------------------------------------

all40 <- filter(all, cover_pct < 40)

w <- lm(mass_gm2~0+cover_pct, data = na.omit(all40))
sw <- summary(w)
p640 <- ggplot(na.omit(all40), aes(x=cover_pct, y=mass_gm2, color = study)) +
  # geom_abline(slope=1, intercept = 0)+
  geom_point() +
  #geom_smooth(method="lm") +
  geom_line(aes(y=predict(w)))+
  ggtitle(paste("All Studies. R2 = ", round(sw$r.squared,2), "slope = ",round(as.numeric(w$coefficients),2),
                "\nLinear Model with No Interactions")) +
  theme_bw() +
  theme(plot.title = element_text(size = 12)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = 'transparent'))+
  ggsave("all_together_oneline40.png", width=6,height=6,limitsize = FALSE)

x <- lmer(mass_gm2 ~ 0 + cover_pct +(cover_pct -1| study), na.omit(all40))
#x <- lm(mass_gm2~0+cover_pct*study, data = na.omit(all))
sx <- summary(x)
rx <- r.squaredLR(x)
p7 <- ggplot(na.omit(all40), aes(x=cover_pct, y=mass_gm2, color = study)) +
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
  ggsave("all_together_manylines40.png",width=6,height=6, limitsize = FALSE)

y <- lmer(mass_gm2 ~ 0 + poly(cover_pct,2) +(cover_pct -1| study), na.omit(all40))
# y <- lm(mass_gm2~0+poly(cover_pct,2)*study, data = na.omit(all))
sy <- summary(y)
ry <- r.squaredLR(y)
p8 <- ggplot(na.omit(all40), aes(x=cover_pct, y=mass_gm2, color = study)) +
  # geom_abline(slope=1, intercept = 0)+
  geom_point() +
  #geom_smooth(method="lm") +
  geom_line(aes(y=predict(y)))+
  ggtitle(paste("All studies. pseudo R2 = ", signif(ry[1],2),
                "\nLMM with fixed intercept and random slope")) +
  theme_bw() +
  theme(plot.title = element_text(size = 12)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = 'transparent'),
        legend.title = element_blank())+
  ggsave("all_together_manycurves40.png",width=6,height=6, limitsize = FALSE)



# -------
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
