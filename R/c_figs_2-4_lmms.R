libs<- c("ggplot2", "ggpubr", "lme4", "lmerTest", "MuMIn", "broom")

iini <-function(x){
  #stands for install if not installed
  if (!x %in% rownames(installed.packages())) install.packages(x)
}
lapply(libs, iini)
lapply(libs, library, character.only = TRUE, verbose = FALSE)
source("R/a_data_prep.R")

theme_set(theme_classic())

# figure 2 ---------------------------------------------------------------------
f2_data <- rbind(ff %>% dplyr::select(Plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "ff",
                          preds = predict(lm(mass_gm2~cover_pct,.)),
                          r2 = summary(lm(mass_gm2~cover_pct,.))$r.squared),
                 js %>% dplyr::select(Plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "js",
                          preds = predict(lm(mass_gm2~cover_pct,.)),
                          r2 = summary(lm(mass_gm2~cover_pct,.))$r.squared), 
                 bm17%>% dplyr::select(Plot=plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "bm17",
                          preds = predict(lm(mass_gm2~cover_pct,.)),
                          r2 = summary(lm(mass_gm2~cover_pct,.))$r.squared),
                 bm18%>% dplyr::select(Plot=plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "bm18",
                          preds = predict(lm(mass_gm2~cover_pct,.)),
                          r2 = summary(lm(mass_gm2~cover_pct,.))$r.squared),
                 bm19%>% dplyr::select(Plot=plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "bm19",
                          preds = predict(lm(mass_gm2~cover_pct,.)),
                          r2 = summary(lm(mass_gm2~cover_pct,.))$r.squared))

# add in regression equation
ggplot(f2_data, aes(x=cover_pct, y=mass_gm2)) +
  geom_point() +
  geom_line(aes(y=preds)) +
  facet_wrap(~study, ncol = 1) +
  geom_label(aes(x=0,y=200, label = paste("R2: ",round(r2,2))),
             hjust = "left")+
  ggsave("figures/figure_2_panel.png", height = 10, width=4)


# figure 3 ---------------------------------------------------------------------
all_ps <- filter(all, study != "id18", study != "c19", study != "w19")

x <- lmer(mass_gm2 ~ 0 + cover_pct +(1| study), na.omit(all_ps))
sx <- summary(x)
rx <- r.squaredLR(x)

# add in regression equation

ggplot(na.omit(all_ps), aes(x=cover_pct, y=mass_gm2, color = study)) +
  geom_point() +
  geom_abline(slope = sx$coefficients[1], color = "red", lwd=1) +
  ggtitle(paste("All studies. pseudo R2 = ", signif(rx[1],2))) +
  theme_pubr() +
  theme(plot.title = element_text(size = 12))  +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = 'transparent'))+
  ggsave("figures/figure_3_lmm_line.png",
         width=6,height=6, limitsize = FALSE);p7

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
