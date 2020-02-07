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
f2_data <- rbind(
  # ff %>% dplyr::select(Plot, cover_pct, mass_gm2)%>%
  #                  na.omit()%>%
  #                  mutate(study = "ff",
  #                         preds = predict(lm(mass_gm2~cover_pct,.)),
  #                         r2 = summary(lm(mass_gm2~cover_pct,.))$r.squared),
                 js %>% dplyr::select(Plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "js"), 
                 bm17%>% dplyr::select(Plot=plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "bm17"),
                 bm18%>% dplyr::select(Plot=plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "bm18"),
                 bm19%>% dplyr::select(Plot=plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "bm19"))
f2_data %>%
  nest(-study) %>% 
  mutate(model = map(data,~lm(mass_gm2~0+cover_pct, data = .x)),
         adj.r.squared = map_dbl(model, ~ signif(summary(.x)$r.squared, 3)),
         slope = map_dbl(model, ~signif(summary(.x)$coefficients[1],3)),
         p=map_dbl(model, ~ signif(summary(.x)$coefficients[4],3)))  %>% 
  select(-data, -model) %>% 
  left_join(f2_data) %>% 
  mutate(p = ifelse(p < 0.05, "p < 0.05", "p > 0.05"))%>%
  ggplot(aes(x=cover_pct, y=mass_gm2)) +
  geom_point() +
  ylab(expression(Mass~(g~m^-2))) +
  xlab("Percent Cover") +
  geom_smooth(method="lm", se=FALSE) +
  facet_wrap(~study, ncol = 1) +
  geom_text(aes(0, 200, label = paste("R2 = ",adj.r.squared, 
                                     "\n",p,
                                     "\ny = 0 + ", slope,"x")), 
            hjust="left", vjust="top")+
  ggsave("figures/figure_2_panel.png", height = 10, width=4)

# figure 3 ---------------------------------------------------------------------
mod_f3 <- lm(mass_gm2 ~ 0 + cover_pct, f2_data)

preds <- predict(mod_f3, interval = "confidence", level=.95) %>%
  as_tibble 
mm <- mod_f3$coefficients %>% round(2) %>% as.numeric

f2_data<- f2_data %>%
  cbind(preds) 
f2_data$slope=mm

ggplot(f2_data, aes(x=cover_pct, y=mass_gm2, color = study)) +
  geom_point() +
  geom_line(aes(y=fit), color="red")+
  geom_line(aes(y=upr), color="red", lty=2)+
  geom_line(aes(y=lwr), color="red", lty=2)+
  ylab(expression(Mass~(g~m^-2))) +
  xlab("Percent Cover") +
  geom_text(aes(0, 180, label = paste0("y = ", slope, "x + 0")),
            parse=FALSE, hjust="left", color="black")+
  geom_text(aes(0, 170, label = paste(expression(R^2~0.89))),
            parse=TRUE,hjust="left", color="black")+
  theme(plot.title = element_text(size = 12))  +
  theme(legend.justification=c(1,0), legend.position=c(1,0),
        legend.background = element_rect(fill = 'transparent'))+
  ggsave("figures/figure_3_lmm_line.png",
         width=6,height=6, limitsize = FALSE)


# figure for 1m2 plots----------------------------------------------------------
bct <- beautiful_clean_thing %>%
  mutate(study = str_to_title(paste0(region, " ", year)))%>% 
  filter(study != "Utah 2016") %>%
  rbind(beautiful_clean_thing %>% mutate(study = "All Together"))

bct %>%
    nest(-study) %>% 
    mutate(model = map(data,~lm(mass_g~0+cover_pct, data = .x)),
           adj.r.squared = map_dbl(model, ~ signif(summary(.x)$r.squared, 2)),
           slope = map_dbl(model, ~signif(summary(.x)$coefficients[1],2)),
           p=map_dbl(model, ~ signif(summary(.x)$coefficients[4],3)))  %>% 
    select(-data, -model) %>% 
    left_join(bct) %>% 
    mutate(p = ifelse(p < 0.05, "p < 0.05", "p > 0.05"))%>%
    ggplot(aes(x=cover_pct, y=mass_g)) +
    geom_point() +
    geom_smooth(se = FALSE, method = "lm") +
    ylab(expression(Mass~(g~m^-2))) +
    xlab("Percent Cover") +
    facet_wrap(~study) +
    geom_text(aes(3, 40, label = paste("R2 = ",adj.r.squared, 
                                       "\n",p,
                                       "\ny = 0 + ", slope,"x")), 
              hjust="left") +
  ggsave("figures/one_meter_sq_plots.png", height = 10, width = 10)

# subplot vs plot level

bm_agg <- bm_sp %>%
  mutate(plot = str_sub(plot, 1,5))%>%
  group_by(plot) %>%
  summarise(cover_pct = sum(cover_pct)/5,
            mass_gm2 = sum(mass_gm2)/5,
            subplot = "plot",
            scale = "plot") %>%
  ungroup() %>%
  rbind(bm_sp %>% 
          dplyr::select(-biomass,-type) %>%
          mutate(scale = "subplot"))

bm_agg %>%
  nest(-scale) %>% 
  mutate(model = map(data,~lm(mass_gm2~0+cover_pct, data = .x)),
         adj.r.squared = map_dbl(model, ~ signif(summary(.x)$r.squared, 2)),
         slope = map_dbl(model, ~signif(summary(.x)$coefficients[1],2)),
         p=map_dbl(model, ~ signif(summary(.x)$coefficients[4],3)))  %>% 
  select(-data, -model) %>% 
  left_join(bm_agg) %>% 
  mutate(p = ifelse(p < 0.05, "p < 0.05", "p > 0.05"))%>%
  ggplot(aes(x=cover_pct, y=mass_gm2)) +
    geom_point() +
    geom_smooth(method="lm", se=F)+
    geom_text(aes(0, 150, label = paste("R2 = ",adj.r.squared, 
                                       "\n",p,
                                       "\ny = 0 + ", slope,"x")), 
              hjust="left")+
    facet_wrap(~scale) +
  ggsave("figures/plot_vs_subplot.png", height =5, width=6)
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
