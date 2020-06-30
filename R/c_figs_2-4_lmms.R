libs<- c("ggplot2", "ggpubr", "lme4", "lmerTest", "MuMIn", "broom")

iini <-function(x){
  #stands for install if not installed
  if (!x %in% rownames(installed.packages())) install.packages(x)
}
lapply(libs, iini)
lapply(libs, library, character.only = TRUE, verbose = FALSE)
source("R/a_data_prep.R")

theme_set(theme_classic())

# conceptual figure ------------------------------------------------------------
a<-120 #asymptote
b<- 1.8 #horizontal displacement
c<- .024
f1 <- function(x) (a*exp(-1*b*exp(-1*c*x)))-20

ggplot(data.frame(x = c(0, 140)), aes(x)) + 
  stat_function(fun = f1, n = 1000, color = "grey30") +
  coord_cartesian(ylim = c(0, 120)) +
  geom_segment(x=0, xend=130, y=0, yend=130, color = "red")+
  theme(axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_hline(yintercept=100, lty=2)+
  xlab("Density") +
  ylab("Abundance Measures") +
  geom_text(x=55, y=80, label="Biomass", color="red") +
  geom_text(x=95, y=70, label = "Cover") +
  geom_text(x=1, y=105, label = "100% Cover", hjust = "left")+
  ggsave("figures/conceptual_figure.png", width=3.5, height=3.5)

# figure 2 ---------------------------------------------------------------------
f2_data <- rbind(js %>% dplyr::select(Plot=plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "June 2016. n = 59"), 
                 bm17%>% dplyr::select(Plot=plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "July 2017. n = 40"),
                 bm18%>% dplyr::select(Plot=plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "July 2018. n = 40"),
                 bm19%>% dplyr::select(Plot=plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "September 2019. n = 40")) %>%
  mutate(study = factor(study, levels = c("June 2016. n = 59",
                                          "July 2017. n = 40",
                                          "July 2018. n = 40",
                                          "September 2019. n = 40")))
xd<-f2_data %>%
  nest(-study) %>% 
  mutate(model = map(data,~lm(mass_gm2~0+cover_pct, data = .x)),
         adj.r.squared = map_dbl(model, ~ signif(summary(.x)$r.squared, 3)),
         slope = map_dbl(model, ~signif(summary(.x)$coefficients[1],3)),
         p=map_dbl(model, ~ signif(summary(.x)$coefficients[4],3)))  %>% 
  select(-data, -model) %>% 
  left_join(f2_data) %>% 
  mutate(p = ifelse(p < 0.05, "p < 0.05", "p > 0.05"))

# xdp<-f2_data %>%
#   nest(-study) %>% 
#   mutate(model = map(data,~lm(mass_gm2~0+poly(cover_pct,2), data = .x)),
#          adj.r.squared = map_dbl(model, ~ signif(summary(.x)$r.squared, 3)),
#          slope = map_dbl(model, ~signif(summary(.x)$coefficients[1],3)),
#          p=map_dbl(model, ~ signif(summary(.x)$coefficients[4],3)))  %>% 
#   select(-data, -model) %>% 
#   left_join(f2_data) %>% 
#   mutate(p = ifelse(p < 0.05, "p < 0.05", "p > 0.05"))
  
ggplot(xd, aes(x=cover_pct, y=mass_gm2)) +
  geom_point() +
  ylab(expression(Aboveground~Biomass~(g~m^-2))) +
  xlab("Percent Cover") +
  geom_smooth(method="lm", se=TRUE, color = "black") +
  facet_wrap(~study, ncol = 1) +
  geom_text(aes(0, 200), label = expression(R^2))+
  geom_text(aes(0, 200, label = paste("   = ",adj.r.squared, 
                                     "\n",p,
                                     "\ny = 0 + ", slope,"x")), 
            hjust="left", vjust="top")+
  ggsave("figures/figure_2_panel.png", height = 10, width=3.5)


# figure 3 ---------------------------------------------------------------------
#first showing lmm is better
library(nlme)

f3_data <- filter(f2_data, study != "September 2019. n = 40") %>%
  mutate(study = droplevels(study))

mod_f3 <- gls(mass_gm2 ~ 0 + cover_pct, f3_data, method = "ML")
mod_f3mm <-lme(mass_gm2 ~ 0 + cover_pct, 
               control = lmeControl(maxIter = 1000, opt = "optim"),
               random = ~cover_pct|study,
               data = f3_data,method = "ML")

anova(mod_f3,mod_f3mm)

mod_f3m <-lmer(mass_gm2 ~ 0 + cover_pct + (cover_pct|study), 
              data = f3_data, REML = TRUE)

preds <- predict(mod_f3, interval = "confidence", level=.95) %>%
  as_tibble 
mm <- summary(mod_f3m)$coefficients[1] %>% round(2)

#elevation exploration -- elevation not significant, doesn't lower the AIC
# plots_w_elevation <- st_read("data/plots_w_elevation.gpkg")
# 
# mod_f3_elev <-lmer(mass_gm2 ~ 0 + cover_pct + Elevation + (cover_pct|study), 
#                data = plots_w_elevation, REML = FALSE)
# summary(mod_f3_elev)
# AIC(mod_f3m, mod_f3_elev)
# xx<-effects::Effect("cover_pct", mod_f3m, se=T)

f3_data<- f3_data %>%
  cbind(preds) 
f3_data$slope=mm

rr<- r.squaredLR(mod_f3m)[1]

# colorz<- c("#273253", "#ED5C4D", "#FBBE4B")

ggplot(f3_data, aes(x=cover_pct, y=mass_gm2, color = study)) +
  geom_point() +
  geom_line(aes(y=predict(mod_f3m)))+
  geom_abline(slope = mm, color="red", lwd=0.75)+
  ylab(expression(Aboveground~Biomass~(g~m^-2))) +
  xlab("Percent Cover") +
  geom_text(aes(0, 180, label = paste0("y = ", slope, "x + 0")),
            parse=FALSE, hjust="left", color="black")+
  geom_text(aes(0, 170, label = paste(expression(Pseudo~R^2:~0.81))),
            parse=TRUE,hjust="left", color="black")+
  theme(plot.title = element_text(size = 12))  +
  scale_color_brewer(palette = "Accent")+
  theme(legend.justification=c(1,0), 
        legend.position=c(1,0),
        legend.title = element_blank(),
        legend.background = element_rect(fill = 'transparent'))+
  ggsave("figures/figure_3_lmm_line.png",
         width=4,height=4, limitsize = FALSE)


# figure for 1m2 plots----------------------------------------------------------
bct <- beautiful_clean_thing %>%
  mutate(study = str_to_title(paste0(region, " ", year)))%>% 
  filter(study != "Eastern 2016") %>%
  rbind(beautiful_clean_thing %>% 
          mutate(study = "All Regions",
                 year=NA,
                 date=NA))%>%
  mutate(mass_gm2 = mass_g*10)

dateranges <- bct %>%
  group_by(study) %>%
  summarise(firstdate = min(date, na.rm=T) %>% format("%B %d"),
            lastdate = max(date, na.rm=T) %>% format("%B %d")) %>%
  ungroup()


new_names <- bct %>%
  group_by(year, region) %>%
  summarise(firstdate = min(date, na.rm=T) %>% format("%B %d"),
            lastdate = max(date, na.rm=T) %>% format("%B %d"),
            study = first(study)) %>%
  ungroup() %>%
  mutate(study = paste0(study, ": ", firstdate, "-", lastdate)) %>%
  arrange(year, region) %>%
  pull(study) %>%
  unique

lut_names <- bct %>%
  arrange(year, region) %>%
  pull(study) %>%
  unique

lut_study <-paste0(letters[1:length(new_names)],". ", new_names)

lut_study <- lut_study %>% str_replace(": NA-NA", "")
names(lut_study) <- lut_names


bct %>%
  nest(-study) %>% 
  mutate(model = map(data,~lm(mass_gm2~0+cover_pct, data = .x)),
         adj.r.squared = map_dbl(model, ~ signif(summary(.x)$r.squared, 2)),
         slope = map_dbl(model, ~signif(summary(.x)$coefficients[1],2)),
         p=map_dbl(model, ~ signif(summary(.x)$coefficients[4],3)))  %>% 
  select(-data, -model) %>% 
  left_join(bct) %>% 
  mutate(p = ifelse(p < 0.05, "p < 0.05", "p > 0.05")) %>%
  group_by(study) %>%
  mutate(date_r = min(date, na.rm = TRUE)) %>%
  mutate(study = lut_study[study],
         n = paste("n =",n()))%>%
  ungroup() %>% 
    ggplot(aes(x = cover_pct, y = mass_gm2)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y~0+x, color = "black") +
    ylab(expression(Aboveground~Biomass~(g~m^-2))) +
    xlab("Percent Cover") +
    facet_wrap(~study, ncol=3) +
    theme(panel.border = element_rect(size=0.5, fill=NA))+
    geom_text(aes(3,465), label= expression(R^2))+
    geom_text(aes(3, 400, label = paste("  = ",adj.r.squared, 
                                       "\n",n,
                                       "\ny = 0 + ", slope,"x")), 
              hjust="left") +
  ggsave("figures/one_meter_sq_plots.png", height = 10, width = 8)

# subplot vs plot level=========================================================

bm_agg <- bm_sp %>%
  mutate(plot = str_sub(plot, 1,5))%>%
  group_by(plot) %>%
  summarise(cover_pct = sum(cover_pct, na.rm=T)/5,
            mass_gm2 = sum(mass_gm2, na.rm=T)/5,
            se = sd(biomass, na.rm=T)*1.96,
            subplot = "plot",
            scale = "b. Transect-level") %>%
  ungroup() %>%
  rbind(bm_sp %>% 
          dplyr::select(-biomass,-type) %>%
          mutate(scale = "a. Individual quadrats",
                 se = 0))

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
   ylab(expression(Aboveground~Biomass~(g~m^-2))) +
   xlab("Percent Cover") +
    geom_errorbar(aes(x=cover_pct,
                  ymax = mass_gm2 + se, ymin = mass_gm2-se))+
    geom_smooth(method="lm", se=T, color = "black")+
    geom_text(aes(0,160), label = expression(R^2))+
    geom_text(aes(0, 150, label = paste("   = ",adj.r.squared, 
                                       "\n",p,
                                       "\ny = 0 + ", slope,"x")), 
              hjust="left")+
    facet_wrap(~scale) +
  ggsave("figures/plot_vs_subplot.png", height =5, width=6)

lm_p <- lm(mass_gm2 ~ cover_pct, data = filter(bm_agg, scale == "plot"))
lm_sp <- lm(mass_gm2 ~ cover_pct, data = filter(bm_agg, scale == "subplot"))
lmer_sp <- lmer(mass_gm2 ~ cover_pct + (1|plot), REML = T,
                data = filter(bm_agg, scale == "subplot") %>%
                  mutate(plot = str_sub(plot,1,5)))
summary(lm_p)
summary(lm_sp)
summary(lmer_sp)
AIC(lm_sp, lmer_sp)
