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
library(nlme)
mod_f3 <- gls(mass_gm2 ~ 0 + cover_pct, f2_data, method = "ML")
mod_f3mm <-lme(mass_gm2 ~ 0 + cover_pct, 
               control = lmeControl(maxIter = 1000, opt = "optim"),
               random = ~cover_pct|study,
               data = f2_data,method = "ML")

anova(mod_f3,mod_f3mm)

mod_f3m <-lmer(mass_gm2 ~ 0 + cover_pct + (cover_pct|study), 
              # control = lmeControl(maxIter = 1000, opt = "optim"),
              # random = ~cover_pct|study,
              data = f2_data,REML = TRUE)

preds <- predict(mod_f3, interval = "confidence", level=.95) %>%
  as_tibble 
mm <- summary(mod_f3m)$coefficients[1]

f3_data<- f2_data %>%
  cbind(preds) 
f3_data$slope=mm

rr<- r.squaredLR(mod_f3m)[1]

ggplot(f3_data, aes(x=cover_pct, y=mass_gm2, color = study)) +
  geom_point() +
  geom_line(aes(y=predict(mod_f3m)), alpha = 0.5)+
  geom_abline(slope = mm, color="red", lwd=0.75)+
  ylab(expression(Mass~(g~m^-2))) +
  xlab("Percent Cover") +
  geom_text(aes(0, 180, label = paste0("y = ", slope, "x + 0")),
            parse=FALSE, hjust="left", color="black")+
  geom_text(aes(0, 170, label = paste(expression(Pseudo~R^2:~0.82))),
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

# subplot vs plot level=========================================================

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
