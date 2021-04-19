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
f2 <- function(x) tan(x)

cf1<- ggplot(data.frame(x = c(0, 105)), aes(x)) + 
  stat_function(fun = f1, n = 1000, color = "grey30") +
  coord_cartesian(ylim = c(0, 120)) +
  geom_segment(x=0, xend=105, y=0, yend=105, color = "red")+
  theme(axis.ticks=element_blank(),
        axis.text=element_blank())+
  geom_hline(yintercept=84, lty=2)+
  xlab("Stem Density") +
  ylab("Abundance Measures") +
  geom_text(x=75, y=95, label="Biomass", color="red") +
  geom_text(x=95, y=70, label = "Cover") +
  geom_text(x=1, y=80, label = "100% Cover", hjust = "left")

cf2<-ggplot(data.frame(x = c(0, 1)), aes(x)) + 
  stat_function(fun = f2, n = 1000, color = "red") +
  # coord_cartesian(ylim = c(0, 120)) +
  geom_segment(x=0, xend=1, y=0, yend=1, color = "grey30") +
  theme(axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title.y = element_blank()) +
  geom_vline(xintercept=1, lty=2) +
  xlab("Cover") +
  # ylab("Abundance Measures") +
  geom_text(x=.5, y=.80, label="Biomass", color="red") +
  geom_text(x=.85, y=.70, label = "Cover") +
  geom_text(x= 0.95, y=.105, label = "100% Cover", hjust = "left", angle=90)

ggarrange(cf1, cf2, labels="auto", label.x = c(.1, 0.05))+
  ggsave("figures/conceptual_figure.png", width=7, height=3.5)

# figure 2 ---------------------------------------------------------------------
f2_data <- rbind(js %>% dplyr::select(Plot=plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "a. June 2016. n = 59",
                          year = 2016), 
                 bm17%>% dplyr::select(Plot=plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "b. July 2017. n = 40",
                          year=2017),
                 bm18%>% dplyr::select(Plot=plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "c. July 2018. n = 40",
                          year=2018),
                 bm19%>% dplyr::select(Plot=plot, cover_pct, mass_gm2)%>%
                   na.omit()%>%
                   mutate(study = "d. September 2019. n = 40",
                          year = 2019)) %>%
  mutate(study = factor(study, levels = c("a. June 2016. n = 59",
                                          "b. July 2017. n = 40",
                                          "c. July 2018. n = 40",
                                          "d. September 2019. n = 40")))
xd<-f2_data %>%
  nest(-study) %>% 
  mutate(model = map(data,~lm(mass_gm2~cover_pct, data = .x)),
         adj.r.squared = map_dbl(model, ~ signif(summary(.x)$r.squared, 3)),
         # slope = map_dbl(model, ~signif(summary(.x)$coefficients[1],3)),
         slope = map_dbl(model, ~signif(summary(.x)$coefficients[2],3)),
         intercept = map_dbl(model, ~signif(summary(.x)$coefficients[1],3)),
         p=map_dbl(model, ~ signif(summary(.x)$coefficients[4],3)))  %>% 
  select(-data, -model) %>% 
  left_join(f2_data) %>% 
  mutate(p = ifelse(p < 0.05, "p < 0.05", "p > 0.05")) 

xd0<-f2_data %>%
  nest(-study) %>% 
  mutate(model = map(data,~lm(mass_gm2~0+cover_pct, data = .x)),
         adj.r.squared = map_dbl(model, ~ signif(summary(.x)$r.squared, 3)),
         slope = map_dbl(model, ~signif(summary(.x)$coefficients[1],3)),
         # slope = map_dbl(model, ~signif(summary(.x)$coefficients[2],3)),
         # intercept = map_dbl(model, ~signif(summary(.x)$coefficients[1],3)),
         p=map_dbl(model, ~ signif(summary(.x)$coefficients[4],3)))  %>% 
  select(-data, -model) %>% 
  left_join(f2_data) %>% 
  mutate(p = ifelse(p < 0.05, "p < 0.05", "p > 0.05")) 



xdp<-f2_data %>%
  nest(-study) %>%
  mutate(model = map(data,~lm(mass_gm2~0+poly(cover_pct,2), data = .x)),
         adj.r.squared = map_dbl(model, ~ signif(summary(.x)$r.squared, 3)),
         slope = map_dbl(model, ~signif(summary(.x)$coefficients[1],3)),
         p=map_dbl(model, ~ signif(summary(.x)$coefficients[4],3)))  %>%
  select(-data, -model) %>%
  left_join(f2_data) %>%
  mutate(p = ifelse(p < 0.05, "p < 0.05", "p > 0.05"))

tibble(normal=lapply(xd$model, AIC)%>% unlist ,
       zero= lapply(xd0$model, AIC)%>% unlist,
       poly=lapply(xdp$model, AIC) %>% unlist)
  
ggplot(xd, 
       aes(x=cover_pct, y=mass_gm2)) +
  geom_point() +
  ylab(expression(Aboveground~Biomass~(g~m^-2))) +
  xlab("Percent Cover") +
  geom_smooth(method="lm", se=TRUE, color = "black") +
  facet_wrap(~study, ncol = 1) +
  theme(strip.text = element_text(size=15))+
  geom_text(aes(0, 200), label = expression(R^2))+
  geom_text(aes(0, 200, label = paste("   = ",adj.r.squared, 
                                     "\n",p,
                                     "\ny = ",intercept," + ", slope,"x")), 
            hjust="left", vjust="top")+
  ggsave("figures/figure_2_panel.png", height = 10, width=3.5)

ggplot(xd, 
       aes(x=cover_pct, y=mass_gm2)) +
  geom_point() +
  ylab(expression(Aboveground~Biomass~(g~m^-2))) +
  xlab("Percent Cover") +
  geom_smooth(method="lm", se=TRUE, color = "black") +
  facet_wrap(~study, ncol = 2, nrow=2) +
  theme(strip.text = element_text(size=15))+
  geom_text(aes(0, 200), label = expression(R^2))+
  geom_text(aes(0, 200, label = paste("   = ",adj.r.squared, 
                                      "\n",p,
                                      "\ny = ",intercept," + ", slope,"x")), 
            hjust="left", vjust="top")+
  ggsave("figures/figure_2_square.png", height = 8, width=8)

# figure 3 ---------------------------------------------------------------------
# first showing lmm is better
library(nlme)
load("data/peak_green.Rda")

plots_w_ppt_elv <- st_read("data/plots_w_elv_ppt.gpkg") %>%
  st_set_geometry(NULL) %>%
  dplyr::select(-study)

f3_data <- filter(f2_data, study != "d. September 2019. n = 40") %>%
  unique %>%
  mutate(study = droplevels(study),
         Plot = str_to_lower(Plot)) %>%
  mutate(study = str_sub(study, 4),
         plot_year = str_c(Plot, year))%>%
  left_join(peak_green %>% 
              mutate(plot_year = str_c(plot, year) %>% 
                       str_to_lower())%>%
              dplyr::select(plot_year, days_after_green), 
            by = "plot_year") %>%
  mutate(plot = ifelse(startsWith(Plot, "j"), str_sub(Plot, 1,3), Plot)) %>%
  left_join(plots_w_ppt_elv,
            by=c("plot","year")) %>%
  mutate(study = factor(study, levels = c("June 2016. n = 59",
                                          "July 2017. n = 40",
                                          "July 2018. n = 40")))
# lmers - for better extraction of the formulas


mod_f3 <- lm(mass_gm2 ~ 0 + cover_pct, f3_data)
mod_f3_sq <- lm(sqrt(mass_gm2) ~ sqrt(cover_pct), f3_data)
mod_f3_sq_gr <- lm(sqrt(mass_gm2) ~ sqrt(cover_pct)+days_after_green, f3_data) 

mod_f3mm <-lmer(mass_gm2 ~ 0 + cover_pct+(cover_pct|study), control = lmerControl(optimizer="bobyqa"),
                data = f3_data,REML=FALSE)

mod_f3_l <- lm(log(mass_gm2) ~ 0 + log(cover_pct), f3_data)
mod_f3mm_l <-lmer(log(mass_gm2) ~ 0 + log(cover_pct)+(cover_pct|study), control = lmerControl(optimizer="bobyqa"),
                  data = f3_data,REML=FALSE)

mod_f3mm_l_n0_ri <-lmer(log(mass_gm2) ~ log(cover_pct)+(1|study),
                        data = f3_data,REML=FALSE)

mod_f3mm_sq_n0_ri <-lmer(sqrt(mass_gm2) ~ sqrt(cover_pct)+(1|study),
                         data = f3_data,REML=FALSE)
mod_f3mm_sq_n0_ri_pg <-lmer(sqrt(mass_gm2) ~ sqrt(cover_pct) + days_after_green+(1|study),
                            data = f3_data,REML=FALSE)
mod_f3mm_sq_n0_ri_elv <-lmer(sqrt(mass_gm2) ~ sqrt(cover_pct) + elevation+(1|study),
                            data = f3_data,REML=FALSE)
mod_f3mm_sq_n0_ri_ppt <-lmer(sqrt(mass_gm2) ~ sqrt(cover_pct) + ppt+(1|study),
                            data = f3_data,REML=FALSE)
mod_f3mm_l_n0 <-lmer(log(mass_gm2) ~ log(cover_pct)+(cover_pct|study),
                     data = f3_data,REML=FALSE)

mod_f3_no0 <- lm(mass_gm2 ~ cover_pct, f3_data)
mod_f3mm_no0 <-lmer(mass_gm2 ~  cover_pct+ (cover_pct|study),
                    data = f3_data, REML = FALSE)

modf_list <- list(mod_f3,
                  mod_f3_sq,
                  mod_f3_sq_gr,
                  mod_f3_no0,
                  mod_f3_l,
                  mod_f3mm,
                  mod_f3mm_no0,
                  mod_f3mm_sq_n0_ri_pg,
                  mod_f3mm_sq_n0_ri_elv,
                  mod_f3mm_sq_n0_ri_ppt,
                  mod_f3mm_sq_n0_ri,
                  mod_f3mm_l,
                  mod_f3mm_l_n0,
                  mod_f3mm_l_n0_ri)
forms <- lapply(modf_list,formula) %>% as.character()

# lmes ( for better calculation of R2)
mod_f3 <- lm(mass_gm2 ~ 0 + cover_pct, f3_data)
mod_f3_sq <- lm(sqrt(mass_gm2) ~ sqrt(cover_pct), f3_data)
mod_f3_sq_gr <- lm(sqrt(mass_gm2) ~ sqrt(cover_pct)+days_after_green, f3_data) 

mod_f3mm <-lme(mass_gm2 ~ 0 + cover_pct, 
               control = lmeControl(maxIter = 1000, opt = "optim"),
               random = ~cover_pct|study,
               data = f3_data,method = "ML")

mod_f3_l <- lm(log(mass_gm2) ~ 0 + log(cover_pct), f3_data, method = "ML")
mod_f3mm_l <-lme(log(mass_gm2) ~ 0 + log(cover_pct), 
               control = lmeControl(maxIter = 1000, opt = "optim"),
               random = ~cover_pct|study,
               data = f3_data,method = "ML")

mod_f3mm_l_n0_ri <-lme(log(mass_gm2) ~ log(cover_pct), 
                    control = lmeControl(maxIter = 1000, opt = "optim"),
                    random = ~1|study,
                    data = f3_data,method = "ML")
mod_f3mm_sq_n0_ri <-lme(sqrt(mass_gm2) ~ sqrt(cover_pct), 
                       control = lmeControl(maxIter = 1000, opt = "optim"),
                       random = ~1|study,
                       data = f3_data,method = "ML")
mod_f3mm_sq_n0_ri_pg <-lme(sqrt(mass_gm2) ~ sqrt(cover_pct) + days_after_green, 
                        control = lmeControl(maxIter = 1000, opt = "optim"),
                        random = ~1|study,
                        data = f3_data,method = "ML")
mod_f3mm_sq_n0_ri_elv <-lme(sqrt(mass_gm2) ~ sqrt(cover_pct) + elevation, 
                           control = lmeControl(maxIter = 1000, opt = "optim"),
                           random = ~1|study,
                           data = f3_data,method = "ML")
mod_f3mm_sq_n0_ri_ppt <-lme(sqrt(mass_gm2) ~ sqrt(cover_pct) + ppt, 
                            control = lmeControl(maxIter = 1000, opt = "optim"),
                            random = ~1|study,
                            data = f3_data,method = "ML")
mod_f3mm_l_n0 <-lme(log(mass_gm2) ~ log(cover_pct), 
                 control = lmeControl(maxIter = 1000, opt = "optim"),
                 random = ~cover_pct|study,
                 data = f3_data,method = "ML")

mod_f3_no0 <- lm(mass_gm2 ~ cover_pct, f3_data, method = "ML")
mod_f3mm_no0 <-lme(mass_gm2 ~  cover_pct, 
               control = lmeControl(maxIter = 1000, opt = "optim"),
               random = ~cover_pct|study,
               data = f3_data,method = "ML")


mod_list<- list(mod_f3,
                mod_f3_sq,
                mod_f3_sq_gr,
                mod_f3_no0,
                mod_f3_l,
                mod_f3mm,
                mod_f3mm_no0,
                mod_f3mm_sq_n0_ri_pg,
                mod_f3mm_sq_n0_ri_elv,
                mod_f3mm_sq_n0_ri_ppt,
                mod_f3mm_sq_n0_ri,
                mod_f3mm_l,
                mod_f3mm_l_n0,
                mod_f3mm_l_n0_ri)

# observed vs predicted R2

ovpr2<-lapply(mod_list, predict) %>% 
  as_tibble(.name_repair = "universal") %>%
  mutate(observed = f3_data$mass_gm2)

res <- list()
for(i in 1:(ncol(ovpr2)-1)){
  frmla<- paste0("observed~","...", i)
  mod <- lm(frmla, data = ovpr2)
  s<-summary(mod)
  res[[i]] <-s$r.squared
}

ovp_r2<-as.vector(res) %>% unlist


# constructing the big table ==============
big_tab <- compare_performance(mod_f3,
                               mod_f3_sq,
                               mod_f3_sq_gr,
                               mod_f3_no0,
                               mod_f3_l,
                               mod_f3mm,
                               mod_f3mm_no0,
                               mod_f3mm_sq_n0_ri_pg,
                               mod_f3mm_sq_n0_ri_elv,
                               mod_f3mm_sq_n0_ri_ppt,
                               mod_f3mm_sq_n0_ri,
                               mod_f3mm_l,
                               mod_f3mm_l_n0,
                               mod_f3mm_l_n0_ri) %>%
  as.data.frame() %>%
  mutate(Formula = forms, 
         AIC = round(AIC, 1),
         BIC = round(BIC, 1),
         RMSE = round(RMSE, 3),
         R2_conditional = round(R2_conditional, 3),
         R2_marginal = round(R2_marginal, 3),
         R2 = round(R2, 3),
         ovpR2 = round(ovp_r2,3)) %>%
  dplyr::select(Formula, AIC, BIC, RMSE, R2_conditional, R2_marginal, R2, ovpR2)

write_csv(big_tab, "data/model_comparison.csv")

performance::check_model(mod_f3mm_no0)

mod_f3m <-lmer(sqrt(mass_gm2) ~ sqrt(cover_pct)+(1|study), 
              data = f3_data, REML = TRUE)
performance::check_model(mod_f3m)
summary(mod_f3m)

preds <- predict(mod_f3m, type = "response", level=.95) %>%
  as_tibble 
confint_tidy(mod_f3m)

mm <- summary(mod_f3m)$coefficients[2,1] 
ii <- summary(mod_f3m)$coefficients[1,1]

# 
# newdat<- expand_grid(cover_pct = seq(0, 6, by=0.2), study=unique(f3_data$study))
# preds<- merTools::predictInterval(mod_f3m, newdata=newdat) %>% 
#   cbind(newdat)

# f3_data<- f3_data %>%
#   cbind(preds) 
f3_data$slope=mm %>% round(2)
f3_data$intercept <- ii %>% round(2)
rr<- r.squaredGLMM(mod_f3m)

# colorz<- c("#273253", "#ED5C4D", "#FBBE4B")
library(ggtext)

# partial effects plot for the lm


# new fig
pmm<- ggplot(f3_data, aes(x=sqrt(cover_pct),
                          y=sqrt(mass_gm2), color = study)) +
  geom_abline(slope = mm, color="red", lwd=0.75, intercept = ii)+
  geom_point() +
  geom_line(aes(y=predict(mod_f3m)))+
  ylab(expression(Aboveground~Biomass~(g~m^-2)^{1/2})) +
  xlab(expression(Percent~Cover^{1/2})) +
  scale_color_brewer(palette = "Accent")+
  theme_classic()+
  theme(legend.title = element_blank(),
        legend.position ="none",
        legend.justification = c(1,0),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill=NA, size =.75))

# transformed to regular scale
ggplot(f3_data, aes(x=(cover_pct), y=(mass_gm2), color = study)) +
  geom_abline(slope = mm^2, color="red", lwd=0.75, intercept = ii)+
  geom_point() +
  geom_line(aes(y=predict(mod_f3m)^2))+
  ylab(expression(Aboveground~Biomass^{1/2}~(g~m^-2))) +
  xlab(expression(Percent~Cover^{1/2}))+
  scale_color_brewer(palette = "Accent")+
  theme(legend.justification=c(1,0), 
        legend.position=c(1,0),
        legend.title = element_blank(),
        legend.background = element_rect(fill = 'transparent'))+
  ggsave("figures/figure_3_lmm_line_sqrt_trans.png",
         width=3.5,height=3.5, limitsize = FALSE)
  
ggplot(f3_data, aes(x=cover_pct, y=mass_gm2, color = study)) +
  geom_point() +
  geom_line(aes(y=exp(predict(mod_f3mm_l_n0))))#+
  facet_wrap(~study)
  # geom_abline(slope = mm, color="red", lwd=0.75)+
  ylab(expression(Aboveground~Biomass~(g~m^-2))) +
  xlab("Percent Cover") +
  geom_text(aes(0, 180, label = paste0("y = ", slope, "x + 0")),
            parse=FALSE, hjust="left", color="black")+
  geom_text(aes(0, 165, label = paste(expression(Pseudo~R^2:~0.81))),
            parse=TRUE,hjust="left", color="black")+
  theme(plot.title = element_text(size = 12))  +
  scale_color_brewer(palette = "Accent")+
  facet_wrap(~study)+
  theme(legend.justification=c(1,0), 
        legend.position=c(1,0),
        legend.title = element_blank(),
        legend.background = element_rect(fill = 'transparent'))+
  ggsave("figures/figure_3_lmm_line.png",
         width=3.5,height=3.5, limitsize = FALSE)

# partial effects plots
closest <- function(x, x0) apply(outer(x, x0, FUN=function(x, x0) abs(x - x0)), 1, which.min)
  
library(effects)
effr<-Effect("days_after_green", partial.residuals=T, mod_f3_sq_gr)
efff<-Effect("cover_pct", partial.residuals=T, mod_f3_sq_gr)

res_dfgr <- data.frame(res =  effr$fit[closest(effr$data$days_after_green,
                                              effr$x$days_after_green)] + effr$residuals, 
                      value = effr$data$days_after_green,
                      variable = "*B. tectorum* Seeds (Count)")
res_dfp <- data.frame(res =  efff$fit[closest(efff$data$cover_pct,
                                              efff$x$cover_pct)] + efff$residuals, 
                      value = efff$data$cover_pct,
                      variable = "*P. secunda* Seeds (Count)")



mod_effr <- data.frame(lwr = (effr$lower), 
                       upr = (effr$upper), 
                       fit = (effr$fit), 
                       variable = "Days after Peak Green",
                       value = effr$x$days_after_green)
mod_efff <- data.frame(lwr = (efff$lower), 
                       upr = (efff$upper), 
                       fit = (efff$fit), 
                       variable = "Percent Cover",
                       value = efff$x$cover_pct)

p1 <- mod_efff %>%
  ggplot(aes(x=sqrt(value))) +
  geom_line(aes(y=fit), color = "red", lwd=1) +
  geom_point(data = res_dfp,aes(y=res, color = f3_data$study)) +
  # geom_line(aes(y=upr), lty=2, color = "grey") +
  # geom_line(aes(y=lwr), lty=2, color = "grey") +
  ylab(expression(Aboveground~Biomass~(g~m^-2)^{1/2})) +
  xlab(expression(Percent~Cover^{1/2}))+
  theme_classic()+
  scale_color_brewer(palette = "Accent")+
  theme(legend.title = element_blank(),
        legend.position =c(0,1),
        legend.justification = c(0,1),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill=NA, size =.75)) +
  ggsave("figures/figure_3_lm.png", height = 3.5, width = 3.5)

ggpubr::ggarrange(p1,pmm, labels = c("(a)", "(b)") ) +
  ggsave("figures/figure_3_2panel.png", height =3.5, width=7.5)

# p2<-mod_effr %>%
#   ggplot(aes(x=value)) +
#   geom_line(aes(y=fit), color = "grey") +
#   geom_point(data = res_dfgr,aes(y=res, color = f3_data$study)) +
#   geom_line(aes(y=upr), lty=2, color = "grey") +
#   geom_line(aes(y=lwr), lty=2, color = "grey") +
#   ylab("Days After") +
#   xlab("Percent Cover") +
#   theme_classic()+
#   theme(legend.title = element_blank(),
#         axis.title.x = ggtext::element_markdown(),
#         legend.position = c(0,1),
#         legend.justification = c(0,1),
#         legend.background = element_rect(fill = "transparent"),
#         panel.border = element_rect(fill=NA, size =.75));p2

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


# elevation exploration -- elevation not significant, doesn't lower the AIC
plots_w_elevation <- st_read("data/plots_w_elevation.gpkg")

mod_f3_elev <-lmer(mass_gm2 ~ 0 + cover_pct + Elevation + (cover_pct|study),
                   data = plots_w_elevation, REML = FALSE)
summary(mod_f3_elev)
AIC(mod_f3m, mod_f3_elev)
