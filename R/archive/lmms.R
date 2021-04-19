source("data_prep.R")
library(lme4)
library(remef) # devtools::install_github('hohenstein/remef')
library(lmerTest)
library(car)

beautiful_clean_thing <- read_csv("data/all_3_years.csv")%>%
  mutate(year_region = paste(year,region,sep = "_")) %>%
  mutate(scaled_h = c(scale(max_ht_cm)),
         scaled_c = c(scale(cover_pct)),
         log_mass = log(mass_g))
# from the email:
# ~ mass and cover, and mass and height, in which we look at response of 
# the relation to region, year, and region * year interaction?
# quick look -------------------------------------------------------------------
ggplot(beautiful_clean_thing, aes(x=cover_pct, y=max_ht_cm)) +
  geom_point(aes(color = observers)) +
  ggsave("images/coverXheight.png", limitsize=F)


mod <- lmer(log(mass_g) ~ cover_pct + (cover_pct||year), 
            data=beautiful_clean_thing)
beautiful_clean_thing$preds <- predict(mod)

ggplot(beautiful_clean_thing, aes(x=cover_pct, y=log(mass_g), color=as.factor(year))) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds))+
  theme_bw() +
  ggsave("images/massXcover-year.png", limitsize = F)

mod1 <- lmer(log(mass_g) ~ cover_pct + (cover_pct||region), data=beautiful_clean_thing)
beautiful_clean_thing$preds1 <- predict(mod1)

ggplot(beautiful_clean_thing, aes(x=cover_pct, y=log(mass_g), color=region)) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds1))+
  theme_bw() +
  ggsave("images/massXcover-region.png", limitsize = F)


mod2 <- lmer(log(mass_g) ~ max_ht_cm + (max_ht_cm||year), data=beautiful_clean_thing)
beautiful_clean_thing$preds2 <- predict(mod2)

ggplot(beautiful_clean_thing, aes(x=max_ht_cm, y=log(mass_g), color=as.factor(year))) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds2))+
  theme_bw() +
  ggsave("images/massXheight-year.png", limitsize = F)


mod3 <- lmer(log(mass_g) ~ max_ht_cm + (max_ht_cm||region), data=beautiful_clean_thing)
beautiful_clean_thing$preds3 <- predict(mod3)

ggplot(beautiful_clean_thing, aes(x=max_ht_cm, y=log(mass_g), color=region)) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds3))+
  theme_bw() +
  ggsave("images/massXheight-region.png", limitsize = F)

mod4 <- lmer(log(mass_g) ~ max_ht_cm + (max_ht_cm||year_region), data=beautiful_clean_thing)
beautiful_clean_thing$preds4 <- predict(mod4)

ggplot(beautiful_clean_thing, aes(x=max_ht_cm, y=log(mass_g), color=year_region)) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds4))+
  theme_bw() +
  ggsave("images/massXheight-year_region.png", limitsize = F)

mod5 <- lmer(log(mass_g) ~ cover_pct + (cover_pct||year_region), data=beautiful_clean_thing)
beautiful_clean_thing$preds5 <- predict(mod5)

ggplot(beautiful_clean_thing, aes(x=cover_pct, y=log(mass_g), color=year_region)) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds5))+
  theme_bw() +
  ggsave("images/massXcover-year_region.png", limitsize = F)


# adjusting for height, year as random -----------------------------------------

mod6 <- lmer(log_mass ~ cover_pct* + scaled_h + (cover_pct||year), data = beautiful_clean_thing)
car::vif(mod6)

beautiful_clean_thing$log_mass_adj <- remef(mod6, fix = "scaled_h", keep.intercept = TRUE)
bct_mean_h <- beautiful_clean_thing %>%
  mutate(scaled_h = 0,
         max_ht_cm = mean(beautiful_clean_thing$max_ht_cm))
beautiful_clean_thing$preds6 <- predict(mod6, newdata = bct_mean_h)

ggplot(beautiful_clean_thing, aes(x=cover_pct, y=log_mass_adj, color=as.factor(year))) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds6))+
  theme_bw() +
  ggtitle("Adjusted for height")+
  ggsave("images/massXcover+height-year__adj.png", limitsize = F)

ggplot(beautiful_clean_thing, aes(x=cover_pct, y=log_mass, color=as.factor(year))) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=predict(mod6)))+
  theme_bw() +
  ggtitle("Without adjusting for height")+
  ggsave("images/massXcover+height-year__noadj.png", limitsize = F)

# comparing to gls -------------------------------------------------------------
# control <- lmeControl(maxIter = 10000, msMaxIter = 1000)
# nmod6 <- lme(mass_g ~ cover_pct*observers + max_ht_cm, random = ~cover_pct|year,
#              data = beautiful_clean_thing,
#              method="ML")
# gmod6 <- gls(mass_g ~ cover_pct*observers +max_ht_cm, data=beautiful_clean_thing, method="ML")
# nmod7 <- lme(mass_g ~ cover_pct + max_ht_cm, random = ~1|observers,
#              data = beautiful_clean_thing,
#              control = list(maxIter = 10000, msMaxIter = 1000, optim="SANN"),
#              method="ML")
#
# summary(nmod6)
# summary(nmod7)
# summary(gmod6)
# anova(nmod6, gmod6,nmod7)
#
# beautiful_clean_thing$preds7 <- predict(gmod6)
#
# ggplot(beautiful_clean_thing, aes(x=cover_pct, y=mass_g, color=observers)) +
#   geom_point(aes(shape = region), alpha = 0.85, size=3) +
#   #geom_smooth(method="lm") +
#   geom_line(aes(y=preds7, color = observers))+
#   theme_bw()

# adjusting for height and using observers instead of year ---------------------

mod7 <- lmer(log_mass ~ cover_pct + scaled_h+(cover_pct||observers), data = beautiful_clean_thing)

beautiful_clean_thing$log_mass_adj <- remef(mod7, fix = "scaled_h", keep.intercept = TRUE)
bct_mean_h <- beautiful_clean_thing %>%
  mutate(scaled_h = 0, 
         max_ht_cm = mean(beautiful_clean_thing$max_ht_cm))
beautiful_clean_thing$preds6 <- predict(mod7, newdata = bct_mean_h)

ggplot(beautiful_clean_thing, aes(x=cover_pct, y=log_mass_adj, color=observers)) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds6))+
  theme_bw()+
  ggsave("images/massXcover+height-observers__adj.png", limitsize = F)

ggplot(beautiful_clean_thing, aes(x=cover_pct, y=log_mass_adj, color=observers)) +
  geom_smooth(method="lm", fill = "grey80") +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_line(aes(y=predict(mod7)))+
  theme_bw()

# adjusting for cover and using observers as random ----------------------------

mod8 <- lmer(log_mass ~ scaled_c + max_ht_cm + (max_ht_cm||observers), 
             data = beautiful_clean_thing)

beautiful_clean_thing$log_mass_adjc <- remef(mod8, fix = "scaled_c", keep.intercept = TRUE)
bct_mean_c <- beautiful_clean_thing %>%
  mutate(scaled_c= 0, 
         cover_pct = mean(beautiful_clean_thing$cover_pct))
beautiful_clean_thing$preds7 <- predict(mod8, newdata = bct_mean_c)

ggplot(beautiful_clean_thing, aes(x=max_ht_cm, y=log_mass_adjc, color=observers)) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds7))+
  theme_bw()+
  ggsave("images/massXheight+cover-observers__adj.png", limitsize = F)

ggplot(beautiful_clean_thing, aes(x=max_ht_cm, y=log_mass_adjc, color=observers)) +
  geom_smooth(method="lm", fill = "grey80") +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_line(aes(y=predict(mod7)))+
  theme_bw()


# model diagnostics & validation -----------------------------------------------
vif(mod7)
AIC(mod,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8)
BIC(mod,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8)

performance::compare_performance(mod,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8)

summary(mod8)
