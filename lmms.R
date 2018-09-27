source("data_prep.R")
library(lme4)
library(remef) # install_github('hohenstein/remef')
library(lmerTest)

beautiful_clean_thing <- mutate(beautiful_clean_thing,
                                year_region = paste(year,region,sep = "_")) %>%
  mutate(scaled_h = c(scale(max_ht_cm)),
         log_mass = log(mass_g))
# from the email:
# ~ mass and cover, and mass and height, in which we look at response of 
# the relation to region, year, and region * year interaction?
# quick look -------------------------------------------------------------------
mod <- lmer(log(mass_g) ~ cover_pct + (cover_pct||year), 
            data=beautiful_clean_thing)
beautiful_clean_thing$preds <- predict(mod)

ggplot(beautiful_clean_thing, aes(x=cover_pct, y=log(mass_g), color=year)) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds))+
  theme_bw()

mod1 <- lmer(log(mass_g) ~ cover_pct + (cover_pct||region), data=beautiful_clean_thing)
beautiful_clean_thing$preds1 <- predict(mod1)

ggplot(beautiful_clean_thing, aes(x=cover_pct, y=log(mass_g), color=region)) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds1))+
  theme_bw()


mod2 <- lmer(log(mass_g) ~ max_ht_cm + (max_ht_cm||year), data=beautiful_clean_thing)
beautiful_clean_thing$preds2 <- predict(mod2)

ggplot(beautiful_clean_thing, aes(x=max_ht_cm, y=log(mass_g), color=year)) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds2))+
  theme_bw()


mod3 <- lmer(log(mass_g) ~ max_ht_cm + (max_ht_cm||region), data=beautiful_clean_thing)
beautiful_clean_thing$preds3 <- predict(mod3)

ggplot(beautiful_clean_thing, aes(x=max_ht_cm, y=log(mass_g), color=region)) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds3))+
  theme_bw()

mod4 <- lmer(log(mass_g) ~ max_ht_cm + (max_ht_cm|year_region), data=beautiful_clean_thing)
beautiful_clean_thing$preds4 <- predict(mod4)

ggplot(beautiful_clean_thing, aes(x=max_ht_cm, y=log(mass_g), color=year_region)) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds4))+
  theme_bw()

mod5 <- lmer(log(mass_g) ~ cover_pct + (cover_pct|year_region), data=beautiful_clean_thing)
beautiful_clean_thing$preds5 <- predict(mod5)

ggplot(beautiful_clean_thing, aes(x=cover_pct, y=log(mass_g), color=year_region)) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds5))+
  theme_bw()


# adjusting for height, year as random -----------------------------------------

mod6 <- lmer(log_mass ~ cover_pct* + scaled_h + (cover_pct||year), data = beautiful_clean_thing)
car::vif(mod6)

beautiful_clean_thing$log_mass_adj <- remef(mod6, fix = "scaled_h", keep.intercept = TRUE)
bct_mean_h <- beautiful_clean_thing %>%
  mutate(scaled_h = 0, 
         max_ht_cm = mean(beautiful_clean_thing$max_ht_cm))
beautiful_clean_thing$preds6 <- predict(mod6, newdata = bct_mean_h)

ggplot(beautiful_clean_thing, aes(x=cover_pct, y=log_mass_adj, color=year)) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds6))+
  theme_bw() +
  ggtitle("Adjusted for height")

ggplot(beautiful_clean_thing, aes(x=cover_pct, y=log_mass, color=year)) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=predict(mod6), color = year))+
  theme_bw() +
  ggtitle("Without adjusting for height")

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

mod7 <- lmer(log_mass ~ cover_pct + scaled_h + (cover_pct||observers), data = beautiful_clean_thing)

beautiful_clean_thing$log_mass_adj <- remef(mod7, fix = "scaled_h", keep.intercept = TRUE)
bct_mean_h <- beautiful_clean_thing %>%
  mutate(scaled_h = 0, 
         max_ht_cm = mean(beautiful_clean_thing$max_ht_cm))
beautiful_clean_thing$preds6 <- predict(mod7, newdata = bct_mean_h)

ggplot(beautiful_clean_thing, aes(x=cover_pct, y=log_mass_adj, color=observers)) +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_smooth(method="lm") +
  geom_line(aes(y=preds6))+
  theme_bw()

ggplot(beautiful_clean_thing, aes(x=cover_pct, y=log_mass_adj, color=observers)) +
  geom_smooth(method="lm", fill = "grey80") +
  geom_point(aes(shape = region), alpha = 0.85, size=3) +
  #geom_line(aes(y=predict(mod7)))+
  theme_bw()

# model diagnostics & validation -----------------------------------------------
vif(mod7)
AIC(mod,mod1,mod2,mod3,mod4,mod5,mod6,mod7)
BIC(mod,mod1,mod2,mod3,mod4,mod5,mod6,mod7)
