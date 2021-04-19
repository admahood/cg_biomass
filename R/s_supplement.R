# for supplement

source("data_prep.R")
library(lme4)
library(remef) # devtools::install_github('hohenstein/remef')
library(lmerTest)
library(performance)
library(car)

beautiful_clean_thing <- read_csv("data/all_3_years.csv")%>%
  mutate(year_region = paste(year,region,sep = "_")) %>%
  mutate(scaled_h = c(scale(max_ht_cm)),
         scaled_c = c(scale(cover_pct)),
         log_mass = log(mass_g))

# first model - just cover --------

logged_lm <- lm(log(mass_g) ~ cover_pct, 
            data=beautiful_clean_thing)
raw_lm <- lm(mass_g ~ cover_pct, 
          data=beautiful_clean_thing)

compare_performance(mod, mass_X_cover)

# log is better

year_random <- lmer(log(mass_g) ~ cover_pct + (1|year), 
            data=beautiful_clean_thing)

observers_random <- lmer(log(mass_g) ~ cover_pct + (1|observers), 
             data = beautiful_clean_thing)

compare_performance(raw_lm, logged_lm, year_random, observers_random) %>%
  as.data.frame %>%
  dplyr::select(-BIC,-Model, -Sigma, -ICC, -R2_adjusted) %>%
  mutate_if(is.numeric, round, 2)%>%
  mutate_at(c("AIC"),round) %>%
  write_csv("out/model_performance.csv")
