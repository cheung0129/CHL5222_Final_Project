# import packages
library(lme4)
library(here)
library(dplyr)
library(lmerTest)

library(ggeffects)
library(ggplot2)


path_lmm_datasets <- here("data","processed","lmmdata.RData")
load(path_lmm_datasets)


# final model, REML, young
mod_lmm_yg <- lmer(zhfa ~ chsex + time + drwaterq_new +
                         chsex:time + chsex:drwaterq_new + drwaterq_new:time +
                         age_first_c + 
                         (1 | childid),
                       data = ethiopia_yg)
summary(mod_lmm_yg)


# final model, REML, old
mod_lmm_old <- lmer(zhfa ~ chsex + time + drwaterq_new +
                     chsex:time + chsex:drwaterq_new + drwaterq_new:time +
                     age_first_c + 
                     (1 | childid),
                   data = ethiopia_old)
summary(mod_lmm_old)
