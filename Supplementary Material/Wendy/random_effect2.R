library(lme4)
library(gtsummary)
library(flextable)
library(broom.mixed)
library(sjPlot)
library(ggplot2)
library(patchwork)

ethiopia <- read.csv("data/processed/ethiopia_processed.csv")
ethiopia_cc <- read.csv("data/processed/ethiopia_complete_cases.csv")

ethiopia$agemonc <- (ethiopia$agemon-mean(ethiopia$agemon, na.rm=T))/12

# ethiopia ====
# childid 
## RI ====
RI <- lmer(zhfa ~ chsex + agemonc + drwaterq_new + chsex*agemonc + chsex*drwaterq_new + agemonc*drwaterq_new + chsex*agemonc*drwaterq_new + (1 | childid), data=ethiopia)

## RI w/ yc
RI_yc <- lmer(zhfa ~ chsex + agemonc + yc + drwaterq_new + chsex*agemonc + chsex*drwaterq_new + agemonc*drwaterq_new + chsex*agemonc*drwaterq_new + (1 | childid), data=ethiopia)

## RS w/ water ====
## reasoning of random slope for drwaterq_new: access to drinking water is not constant within each child 
RS_water <- lmer(zhfa ~ chsex + agemonc + drwaterq_new + chsex*agemonc + chsex*drwaterq_new + agemonc*drwaterq_new + chsex*agemonc*drwaterq_new + (drwaterq_new | childid) , data=ethiopia)

## RS w/ age ====
RS_age <- lmer(zhfa ~ chsex + agemonc + drwaterq_new + chsex*agemonc + chsex*drwaterq_new + agemonc*drwaterq_new + chsex*agemonc*drwaterq_new + (agemonc | childid), data=ethiopia)

## RS w/ age & water ====
RS_age_water <- lmer(zhfa ~ chsex + agemonc + drwaterq_new + chsex*agemonc + chsex*drwaterq_new + agemonc*drwaterq_new + chsex*agemonc*drwaterq_new + (agemonc + drwaterq_new | childid), data=ethiopia)


# summary ====
labels=c("RI","RS_water","RS_age", "RS_age_water")

tab_model(RI,RS_water, RS_age, RS_age_water,
          dv.labels = labels, 
          show.ci=F, show.se=T, show.aic=T,
          file="Individual/Wendy/output/Table_random_effect_agemonc.doc",
          p.style="star"
)


##### personal notes ====
# yc is not significant
# three-way interaction not significant 
# two-way interactions not significant except for sex*age
# drwaterq_new fe not significant 
# drwaterq_new re moderate variability
# age needs to be centered, preferably /12
# age not centered: RS_age lowest AIC
# age centered: RS_age_water lowest AIC
