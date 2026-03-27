library(lme4)
library(gtsummary)
library(flextable)
library(broom.mixed)
library(sjPlot)
library(ggplot2)
library(patchwork)
library(ggeffects)

load("data/processed/lmmdata.RData")


## RI ====
RI <- lmer(zhfa ~ chsex + agemon + drwaterq_new + chsex*agemon + chsex*drwaterq_new + agemon*drwaterq_new + chsex*agemon*drwaterq_new + (1 | childid), data=ethiopia_df)

## RI w/ yc
RI_yc <- lmer(zhfa ~ chsex + agemon + yc + drwaterq_new + chsex*agemon + chsex*drwaterq_new + agemon*drwaterq_new + (1 | childid), data=ethiopia_df)

## RS w/ water ====
## reasoning of random slope for drwaterq_new: access to drinking water is not constant within each child 
RS_water <- lmer(zhfa ~ chsex + agemon + drwaterq_new + chsex*agemon + chsex*drwaterq_new + agemon*drwaterq_new + (drwaterq_new | childid) , data=ethiopia_df)

## RS w/ age ====
RS_age <- lmer(zhfa ~ chsex + agemon + drwaterq_new + chsex*agemon + chsex*drwaterq_new + agemon*drwaterq_new + (agemon | childid), data=ethiopia_df)

## RS w/ age & water ====
RS_age_water <- lmer(zhfa ~ chsex + agemon + drwaterq_new + chsex*agemon + chsex*drwaterq_new + agemon*drwaterq_new + chsex*agemon*drwaterq_new + (agemon + drwaterq_new | childid), data=ethiopia_df)


# summary ====
labels=c("RI","RS_water","RS_age", "RS_age_water")

tab_model(RI,RS_water, RS_age, RS_age_water,
          dv.labels = labels, 
          show.ci=F, show.se=T, show.aic=T,
          file="output/Table_random_effect.doc",
          p.style="star"
          )

