library(lme4)
library(gtsummary)
library(flextable)
library(broom.mixed)
library(sjPlot)
library(ggplot2)
library(patchwork)

ethiopia <- read.csv("data/processed/ethiopia_processed.csv")
ethiopia_cc <- read.csv("data/processed/ethiopia_complete_cases.csv")

# ethiopia$agemonc <- (ethiopia$agemon-mean(ethiopia$agemon, na.rm=T))/12

# ethiopia ====
# childid 
## RI ====
RI <- lmer(zhfa ~ chsex + agemon + drwaterq_new + chsex*agemon + chsex*drwaterq_new + agemon*drwaterq_new + chsex*agemon*drwaterq_new + (1 | childid), data=ethiopia)

## RI w/ yc
RI_yc <- lmer(zhfa ~ chsex + agemon + yc + drwaterq_new + chsex*agemon + chsex*drwaterq_new + agemon*drwaterq_new + chsex*agemon*drwaterq_new + (1 | childid), data=ethiopia)

## RS w/ water ====
## reasoning of random slope for drwaterq_new: access to drinking water is not constant within each child 
RS_water <- lmer(zhfa ~ chsex + agemon + drwaterq_new + chsex*agemon + chsex*drwaterq_new + agemon*drwaterq_new + chsex*agemon*drwaterq_new + (drwaterq_new | childid) , data=ethiopia)

## RS w/ age ====
RS_age <- lmer(zhfa ~ chsex + agemon + drwaterq_new + chsex*agemon + chsex*drwaterq_new + agemon*drwaterq_new + chsex*agemon*drwaterq_new + (agemon | childid), data=ethiopia)

## RS w/ age & water ====
RS_age_water <- lmer(zhfa ~ chsex + agemon + drwaterq_new + chsex*agemon + chsex*drwaterq_new + agemon*drwaterq_new + chsex*agemon*drwaterq_new + (agemon + drwaterq_new | childid), data=ethiopia)


# summary ====
labels=c("RI","RS_water","RS_age", "RS_age_water")

tab_model(RI,RS_water, RS_age, RS_age_water,
          dv.labels = labels, 
          show.ci=F, show.se=T, show.aic=T,
          file="Individual/Wendy/output/Table_random_effect.doc",
          p.style="star"
          )


p1 <- plot(ggpredict(RI, terms=c("agemon", "drwaterq_new")), show_data = T)
p2 <- plot(ggpredict(RS_water, terms=c("agemon", "drwaterq_new")), show_data = T)
p3 <- plot(ggpredict(RS_age, terms=c("agemon", "drwaterq_new")), show_data = T)

p1 + p2 + p3 + plot_layout(guides="collect", nrow=1) &
  theme(legend.position="bottom")


##### personal notes ====
# yc is not significant
# three-way interaction not significant 
# two-way interactions not significant except for sex*age
# drwaterq_new fe not significant 
# drwaterq_new re moderate variability
# age needs to be centered, preferably /12
# age not centered: RS_age lowest AIC
# age centered: RS_age_water lowest AIC
