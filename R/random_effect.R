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
RI <- lmer(zhfa ~ chsex + time + drwaterq_new + age_first_c  + chsex*time + chsex*drwaterq_new + time*drwaterq_new + (1 | childid), data=ethiopia_df)


## RS w/ water ====
## reasoning of random slope for drwaterq_new: access to drinking water is not constant within each child 
RS_water <- lmer(zhfa ~ chsex + time + drwaterq_new + age_first_c  + chsex*time + chsex*drwaterq_new + time*drwaterq_new + (drwaterq_new | childid) , data=ethiopia_df)

## RS w/ age ====
RS_age <- lmer(zhfa ~ chsex + time + drwaterq_new + age_first_c  + chsex*time + chsex*drwaterq_new + time*drwaterq_new + (time | childid), data=ethiopia_df)

## RS w/ age & water ====
RS_age_water <- lmer(zhfa ~ chsex + time + drwaterq_new + age_first_c  + chsex*time + chsex*drwaterq_new + time*drwaterq_new +  (time + drwaterq_new | childid), data=ethiopia_df)


# summary ====
labels=c("RI","RS_water","RS_age", "RS_age_water")

tab_model(RI, RS_age,RS_water, RS_age_water,
          dv.labels = labels, 
          show.ci=F, show.se=T, show.aic=T,
          show.r2 = F, show.obs=F, show.ngroups = F,
          terms=NA,
          file="output/table_random_effect.doc",
          p.style="star"
          )

png("output/figure_random_slope.png", width=20, height=20, units="cm", res=300)
dotplot(ranef(RS_age))
dev.off()
