library(lme4)
library(gtsummary)
library(flextable)
library(broom.mixed)
library(sjPlot)
library(ggplot2)
library(patchwork)
library(lattice)
library(ggeffects)

load("data/processed/lmmdata.RData")
ethiopia_old <- ethiopia_df |> dplyr::filter(yc==0)

## RI ====
RI <- lmer(zhfa ~ chsex + time + drwaterq_new + age_first_c  + chsex*time + chsex*drwaterq_new + time*drwaterq_new + (1 | childid), data=ethiopia_df)


## RS w/ age ====
RS_age <- lmer(zhfa ~ chsex + time + drwaterq_new + age_first_c  + chsex*time + chsex*drwaterq_new + time*drwaterq_new + (time | childid), data=ethiopia_df)


# summary ====
labels=c("RI","RS_age")

tab_model(RI, RS_age,
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
