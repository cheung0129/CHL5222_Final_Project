library(lme4)
library(gtsummary)
library(flextable)
library(broom.mixed)
library(texreg)
library(ggplot2)
library(patchwork)
library(lattice)
library(ggeffects)
#library(html2latex)

load("data/processed/lmmdata.RData")
ethiopia_old <- ethiopia_df |> dplyr::filter(yc==0)

## RI ====
RI <- lmer(zhfa ~ chsex + time + drwaterq_new + age_first_c  + chsex*time + chsex*drwaterq_new + time*drwaterq_new + (1 | childid), data=ethiopia_df)


## RS w/ age ====
RS_age <- lmer(zhfa ~ chsex + time + drwaterq_new + age_first_c  + chsex*time + chsex*drwaterq_new + time*drwaterq_new + (time | childid), data=ethiopia_df)


# summary ====
labels=c("RI","RS_age")

texreg(list(RI,RS_age), custom.coef.names=c(NA, "Sex", "Time", "Access to drinking water", "Baseline age", "Sex:Time", "Sex:Access to drinking water","Time:Access to drinking water"),
       caption="Comparison between the random intercept (RI) model and the random slope (RS) model. The RS model has a random slope on time.")

png("output/figure_random_slope.png", width=25, height=15, units="cm", res=300)
p1 <- update(dotplot(ranef(RI))$childid, main = list(label="A", just="left", x=0.02))
p2 <- update(dotplot(ranef(RS_age))$childid, main = list(label="B", just="left", x=0.02))
print(p1, split = c(1, 1, 2, 1), more = TRUE)  # Places plot1 in column 1
print(p2, split = c(2, 1, 2, 1), more = FALSE)
dev.off()
