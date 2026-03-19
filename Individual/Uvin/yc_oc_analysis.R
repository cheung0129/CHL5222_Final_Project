library(readr)
library(tidyverse)
library(lme4)
library(sjPlot)

## Load dataset
setwd("~/CHL5222_Final_Project")
eth <- read_csv("https://raw.githubusercontent.com/cheung0129/CHL5222_Final_Project/refs/heads/main/data/processed/ethiopia_processed.csv")


eth <- eth |>
  mutate(chsex = factor(chsex,
                        levels = c(1, 2)),
         yc = factor(yc,
                     levels = c(0, 1)),
         round = factor(round),
         drwaterq_new = factor(drwaterq_new,
                               levels = c(0, 1)))
#older cohort
eth_oc <- eth |>
  filter(yc == "0")
#younger vohort
eth_yc <- eth |>
  filter(yc == "1")

#full model
full_model<- lmer(zhfa ~ chsex + agemon + drwaterq_new + chsex*agemon +
                    chsex*drwaterq_new + agemon*drwaterq_new +
                    chsex*agemon*drwaterq_new +
                    (1 | childid),
                  data = eth,
                  REML = FALSE)


interaction_model<- lmer(zhfa ~ chsex + agemon + drwaterq_new + chsex*agemon +
                    chsex*drwaterq_new + agemon*drwaterq_new +
                    chsex*agemon*drwaterq_new +
                    yc * (chsex + agemon + drwaterq_new) +
                    (1 | childid),
                  data = eth,
                  REML = FALSE)


anova(full_model, interaction_model)



yc * (chsex + agemon + drwaterq_new)

anova(pooled_model, interaction_model)


#old cohort
oc_model<- lmer(zhfa ~ chsex + agemon + drwaterq_new + chsex*agemon +
                    chsex*drwaterq_new + agemon*drwaterq_new +
                    chsex*agemon*drwaterq_new +
                    (1 | childid),
                  data = eth_oc,
                  REML = FALSE)
summary(oc_model)

#yound cohort
yc_model<- lmer(zhfa ~ chsex + agemon + drwaterq_new + chsex*agemon +
                  chsex*drwaterq_new + agemon*drwaterq_new +
                  chsex*agemon*drwaterq_new +
                  (1 | childid),
                data = eth_yc,
                REML = FALSE)
summary(yc_model)


#

oc_model_reduced <- update(oc_model, . ~ . - chsex:agemon:drwaterq_new)
yc_model_reduced <- update(yc_model, . ~ . - chsex:agemon:drwaterq_new)

anova(oc_model, oc_model_reduced)
anova(yc_model, yc_model_reduced)




tab_model(full_model, oc_model, yc_model,
          show.ci = TRUE,
          show.se = TRUE,
          dv.labels = c("Pooled", "Older", "Younger"))

######
#The older cohort shows stronger and more interpretable effects, especially for sex and water quality.
#The younger cohort appears more homogeneous, with weaker or non-significant effects.
#The 3-way interaction is only meaningful in the older cohort, so you may choose to drop it in the younger model (based on sensitivity analysis).
#These findings suggest that interventions targeting water quality may be more impactful for older children, especially boys.

  