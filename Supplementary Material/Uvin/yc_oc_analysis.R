library(readr)
library(tidyverse)
library(lme4)

library(sjPlot)

## Load dataset
load("data/processed/lmmdata.RData")


#################### full model ####################
base_model<- lmer(zhfa ~ chsex + time + age_first_c + drwaterq_new +
                    chsex*time*drwaterq_new +
                    (1 | childid),
                  data = ethiopia_df,
                  REML = FALSE)
summary(base_model)

interaction_model<- lmer(zhfa ~ chsex + time + age_first_c + drwaterq_new +
                           chsex*time*drwaterq_new +
                    yc * (chsex + agemon + drwaterq_new) +
                    (1 | childid),
                  data = ethiopia_df,
                  REML = FALSE)
summary(interaction_model)


anova(base_model, interaction_model)

####the effects of sex, age, and water quality
####differ between younger and older children 

#################### old cohort ####################
oc_model<- lmer(zhfa ~ chsex * time * drwaterq_new + age_first_c + 
                  (1 | childid),
                data = ethiopia_old,
                REML = FALSE)
summary(oc_model)
oc_model_reduced <- lmer(zhfa ~ chsex * time + chsex * drwaterq_new + time * drwaterq_new +
                           age_first_c +
                           (1 | childid),
                         data = ethiopia_old,
                         REML = FALSE)
anova(oc_model, oc_model_reduced)

#The three‑way interaction model provided a better fit to the oc data
#Height for age differ by sex, water access and the combination of both
##The slope over time is not the same for boys and girls, and that difference 
#changes depending on whether the household has safe drinking water.

#################### yound cohort ####################
yc_model<- lmer(zhfa ~ chsex * time * drwaterq_new + age_first_c +
                  (1 | childid),
                data = ethiopia_yg,
                REML = FALSE)
summary(yc_model)
yc_model_reduced <- lmer(zhfa ~ chsex * time + chsex * drwaterq_new + time * drwaterq_new +
                           age_first_c +
                           (1 | childid),
                         data = ethiopia_yg,
                         REML = FALSE)
anova(yc_model, yc_model_reduced)

# For the younger cohort, the three-way interaction did NOT improve model fit.
# Height-for-age still varies by sex, water access, and their two-way combinations,
# The slope over time differs slightly for boys and girls, but this difference
# does NOT depend on water access in the younger cohort.



tab_model(base_model, oc_model, yc_model,
          show.ci = F,
          show.se = TRUE,
          dv.labels = c("Pooled", "Older", "Younger"))

######
# Across the full sample and the older cohort, the three-way interaction 
# (sex × time × water access) meaningfully improves model fit, indicating that 
# growth trajectories differ by sex, water access, and their combined effect. 
# In both groups, the slope over time is different for boys and girls, and this 
# difference depends on whether they have access to safe water.

# In contrast, the younger cohort shows no evidence that the three-way interaction 
# improves model fit. Growth patterns vary by sex and water access through two-way 
# interactions, but the combined three-way effect is not meaningful. The difference 
# in growth slopes between boys and girls does NOT depend on water access in the 
# younger cohort.

# Overall: the three-way interaction is important for older children and the full 
# sample, but not for younger children.


