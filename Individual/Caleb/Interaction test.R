library(ggplot2)
library(tidyverse)
library(lme4)
library(ggplot2)


data = read.csv("data/processed/ethiopia_processed.csv")

base_model = lmer(zhfa ~ drwaterq_new+
                    chsex + 
                    agemon +
                    chsex*agemon* drwaterq_new + 
                    (1|childid), data = data)
summary(base_model)

two_way_model = lmer(zhfa ~ drwaterq_new+
                       chsex + 
                       agemon +
                       chsex*agemon+
                       chsex* drwaterq_new +
                       agemon * drwaterq_new +
                      (1|childid), data = data)
summary(two_way_model)

no_interaction_model = lmer(zhfa ~ drwaterq_new+
                              chsex + 
                              agemon +
                       (1|childid), data = data)
summary(no_interaction_model)


BIC(base_model)
BIC(two_way_model)
BIC(no_interaction_model)

AIC(base_model)
AIC(two_way_model)
AIC(no_interaction_model)



### Two-way interaction fit the data the best in AIC
### No interaction fit the data the best in BIC


anova(base_model)
anova(two_way_model)
anova(no_interaction_model)