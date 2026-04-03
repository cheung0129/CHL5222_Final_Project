library(tidyverse)
library(lme4)
library(knitr)

## Load dataset
load("data/processed/lmmdata.RData")

#################### Model selection: interaction terms ####################
base_model<- lmer(zhfa ~ chsex + time + age_first_c + drwaterq_new +
                    chsex*time*drwaterq_new +
                    (1 | childid),
                  data = ethiopia_df,
                  REML = FALSE)
summary(base_model)

twoway_model<- lmer(zhfa ~ chsex + time + age_first_c + drwaterq_new+ chsex*time +
                      chsex*drwaterq_new + time*drwaterq_new +
                      (1 | childid),
                    data = ethiopia_df,
                    REML = FALSE)
summary(twoway_model)

nointeraction_model <- lmer(zhfa ~ chsex + time + age_first_c + drwaterq_new+
                              (1 | childid),
                            data = ethiopia_df,
                            REML = FALSE)

model_names <- c("No interaction", "Two-way interaction", "Three-way interaction")



model_table <- data.frame(
  Model = model_names,
  AIC = c(AIC(nointeraction_model), AIC(twoway_model), AIC(base_model)),
  BIC = c(BIC(nointeraction_model), BIC(twoway_model), BIC(base_model))
)

kable(model_table, digits = 2, caption = "Model Comparison Using AIC and BIC")