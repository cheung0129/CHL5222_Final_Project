
# notes:
# age at baseline always occurs within the data - age in round 1 exists for all
# time is defined as follow-up time since baseline (in years)

# import packages
library(lme4)
library(here)
library(dplyr)
library(lmerTest)

library(ggeffects)
library(ggplot2)

# load data and refactor
path_ethiopia_processed <- here("Assignments","Prep work","ethiopia_processed.csv")

ethiopia_processed <- read.csv(path_ethiopia_processed, row.names = 1) %>% 
  mutate(childid = factor(childid),
         round = factor(round, c(1:5)),
         dint = as.Date(dint, format = "%Y-%m-%d"),
         chsex = factor(chsex, levels = c(1,2)),
         drwaterq_new = factor(drwaterq_new, levels = c(0,1)))

# dataframe of baseline measurement date, baseline age
ethiopia_first <- ethiopia_processed %>% 
  group_by(childid) %>% 
  arrange(childid, dint) %>% 
  filter(round == 1) %>% 
  mutate(int_first = dint,
         age_first = agemon) %>% 
  select(childid, int_first, age_first) %>% ungroup()

# include follow-up time (years)
ethiopia_df <- ethiopia_processed %>% 
  left_join(ethiopia_first) %>% 
  mutate(time = as.numeric(difftime(dint, int_first, units = "days") / 365.25),
         stunting = ifelse(as.numeric(zhfa) < -2, 1,0)) %>% 
  mutate(stunting = factor(stunting, levels = c(0,1)))

# YOUNG COHORT ====

ethiopia_yg <- ethiopia_df %>% 
  filter(yc == 1) %>% 
  mutate(age_first_c = age_first - mean(age_first, na.rm = TRUE))


# missingness in absolute cases
table(ethiopia_yg$drwaterq_new, ethiopia_yg$round, useNA = "ifany")



# Use Follow-up years as Time

# base model: follow-up as time
mod_base_time <- lmer(zhfa ~ chsex + time + drwaterq_new +
                        chsex:time + chsex:drwaterq_new +
                        age_first_c + 
                        (1 | childid),
                      data = ethiopia_yg)
summary(mod_base_time)

# base model: agemon as time
mod_base_age <- lmer(zhfa ~ chsex + agemon + drwaterq_new +
                       chsex:agemon + chsex:drwaterq_new +
                       (1 | childid),
                     data = ethiopia_yg)
summary(mod_base_age)

# CI confint(mod_base_time)



# interaction terms in base model:
# sex * time ~ with age, development differs with sex
# sex * water ~ water may affect development across sex differently
# sex * time * water ~ the effect of development across sex differs with access

# not in base:
# access * time ~ with age, development differs across access



# LRT - ML instead of REML (comparing models with different fixed effects)
mod_base <- lmer(zhfa ~ chsex + time + drwaterq_new +
                   chsex:time + chsex:drwaterq_new +
                   chsex:time:drwaterq_new +
                   age_first_c + 
                   (1 | childid),
                 data = ethiopia_yg,
                 REML = FALSE)
summary(mod_base)


mod_2way <- lmer(zhfa ~ chsex + time + drwaterq_new + age_first_c +
                   chsex:time + chsex:drwaterq_new + time:drwaterq_new +
                   (1 | childid), 
                 data = ethiopia_yg,
                 REML = FALSE)
summary(mod_2way)


mod_full <- lmer(zhfa ~ chsex + time + drwaterq_new + age_first_c +
                        chsex:time + chsex:drwaterq_new + time:drwaterq_new +
                        chsex:time:drwaterq_new +
                        (1 | childid),
                 data = ethiopia_yg,
                 REML = FALSE)
summary(mod_full)


mod_par2 <- lmer(zhfa ~ chsex + time + drwaterq_new +
                   chsex:time + chsex:drwaterq_new +
                   age_first_c + 
                   (1 | childid),
                 data = ethiopia_yg,
                 REML = FALSE)
summary(mod_par2)



# Hypothesis Testing
# testing if data supports a 3-way interaction
# model fit not improved by 3-way interaction
anova(mod_2way, mod_full)

# testing if removing the time:access interaction negatively affects model fit
# they are the same - mistake to exclude interaction term
anova(mod_base, mod_full)

# testing if removed variable from base, when included improves model fit
# it does not
anova(mod_par2, mod_2way)



# final model, REML
mod_final_time <- lmer(zhfa ~ chsex + time + drwaterq_new +
                    chsex:time + chsex:drwaterq_new +
                    age_first_c + 
                    (1 | childid),
                  data = ethiopia_yg)
summary(mod_final_time)


# scaled residuals magnitude -> outliers?
# boxplot(ethiopia_yg$zhfa)
# 
# # identify large outcome values as potential outliers
# ethiopia_yg %>% arrange(zhfa)
# ethiopia_yg %>% filter(childid == "ET130076")





# plot: time grouped by sex, separated by access
# sex and time affect observed
# potentially little sex and time effect at baseline (for those without access)
plot(ggpredict(mod_final_time, terms = c("time", "chsex", "drwaterq_new"))) +
  labs(
    title = "Model-Based Means Over Time, Grouped by Sex",
    x = "Time",
    y = "Predicted Value of Y",
    color = "Sex"
  ) +
  theme_minimal()


# plot: scores partitioned by sex, grouped by water access
# having access associated with higher zhfa
# lack of interaction effect (access and sex)
plot(ggpredict(mod_final_time, terms = c("chsex", "drwaterq_new"))) +
  labs(
    title = "Model-Based Means Over Time, Grouped by Water Access",
    x = "Sex",
    y = "Predicted Value of ZHFA",
    color = "Access"
  ) +
  theme_minimal()



# plot: time grouped by access, separated by sex
# lack of time and access effect (parallel, across sex)
plot(ggpredict(mod_final_time, terms = c("time", "drwaterq_new", "chsex"))) +
  labs(
    title = "Model-Based Means Over Time, Grouped by Access",
    x = "Time",
    y = "Predicted Value of Y",
    color = "Group"
  ) +
  theme_minimal()


# confirmation in 2-variable plot
# lack of effect of access
plot(ggpredict(mod_final_time, terms = c("time", "drwaterq_new"))) +
  labs(
    title = "Model-Based Means Over Time, Grouped by Access",
    x = "Time",
    y = "Predicted Value of Y",
    color = "Water Access"
  ) +
  theme_minimal()



# YOUNGER COHORT - BINARY OUTCOME ====

table(ethiopia_yg$stunting, useNA = "always")

# Use Follow-up years as Time
glmm_base_time <- glmer(stunting ~ chsex + time + drwaterq_new +
                          age_first_c +
                        chsex:time + chsex:drwaterq_new +
                        (1 | childid),
                      data = ethiopia_yg,
                      family = binomial,
                      # default optimizer failed to converge
                      # use a different optimizer and increase number of iterations
                      control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
summary(glmm_base_time)



# base model: agemon as time
# attempted and provided non-identifiability

# glmm_base_age <- glmer(stunting ~ chsex + agemon + drwaterq_new +
#                        chsex:agemon + chsex:drwaterq_new +
#                        (1 | childid),
#                      data = ethiopia_yg,
#                      family = binomial)
# summary(glmm_base_age)

# CI confint(mod_base_time)



# interaction terms in base model:
# sex * time ~ with age, development differs with sex
# sex * water ~ water may affect development across sex differently
# sex * time * water ~ the effect of development across sex differs with access

# not in base:
# access * time ~ with age, development differs across access



# LRT - ML instead of REML (comparing models with different fixed effects
# glmm_2way <- glmer(stunting ~ chsex + time + drwaterq_new + age_first_c +
#                      chsex:time + chsex:drwaterq_new + time:drwaterq_new +
#                      (1 | childid),
#                    data = ethiopia_yg,
#                    family = binomial,
#                    # default optimizer failed to converge
#                    # use a different optimizer and increase number of iterations
#                    control = glmerControl(optimizer = "bobyqa", 
#                                           optCtrl = list(maxfun=2e5)))
# summary(glmm_2way)
# 
# 
# glmm_full <- glmer(stunting ~ chsex + time + drwaterq_new + age_first_c +
#                      chsex:time + chsex:drwaterq_new + time:drwaterq_new +
#                      chsex:time:drwaterq_new +
#                      (1 | childid),
#                    data = ethiopia_yg,
#                    family = binomial,
#                    # default optimizer failed to converge
#                    # use a different optimizer and increase number of iterations
#                    control = glmerControl(optimizer = "bobyqa", 
#                                           optCtrl = list(maxfun=2e5)))
# summary(glmm_full)




# Hypothesis Testing
# testing if data supports a 3-way interaction
# model fit not improved by 3-way interaction
# anova(glmm_2way, glmm_full)
# 
# # testing if removed variable from base, when included improves model fit
# # it does!
# anova(glmm_base_time, glmm_2way)





# final model
glmm_final_time <- glmer(stunting ~ chsex + time + drwaterq_new +
                           age_first_c +
                           chsex:time + chsex:drwaterq_new +
                           (1 | childid),
                         data = ethiopia_yg,
                         family = binomial,
                         # default optimizer failed to converge
                         # use a different optimizer and increase number of iterations
                         control = glmerControl(optimizer = "bobyqa", 
                                                optCtrl = list(maxfun=2e5)))
summary(glmm_final_time)


# scaled residuals magnitude -> outliers?
# boxplot(ethiopia_yg$zhfa)
# 
# # identify large outcome values as potential outliers
# ethiopia_yg %>% arrange(zhfa)
# ethiopia_yg %>% filter(childid == "ET130076")





# plot: time grouped by sex, separated by access
# sex and time affect observed
# potentially no sex and time effect at baseline
 
# You are calculating adjusted predictions on the population-level (i.e. `type = "fixed"`) for
# a *generalized* linear mixed model.
# This may produce biased estimates due to Jensen's inequality. Consider setting
#   `bias_correction = TRUE` to correct for this bias.
#   See also the documentation of the `bias_correction` argument.
# Data were 'prettified'. Consider using `terms="time [all]"` to get smooth plots.
plot(ggpredict(glmm_final_time, 
               terms = c("time [all]", "chsex", "drwaterq_new"), 
               type = "fixed")) +
  labs(
    title = "Predicted Probability of Stunting Over Time",
    subtitle = "Faceted by Water Access",
    x = "Time",
    y = "Probability of Stunting",
    color = "Sex"
  ) +
  theme_minimal()


# plot: scores partitioned by sex, grouped by water access
# having access associated with higher zhfa
# lack of interaction effect (access and sex)
plot(ggpredict(glmm_final_time, terms = c("chsex", "drwaterq_new"),
               type = "fixed")) +
  labs(
    title = "Model-Based Means Over Time, Grouped by Water Access",
    x = "Sex",
    y = "Predicted Value of Stunting",
    color = "Access"
  ) +
  theme_minimal()



# plot: time grouped by access, separated by sex
# lack of time and access effect (parallel, across sex)
plot(ggpredict(glmm_final_time, terms = c("time [all]", "drwaterq_new", "chsex"),
               type = "fixed")) +
  labs(
    title = "Model-Based Means Over Time, Grouped by Access",
    x = "Time",
    y = "Predicted Value of Y",
    color = "Group"
  ) +
  theme_minimal()


# confirmation in 2-variable plot
# lack of effect of access
plot(ggpredict(glmm_final_time, terms = c("time [all]", "drwaterq_new"),
               type = "fixed")) +
  labs(
    title = "Model-Based Means Over Time, Grouped by Access",
    x = "Time",
    y = "Predicted Value of Y",
    color = "Water Access"
  ) +
  theme_minimal()
