# import packages
library(here)
library(dplyr)

path_ethiopia_processed <- here("data","processed","ethiopia_processed.csv")

ethiopia_processed <- read.csv(path_ethiopia_processed, row.names = 1) %>% 
  mutate(yc = factor(yc, levels = c(0,1)),
         childid = factor(childid),
         round = factor(round, c(1:5)),
         dint = as.Date(dint, format = "%Y-%m-%d"),
         chsex = factor(chsex, levels = c(1,2)),
         drwaterq_new = factor(drwaterq_new, levels = c(0,1)))

# dataframe of baseline measurement date, baseline age
ethiopia_first <- ethiopia_processed %>% 
  group_by(childid) %>% 
  arrange(childid, dint) %>% 
  filter(round == 1) %>% 
  mutate(first = dint,
         age_first = agemon) %>% 
  select(childid, first, age_first) %>% ungroup()

# include follow-up time (years)
ethiopia_df <- ethiopia_processed %>% 
  left_join(ethiopia_first) %>% 
  mutate(time = as.numeric(difftime(dint, first, units = "days") / 365.25),
         stunting = ifelse(as.numeric(zhfa) < -2, 1,0)) %>% 
  mutate(stunting = factor(stunting, levels = c(0,1))) %>% 
  mutate(age_first_c = age_first - mean(age_first, na.rm = TRUE))

# YOUNG COHORT ====
ethiopia_yg <- ethiopia_df %>% 
  filter(yc == 1) %>%
  select(-c(age_first_c)) %>% 
  mutate(age_first_c = age_first - mean(age_first, na.rm = TRUE))


# old cohort
ethiopia_old <- ethiopia_df %>% 
  filter(yc == 0) %>%
  filter(round != 5) %>% 
  select(-c(age_first_c)) %>% 
  mutate(age_first_c = age_first - mean(age_first, na.rm = TRUE))



# save datasets as .RData file
path_lmm_datasets <- here("data","processed","lmmdata.RData")
save(ethiopia_df, ethiopia_yg, ethiopia_old, file = path_lmm_datasets)

# load .RData files
# load(path_lmm_datasets)

# ethiopia_df : full dataset
# ethiopia_yg : young cohort (1)
# ethiopia_old : older cohort (0)

# added model variables: first,  time,  age_first_c
# derived variables: age_first, stunting,

