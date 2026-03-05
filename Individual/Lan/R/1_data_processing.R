# import packages
library(here)
library(dplyr)

# load data
path_data_original <- here("data","original","ethiopia.csv")

data_original <- read.csv(path_data_original)

str(data_original)
summary(data_original)

# transforming

# character variables need to be removed for white spaces : dint
data_original %>% filter(grepl(" ", childid)) %>% select(childid) %>% unique()
data_original %>% filter(grepl(" ", dint)) %>% select(dint) %>% unique()

# check unique values that are non-numerical
# yc, round, chsex, agemon, zhfa, drwaterq_new
data_original %>% select(yc) %>% unique()
data_original %>% select(round) %>% unique()
data_original %>% select(chsex) %>% unique()
data_original %>% select(agemon) %>% summary()
data_original %>% select(zhfa) %>% summary()
data_original %>% select(drwaterq_new) %>% unique()

data_processed <- data_original %>% 
  mutate(dint = if_else(grepl(" ", dint), NA, dint))


# formatting
data_processed <- data_original %>% 
  mutate(yc = factor(yc, levels = c(1,0)),
         round = factor(round, levels = c(1,2,3,4,5)),
         dint = )

# factor
# - yc
# - round
# - chsex
# - dfwaterq_new
# 
# datetime
# - dint

# save processed file