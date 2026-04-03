# import packages
library(here)
library(dplyr)
library(lubridate)

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
data_formatted <- data_processed %>% 
  mutate(childid = factor(childid),
         yc = factor(yc, levels = c(1,0)),
         round = factor(round, levels = c(1,2,3,4,5)),
         dint = lubridate::mdy(dint),
         chsex = factor(chsex, levels = c(1,2)),
         drwaterq_new = factor(drwaterq_new, levels = c(1,0)))

str(data_formatted)


# save processed file
save_path_processed <- here("data","processed","ethiopia_processed.csv")

write.csv(data_formatted, save_path_processed)



# data set with complete case analysis/listwise deletion
# data_complete_cases <- data_formatted[complete.cases(data_formatted),] 
# str(data_complete_cases)

# save data set with removed missing observations
# save_path_complete_cases <- here("data","processed","ethiopia_complete_cases.csv")
# 
# write.csv(data_complete_cases, save_path_complete_cases)
