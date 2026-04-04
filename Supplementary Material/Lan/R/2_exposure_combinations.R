# import packages
library(here)
library(dplyr)
library(tidyr)

# for table creation
library(kableExtra)


# load data
path_complete_cases <- here("data","processed","ethiopia_complete_cases.csv")

data_long <- read.csv(path_complete_cases) %>% 
  select(childid, round, drwaterq_new)



# all possible exposure combinations
exposure_combinations <- expand.grid(c(1,0), c(1,0), c(1,0), c(1,0), c(1,0)) %>% 
  mutate(combination = paste(Var1, Var2, Var3, Var4, Var5, sep = "")) %>% 
  mutate(across(c(Var1, Var2, Var3, Var4, Var5), 
                ~ if_else(. == 1, "Yes","No"))) %>% 
  mutate(exposure = paste(Var1, Var2, Var3, Var4, Var5)) %>% 
  select(combination, exposure)


# wide format
data_wide <- data_long %>% 
  pivot_wider(
    names_from = round,
    names_prefix = "round",
    values_from = drwaterq_new
  )

# add combination to data
data_combinations <- data_wide[complete.cases(data_wide),] %>% 
  mutate(combination = factor(paste(round1,
                                    round2, 
                                    round3, 
                                    round4, 
                                    round5,
                                    sep = "")))

# frequency of exposure combinations
exposure_frequency <- data.frame(table(data_combinations$combination)) %>% 
  mutate(combination = Var1) %>% 
  left_join(exposure_combinations, by = "combination") %>% 
  arrange(desc(combination)) %>% 
  mutate(total = sum(Freq),
         percentage = paste("(",
                            as.character(round(Freq / total, 4)*100),
                            "%)",
                            sep = ""),
         frequency = paste(Freq, percentage)) %>% 
  select(combination, exposure, frequency)




# frequency table
exposure_frequency %>% 
  kbl(caption = "Frequency of Exposure Combinations",
      align = "cll") %>% 
  kable_classic(full_width = FALSE, 
                html_font = "Arial")


