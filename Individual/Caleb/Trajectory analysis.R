library(ggplot2)
library(tidyverse)
data = read.csv("data/processed/ethiopia_processed.csv")


### By sex
data %>%
  mutate(chsex = factor(chsex,
                     levels = c(1,2),
                     ))%>%
  group_by(chsex, round) %>% 
  summarise(m = mean(zhfa,na.rm = TRUE))%>%
  ggplot(aes(x = as.factor(round),
             y = m,
             colour = chsex,
             group = chsex)) +
  geom_point()+
  geom_line()+
  labs(colour = "Sex",
       x = "Round of survey", 
       y = "Height-for-age z-score",
       title = "Height-for-age z-score by Sex")


### By Cohort
data %>%
  mutate(yc = factor(yc,
                     levels = c(0,1),
                     labels = c("Older", "Younger")))%>%
  group_by(yc, round) %>% 
  summarise(m = mean(zhfa,na.rm = TRUE))%>%
  ggplot(aes(x = as.factor(round),
             y = m,
             colour = yc,
             group = yc)) +
  geom_point()+
  geom_line()+
  labs(colour = "Cohort",
       x = "Round of survey", 
       y = "Height-for-age z-score",
       title = "Height-for-age z-score by Cohort")

### By access of drinking water
data %>% 
  filter(drwaterq_new %in% c(0,1)) %>%
  mutate(water = factor(drwaterq_new,
                        levels = c(0,1),
                        labels = c("No","Yes")
  ))%>%
  group_by(water, round) %>% 
  summarise(m = mean(zhfa,na.rm = TRUE))%>%
  ggplot(aes(x = as.factor(round),
             y = m,
             colour = water,
             group = water)) +
  geom_point()+
  geom_line()+
  labs(colour = "Access of drinking water",
       x = "Round of survey", 
       y = "Height-for-age z-score",
       title = "Height-for-age z-score by access of drinking water")



### Individual
data %>% 
  ggplot(aes(x = as.factor(round),
             y = zhfa,
             group = as.factor(childid))) +
  geom_point()+
  geom_line(alpha = 0.4)+
  labs(x = "Round of survey", 
       y = "Height-for-age z-score",
       title = "Individual Trajectory")
