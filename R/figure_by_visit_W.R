library(ggplot2)
library(tidyverse)
library(dplyr)
library(gtsummary)
library(flextable)
ethiopia <- read.csv("data/processed/ethiopia_processed.csv")
ethiopia_cc <- read.csv("data/processed/ethiopia_complete_cases.csv")
ethiopia_cc |> 
  mutate(
    yc = factor(yc, levels=c(0,1), labels=c("Old","Young")),
    drwaterq_new = as.factor(drwaterq_new),
    chsex = factor(chsex, levels=c(1,2), labels=c("F","M"))
  ) |> 
  # 1. Make 'round' a factor here so ggplot groups automatically
  ggplot(aes(x = factor(round), y = zhfa, col = chsex)) +
  
  # 2. Remove group=round. Add position_dodge to place F/M side-by-side
  geom_violin(fill = NA, position = position_dodge(width = 0)) +
  
  facet_grid(yc ~ drwaterq_new,
             labeller = as_labeller(c("0"="No safe drinking water",
                                      "1"="Access to drinking water",
                                      "Old"="Old", 
                                      "Young"="Young"))) +
  
  # 3. Add group=chsex to the line so it connects the medians correctly
  stat_summary(aes(group = chsex), geom = "line", fun = "median", 
               position = position_dodge(width = 0.7)) +
  
  theme_bw() +
  labs(x = "Survey Round", y = "Height-for-age z-score") +
  # 4. Changed from "none" to "bottom" so you can actually see the color labels!
  theme(legend.position = "right")
ggsave("output/figure_by_visit.png", 
       width=14,height=12,unit="cm")
