library(ggplot2)
library(tidyverse)
library(dplyr)
library(gtsummary)
library(flextable)
ethiopia <- read.csv("data/processed/ethiopia_processed.csv")


ethiopia |> 
  mutate(yc=factor(yc, levels=c(0,1), labels=c("Old","Young"))) |> 
  ggplot(aes(x=agemon, y=zhfa,  col=round))+
  geom_point()+
  stat_smooth(col="red")+
  # geom_violin(trim =F, fill=NA, col='red')+
  facet_grid(yc~round, scales="free_x")+
  theme_bw()+
  labs(x="Age in months", y="Height-for-age z-score")
ggsave("individual/Wendy/output/figure_by_visit.png", 
       width=24,height=21,unit="cm")
