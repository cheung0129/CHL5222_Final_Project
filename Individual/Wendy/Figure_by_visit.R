library(ggplot2)
library(tidyverse)
library(dplyr)
library(gtsummary)
library(flextable)
ethiopia <- read.csv("data/processed/ethiopia_processed.csv")
ethiopia_cc <- read.csv("data/processed/ethiopia_complete_cases.csv")


ethiopia_cc |> 
  mutate(yc=factor(yc, levels=c(0,1), labels=c("Old","Young")),
         drwaterq_new=as.factor(drwaterq_new)) |> 
  ggplot(aes(x=round, y=zhfa, col=drwaterq_new))+
  geom_violin(aes(group=round),fill=NA)+
  facet_grid(yc~drwaterq_new,
             labeller=as_labeller(c("0"="No safe drinking water","1"="Access to drinking water",
                                    "Old"="Old", "Young"="Young")))+
  stat_summary(geom="line",fun="median")+
  theme_bw()+
  labs(x="Survey Round", y="Height-for-age z-score")+
  theme(legend.position = "none")
ggsave("individual/Wendy/output/figure_by_visit.png", 
       width=24,height=21,unit="cm")
