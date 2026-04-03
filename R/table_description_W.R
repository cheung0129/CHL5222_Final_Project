library(here)
library(dplyr)
library(gtsummary)

setwd(here())
## data cleaning 
ethiopia <- read.csv(paste0(here(),"/data/processed/ethiopia_processed.csv")) |> 
  dplyr::select(!childid) |> 
  mutate(chsex=factor(chsex,level=c(1,2), label=c("F","M")))
yc <- ethiopia |> dplyr::filter(yc==1) |> dplyr::select(round,agemon,chsex,zhfa, drwaterq_new)
oc <- ethiopia |> dplyr::filter(yc==0) |> dplyr::select(round,agemon,chsex,zhfa, drwaterq_new)
labels <- list(agemon="Age (months)", chsex="Sex", 
               zhfa="Height-for-age Z score", drwaterq_new="Access to safe water")

## younger cohort 
p1 <- tbl_summary(yc, by=round,
                  missing_text = "Missing", missing_stat = "{p_miss}%",
                  statistic = all_continuous() ~ "{mean} ({min}, {max})",
                  label=labels) 


## older cohort 
p2 <- tbl_summary(oc, by=round,
                  missing_text = "Missing", missing_stat = "{p_miss}%",
                  statistic = all_continuous() ~ "{mean} ({min}, {max})",
                  label=labels) 

## combined
tbl_merge(list(p1,p2), tab_spanner=c("**Younger Cohort**", "**Older Cohort**")) |> 
  as_kable(format="latex")
