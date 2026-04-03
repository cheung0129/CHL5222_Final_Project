library(here)
library(gtsummary)
library(gt)

setwd(here())
## data cleaning 
ethiopia <- read.csv("data/processed/ethiopia_processed.csv") |> 
  dplyr::select(!childid) |> 
  mutate(chsex=factor(chsex,level=c(1,2), label=c("F","M")))
yc <- ethiopia |> dplyr::filter(yc==1) |>  dplyr::select(round,agemon,chsex,zhfa, drwaterq_new)
oc <- ethiopia |> dplyr::filter(yc==0) |>  dplyr::select(round,agemon,chsex,zhfa, drwaterq_new)
labels <- list(agemon="Age (months)", chsex="Sex", 
               zhfa="Height-for-age Z score", drwaterq_new="Access to safe water")

## younger cohort 
p1 <- tbl_summary(yc, by=round,
                  missing_text = "Missing", missing_stat = "{p_miss}%",
                  statistic = list(all_continuous() ~ "{mean}\t({min}, {max})",
                                   all_categorical() ~ "{n}\t({p}%)"),
                  label=labels) |> 
  modify_header(all_stat_cols() ~ "**{level}**\nN={n}", label="Variable")


## older cohort 
p2 <- tbl_summary(oc, by=round,
                  missing_text = "Missing", missing_stat = "{p_miss}%",
                  statistic = list(all_continuous() ~ "{mean}\t({min}, {max})",
                                   all_categorical() ~ "{n}\t({p}%)"),
                  label=labels) |> 
  modify_header(all_stat_cols() ~ "**{level}**\nN={n}", label="Variable")

## combined
tbl_merge(list(p1,p2),tab_spanner =c("**Younger Cohort**", "**Older Cohort**")) |> 
  # as_kable_extra(format="latex") |> landscape()
  as_kable_extra(format="latex",booktabs=T) |> 
  column_spec(1:11, width = "1.68cm")

                  