library(readr)
library(tidyverse)
library(table1)

## Load dataset
setwd("~/CHL5222_Final_Project")
eth <- read_csv("https://raw.githubusercontent.com/cheung0129/CHL5222_Final_Project/refs/heads/main/data/processed/ethiopia_processed.csv")

## Table 1
table1_eth <- eth |>
  mutate(chsex = factor(chsex,
                        levels = c(1, 2),
                        labels = c("Female", "Male")),
         yc = factor(yc,
                     levels = c(0, 1),
                     labels = c("Older cohort", "Younger cohort")),
         round = factor(round),
         drwaterq_new = factor(drwaterq_new,
                               levels = c(0, 1),
                               labels = c("No safe water", "Safe water")))

#Label
label(table1_eth$agemon)        <- "Age (months)"
label(table1_eth$zhfa)          <- "Height-for-age z-score"
label(table1_eth$chsex)         <- "Sex"
label(table1_eth$round)         <- "Survey round"
label(table1_eth$drwaterq_new)  <- "Access to safe water"
label(table1_eth$yc)            <- "Cohort"



table1_by_ycround <- table1(
  ~ agemon + zhfa + chsex + drwaterq_new |
    yc * round,
  data = table1_eth,
  topclass = "Rtable1-stripe",
  overall = FALSE
)