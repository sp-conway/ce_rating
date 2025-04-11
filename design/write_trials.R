rm(list=ls())
library(readxl)
library(tidyverse)
library(here)
library(jsonlite)
s <- here("design","stim_crit.xlsx") %>%
  read_excel()
effect <- unique(s$effect)
t_high <- unique(s$t_high)
category <- c("microwave ovens","laptops","washing machines","televisions")
trials <- expand_grid(effect,t_high,category) %>%
  mutate(trial_id=1:n()) %>%
  right_join(s, by=c("effect","t_high"), relationship="many-to-many") %>%
  pivot_wider(names_from = option, values_from = c(d1,d2)) %>%
  select(-trial_id) %>%
  bind_rows(
    tibble(effect=rep("catch",8), # 4 catch trials
           category=rep(category,2))
  )

write_csv(trials,file=here("design","trials.csv"))
write_json(trials,path=here("design","trials.json"))
write_json(trials,path=here("experiment","trials.json"))
