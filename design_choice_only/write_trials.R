# this code takes the stimuli from the stim_crit excel file and writes a json file for the experiment
# I then modify the json file (in the experiment folder) to be a javascript variable
rm(list=ls())
library(readxl)
library(tidyverse)
library(here)
library(jsonlite)
s <- here("design_choice_only","stim_crit.xlsx") %>%
  read_excel()
effect <- unique(s$effect)
t_high <- unique(s$t_high)
set <- unique(s$set)
category <- c("microwave ovens","laptops","washing machines","televisions")
trials <- expand_grid(effect,t_high,category) %>%
  mutate(trial_id=1:n()) %>%
  right_join(s, by=c("effect","t_high"), relationship="many-to-many") %>%
  pivot_wider(names_from = option, values_from = c(d1,d2)) %>%
  select(-trial_id) %>%
  bind_rows(
    tibble(effect=rep("catch",4), # 8 catch trials
           category=rep(category,1),
           set="trinary"),
    tibble(effect=rep("catch",4), # 8 catch trials
           category=rep(category,1),
           set="binary"),
  )
trials %>%
  count(effect,set)

write_csv(trials,file=here("design_choice_only","trials.csv"))
write_json(trials,path=here("design_choice_only","trials.json"))
write_json(trials,path=here("experiment_choice_only","trials.json"))
