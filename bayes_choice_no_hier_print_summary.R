# setup =========================================================================================================
rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
library(bayesplot)

# WHICH MODEL
which_model <- "bayes_choice_dm_no_hier_1" 

# file directory stuff
results_dir <- here(glue("analysis/bayes_choice/{which_model}"))

load(path(results_dir,"theta_m_summary.RData"))
theta_m_summary %>%
  mutate(distance=factor(distance,levels=c("near","far"))) %>%
  arrange(effect,distance)
