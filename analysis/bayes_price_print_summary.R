rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
library(bayesplot)
library(latex2exp)

# settings
which_model <- "bayes_price_2"
model_file <- here("analysis","bayes_price","stan",glue("{which_model}.stan"))
results_dir <- here("analysis","bayes_price",which_model)
load(path(results_dir,"fit_summary.RData"))
fit_summary %>%
  filter(str_detect(variable,"cor")) %>%
  select(variable,mean,sd,q2.75,q97.5)
