rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
d <- here("data","clean","demo.csv") %>%
  read_csv()
(d$exp_duration/1000)/60
