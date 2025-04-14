rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
d <- here("data","clean","demo.csv") %>%
  read_csv()
(d$exp_duration/1000)/60
table(d$race)
hist(d$age)
mean(d$age,na.rm=T)
table(d$ethnicity)

table(d$race,d$ethnicity)
