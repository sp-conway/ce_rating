rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
d <- here("data","clean","demo.csv") %>%
  read_csv()
(d$exp_duration/1000)/60

d %>%
  summarise(m=mean(age,na.rm=T),
            s=sd(age,na.rm=T))
table(d$gender)
table(d$race)
