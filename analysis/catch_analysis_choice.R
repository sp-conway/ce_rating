# setup =========================================================================================================
rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)

d <- here("data","clean","choice.csv") %>%
  read_csv() %>%
  filter(effect=="catch") 

# rt ============================================================================================================
d %>%
  ggplot(aes(rt))+
  geom_histogram(fill="lightblue")+
  ggthemes::theme_few()

# choices ============================================================================================================
d %>%
  count(participant,choice) %>%
  group_by(choice,n) %>%
  summarise(N=n())
