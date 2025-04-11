# setup =========================================================================================================
rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)

d <- here("data","clean","choice.csv") %>%
  read_csv() %>%
  filter(effect!="catch") 

# check that stimuli came out right =============================================================
dd <- d %>%
  pivot_longer(c(d1_t,d1_c,d1_d,d2_t,d2_c,d2_d)) %>%
  separate(name,into=c("dim","option")) %>%
  mutate(value=as.numeric(value)) %>%
  pivot_wider(names_from = dim,
              values_from = value) 
dd %>%
  ggplot(aes(d1,d2,label=option,col=category))+
  geom_text(alpha=.5)+
  coord_fixed(xlim=c(0,120),ylim=c(0,120))+
  facet_grid(effect~t_high+category)+
  ggthemes::theme_few()

# rt ============================================================================================================
d %>%
  ggplot(aes(rt))+
  geom_histogram(fill="lightblue")+
  ggthemes::theme_few()

# choices ============================================================================================================
d %>%
  count(participant,choice) %>%
  group_by(participant) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = choice,
              values_from = prop,
              values_fill = 0) %>%
  mutate(rst=t/(t+c))

# choices ============================================================================================================
p <- d %>%
  group_by(participant,choice,effect) %>%
  summarise(n=n()) %>%
  group_by(participant,effect) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = choice,
              values_from = prop,
              values_fill = 0) 

pp <- d %>%
  group_by(participant,choice,category) %>%
  summarise(n=n()) %>%
  group_by(participant,category) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = choice,
              values_from = prop,
              values_fill = 0) 

