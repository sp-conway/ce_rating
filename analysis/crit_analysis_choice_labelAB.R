rm(list=ls())
library(tidyverse)
library(here)
library(fs)

d <- here("data","clean","choice.csv") %>%
  read_csv() %>%
  filter(effect!="catch") %>%
  mutate(set=case_match(t_high,1~"a-b-db",2~"a-b-da")) %>%
  mutate(choice1=case_when(
    set=="a-b-da" & choice=="t"~"a",
    set=="a-b-da" & choice=="c"~"b",
    set=="a-b-db" & choice=="t"~"b",
    set=="a-b-db" & choice=="c"~"a",
    choice=="d"~"d"
  ),
  choice=as.factor(choice),
  set=as.factor(set)) %>%
  select(-choice) %>%
  rename(choice=choice1)

props <- d %>%
  group_by(category,effect,set,choice) %>%
  summarise(N=n()) %>%
  group_by(category,effect,set) %>%
  mutate(prop=N/sum(N)) %>%
  ungroup() %>%
  select(-N) 

props %>%
  ggplot(aes(set,prop,fill=choice))+
  geom_col(position="dodge")+
  facet_grid(effect~category)+
  ggthemes::theme_few()
ggsave(filenam=here("analysis","plots","crit_choice_by_set.jpeg"),width=8,height=7)

stim <- here("design/stim_crit.xlsx") %>%
  readxl::read_excel() %>%
  mutate(set=case_match(t_high,1~"a-b-db",2~"a-b-da")) %>%
  mutate(choice=case_when(
    set=="a-b-da" & option=="t"~"a",
    set=="a-b-da" & option=="c"~"b",
    set=="a-b-db" & option=="t"~"b",
    set=="a-b-db" & option=="c"~"a",
    set=="a-b-da" & option=="d"~"da",
    set=="a-b-db" & option=="d"~"db"
  ))
stim %>%
  distinct(effect,choice,d1,d2) %>%
  ggplot(aes(d1,d2,col=choice))+
  geom_point(alpha=.5)+
  coord_fixed(xlim=c(0,120),ylim=c(0,120))+
  facet_grid(effect~.)+
  ggthemes::theme_few()
ggsave(filename = here("experiment/stim_labelAB.jpeg"),width=6,height=6)
