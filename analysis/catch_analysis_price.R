rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)

d <- here("data","clean","price.csv") %>%
  read_csv() %>%
  filter(effect=="catch") %>%
  mutate(price_b=case_when(
    str_sub(order,1,1)=="b"~price_1,
    str_sub(order,2,2)=="b"~price_2,
    str_sub(order,3,3)=="b"~price_3
  ),
  price_w1=case_when(
    order=="wbw"~price_1,
    order=="wwb"~price_1,
    order=="bww"~price_2,
  ),
  price_w2=case_when(
    order=="wbw"~price_3,
    order=="wwb"~price_2,
    order=="bww"~price_3,
  )) %>%
  select(-c(price_1,price_2,price_3))

# check RTs ================================================================================
d %>%
  ggplot(aes(rt))+
  geom_histogram(fill="lightblue")+
  scale_x_continuous(limits=c(0,100))+
  ggthemes::theme_few()
summary(d$rt)

# check prices overall, by category ==========================================================
d_long <- d %>%
  pivot_longer(contains("price"),names_to = "option", values_to = "price") %>%
  mutate(option=str_remove(option,"price_"))
d_long %>%
  ggplot(aes(price))+
  geom_histogram(fill="lightblue")+
  scale_x_continuous(labels = scales::dollar_format())+
  facet_grid(category~.)+
  ggthemes::theme_few()

d_long %>%
  mutate(value=case_when(
    option=="b"~d1_best+d2_best,
    option=="w1"~d1_worst1+d2_worst1,
    option=="w2"~d1_worst2+d2_worst2,
  )) %>%
  ggplot(aes(value,price))+
  geom_point()

