rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
library(patchwork)

# read in data and figure out target
d <- here("data","clean","price.csv") %>%
  read_csv() %>%
  filter(effect!="catch") %>%
  mutate(price_t=case_when(
    str_sub(order,1,1)=="t"~price_1,
    str_sub(order,2,2)=="t"~price_2,
    str_sub(order,3,3)=="t"~price_3,
  ),
  price_c=case_when(
    str_sub(order,1,1)=="c"~price_1,
    str_sub(order,2,2)=="c"~price_2,
    str_sub(order,3,3)=="c"~price_3,
  ),
  price_d=case_when(
    str_sub(order,1,1)=="d"~price_1,
    str_sub(order,2,2)=="d"~price_2,
    str_sub(order,3,3)=="d"~price_3,
  )) %>%
  select(-c(price_1,price_2,price_3)) %>%
  pivot_longer(contains("price"),names_to = "opt",values_to = "price") %>%
  group_by(participant) %>%
  mutate(m=mean(price),
         s=sd(price)) %>%
  ungroup() %>%
  mutate(price=(price-m)/s) %>%
  pivot_wider(names_from = opt, values_from = price) %>%
  select(-c(m,s)) %>%
  mutate(effect=str_remove(effect,"_1|_2")) %>%
  arrange(participant, effect, category) 

effects <- unique(d$effect)
categories <- unique(d$category)
n_cat <- length(categories)
N <- (nrow(d)/2)/n_cat
repulsion <- attraction <- array(NA_real_,dim=c(N,n_cat,3))

for(i in 1:n_cat){
  dtmp_att <- filter(d,effect=="attraction" & category==categories[i])
  dtmp_rep <- filter(d,effect=="repulsion" & category==categories[i])
  repulsion[,i,1] <- dtmp_rep$price_t
  repulsion[,i,2] <- dtmp_rep$price_c
  repulsion[,i,3] <- dtmp_rep$price_d
  attraction[,i,1] <- dtmp_att$price_t
  attraction[,i,2] <- dtmp_att$price_c
  attraction[,i,3] <- dtmp_att$price_d
}