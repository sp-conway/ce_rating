# setup =========================================================================
rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)

price <- here("data","clean","price.csv") %>%
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
  select(-c(price_1,price_2,price_3))

choice <- here("data","clean","choice.csv") %>%
  read_csv() %>%
  filter(effect!="catch") %>%
  mutate(choice=factor(choice,levels=c("c","d","t")))

# t c price diffs ==========================================================================================
price_diff <- price %>%
  mutate(effect=str_remove(effect,paste("_1|_2"))) %>%
  pivot_longer(contains("price"),names_to = "option", values_to = "price") %>%
  group_by(participant) %>%
  mutate(m=mean(price),
         s=sd(price)) %>%
  ungroup() %>%
  mutate(price=(price-m)/s) %>%
  pivot_wider(names_from = option,values_from = price) %>%
  mutate(tc_diff=price_t-price_c) %>%
  group_by(participant,effect) %>%
  summarise(mdiff=mean(tc_diff)) %>%
  ungroup()

indiv_corrs <- price %>%
  mutate(effect=str_remove(effect,paste("_1|_2"))) %>%
  group_by(participant,effect) %>%
  summarise(r_td=cor(price_t,price_d,use="na.or.complete"),
            r_tc=cor(price_t,price_c,use="na.or.complete")) %>%
  ungroup() %>%
  mutate(corr_diff=r_td-r_tc) 

indiv_rst <- choice %>%
  mutate(effect=str_remove(effect,paste("_1|_2"))) %>%
  group_by(participant,effect,choice) %>%
  summarise(N=n()) %>%
  ungroup() %>%
  group_by(participant,effect) %>%
  mutate(prop=N/sum(N)) %>%
  select(-N) %>%
  pivot_wider(names_from = choice,
              values_from = prop,
              values_fill = 0) %>%
  mutate(rst=t/(t+c)) %>%
  select(-c(t,c,d))

indiv_corrs_rst <- left_join(indiv_corrs,indiv_rst)
indiv_corrs_rst %>%
  ggplot(aes(corr_diff,rst))+
  geom_point()+
  labs(x="r_td-r_rc")+
  facet_grid(effect~.)+
  ggthemes::theme_few()
ggsave(filename = here("analysis","plots","cor_diff_rst.jpeg"),width=3,height=4)

price_diff_rst <- price_diff %>%
  left_join(indiv_rst)
price_diff_rst %>%
  ggplot(aes(mdiff,rst))+
  geom_point(alpha=.5)+
  labs(x="Mean Target Price-Mean Competitor Price")+
  facet_grid(effect~.)+
  ggthemes::theme_few()
ggsave(filename = here("analysis","plots","mean_price_diff_rst.jpeg"),width=3,height=4)

ggplot(price_diff,aes(mdiff))+
  geom_histogram(fill="lightblue")+
  facet_grid(effect~.)+
  ggthemes::theme_few()
ggsave(filename = here("analysis","plots","mean_price_diff_hist.jpeg"),width=3,height=4)
