# setup =========================================================================
rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)

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
  select(-c(price_1,price_2,price_3))

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

# initial histogram ====================================================================================
price_long <- d %>%
  pivot_longer(contains("price"),names_to = "option", values_to = "price") %>%
  mutate(option=str_remove(option,"price_"))
price_long %>%
  ggplot(aes(price))+
  geom_histogram(fill="lightblue")+
  scale_x_continuous(labels=scales::label_currency())+
  facet_grid(option~.)+
  ggthemes::theme_few()

# z-score everything by participant ========================================================================
price_long_z <- price_long %>%
  group_by(participant) %>%
  mutate(m=mean(price),
         s=sd(price)) %>%
  ungroup() %>%
  mutate(price=(price-m)/s)
price_long_z %>%
  ggplot(aes(price))+
  geom_histogram(fill="lightblue")+
  facet_grid(option~.)+
  ggthemes::theme_few()
price_long_z %>%
  group_by(option) %>%
  summarise(m=mean(price),
            med=median(price))

# mean z by option and category =======================================================================
price_long_z_m <- price_long_z %>%
  group_by(category,option) %>%
  summarise(m=mean(price),
            s=sd(price),
            se=s/sqrt(n()),
            se_lower=m-se,
            se_upper=m+se) %>%
  ungroup()
price_long_z_m %>%
  mutate(category=str_replace(category," ","\n")) %>%
  ggplot(aes(category,m,fill=option))+
  geom_col(position = "dodge",width=.6)+
  geom_hline(yintercept=0,alpha=.4,linetype="dashed")+
  geom_errorbar(aes(ymin=se_lower,ymax=se_upper),position = position_dodge(width=.6),width=.2)+
  labs(x="product category",y="mean price (Z units)")+
  ggsci::scale_fill_startrek()+
  ggthemes::theme_few()+
  theme(text=element_text(size = 10))
ggsave(filename=here("analysis","plots","price_m_z.jpeg"),width=4,height=3)

# mean z by option, category, effect =======================================================================
price_long_z_m_by_opt_cat_eff <- price_long_z %>%
  group_by(category,option, effect) %>%
  summarise(m=mean(price),
            s=sd(price),
            se=s/sqrt(n()),
            se_lower=m-se,
            se_upper=m+se) %>%
  ungroup()
price_long_z_m_by_opt_cat_eff %>%
  ggplot(aes(category,m,fill=option))+
  geom_col(position = "dodge",width=.6)+
  geom_errorbar(aes(ymin=se_lower,ymax=se_upper),position = position_dodge(width=.6),width=.2)+
  labs(x="product category",y="mean price (Z units)")+
  coord_flip()+
  facet_grid(effect~.)+
  ggsci::scale_fill_startrek()+
  ggthemes::theme_few()


# mean z by option, effect =======================================================================
price_long_z_m_by_eff <- price_long_z %>%
  group_by(option, effect) %>%
  summarise(m=mean(price),
            s=sd(price),
            se=s/sqrt(n()),
            se_lower=m-se,
            se_upper=m+se) %>%
  ungroup()
price_long_z_m_by_eff %>%
  ggplot(aes(effect,m,fill=option))+
  geom_col(position = "dodge",width=.6)+
  geom_errorbar(aes(ymin=se_lower,ymax=se_upper),position = position_dodge(width=.6),width=.2)+
  labs(x="product category",y="mean price (Z units)")+
  ggsci::scale_fill_startrek()+
  ggthemes::theme_few()

# correlations ====================================================
price_wide_z <- price_long_z %>%
  pivot_wider(names_from = option,
              values_from = price)
round(cor(price_wide_z[c("t","c","d")]),digits=4)


