# setup =========================================================================
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
ggsave(filename=here("analysis","plots","price_hist_all.jpeg"))

# mean plots ========================================================================
price_long_m_by_effect_category <- price_long %>%
  group_by(effect,category,option) %>%
  summarise(m=mean(price),
            s=sd(price),
            se=s/sqrt(n()),
            se_lower=m-se,
            se_upper=m+se) %>%
  ungroup()
price_long_m_by_effect_category %>%
  mutate(category=str_replace(category," ","\n")) %>%
  ggplot(aes(category,m,fill=option))+
  geom_col(position = "dodge",width=.6)+
  geom_hline(yintercept=0,alpha=.6)+
  geom_errorbar(aes(ymin=se_lower,ymax=se_upper),position = position_dodge(width=.6),width=.2)+
  labs(x="product category",y="mean price")+
  facet_grid(effect~.)+
  ggsci::scale_fill_startrek()+
  ggthemes::theme_few()+
  theme(text=element_text(size = 10))
ggsave(filename=here("analysis","plots","price_m_by_effect_category.jpeg"),width=5,height=6)

price_long_m_by_effect <- price_long %>%
  group_by(effect,option) %>%
  summarise(m=mean(price),
            s=sd(price),
            se=s/sqrt(n()),
            se_lower=m-se,
            se_upper=m+se) %>%
  ungroup()
price_long_m_by_effect %>%
  ggplot(aes(effect,m,fill=option))+
  geom_col(position = "dodge",width=.6)+
  geom_hline(yintercept=0,alpha=.6)+
  geom_errorbar(aes(ymin=se_lower,ymax=se_upper),position = position_dodge(width=.6),width=.2)+
  labs(x="trial type",y="mean price")+
  ggsci::scale_fill_startrek()+
  ggthemes::theme_few()+
  theme(text=element_text(size = 10))
ggsave(filename=here("analysis","plots","price_m_by_effect.jpeg"),width=5,height=3)

# z-score everything ========================================================================
price_long_z <- price_long %>%
  group_by(participant) %>%
  mutate(m=mean(price),
         s=sd(price)) %>%
  ungroup() %>%
  mutate(price=(price-m)/s)
price_long_z %>%
  ggplot(aes(price))+
  geom_histogram(fill="lightblue")+
  labs(x="price (z)")+
  facet_grid(option~.)+
  ggthemes::theme_few()
ggsave(filename=here("analysis","plots","price_hist_z_all.jpeg"),width=5,height=4)

# mean z by option and category =======================================================================
price_long_z_m <- price_long_z %>%
  group_by(effect,category,option) %>%
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
  facet_grid(effect~.)+
  ggsci::scale_fill_startrek()+
  ggthemes::theme_few()+
  theme(text=element_text(size = 10))
ggsave(filename=here("analysis","plots","price_m_z_by_effect_category.jpeg"),width=5,height=6)

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
  geom_hline(yintercept=0,alpha=.4,linetype="dashed")+
  geom_errorbar(aes(ymin=se_lower,ymax=se_upper),position = position_dodge(width=.6),width=.2)+
  labs(x="trial type",y="mean price (Z units)")+
  ggsci::scale_fill_startrek()+
  ggthemes::theme_few()
ggsave(filename=here("analysis","plots","price_m_z_by_effect.jpeg"),width=6,height=4)

# correlations ====================================================
price_wide_z_FILTERED <- price_long_z %>%
  filter(price<=3 | price>=3) %>%
  pivot_wider(names_from = option,
              values_from = price) %>%
  mutate(effect1=str_extract(effect,"repulsion|attraction"))
compute_cors <- function(dat,cond){
  cors <- round(cor(dat[str_detect(dat$effect,cond),c("t","c","d")]),digits=4)
  print(cond)
  print(cors)
}
walk(c("repulsion","attraction"),compute_cors,dat=price_wide_z_FILTERED)

td <- ggplot(price_wide_z_FILTERED,aes(t,d))+
  geom_point(shape=".")+
  geom_abline(slope=1,intercept=0,linetype="dashed",alpha=.5,col="red")+
  coord_fixed(xlim=c(-3,3),ylim=c(-3,3))+
  scale_x_continuous(breaks = c(-3,0,3))+
  scale_y_continuous(breaks = c(-3,0,3))+
  facet_grid(effect1~.)+
  ggthemes::theme_few()
tc <- ggplot(price_wide_z_FILTERED,aes(t,c))+
  geom_point(shape=".")+
  geom_abline(slope=1,intercept=0,linetype="dashed",alpha=.5,col="red")+
  coord_fixed(xlim=c(-3,3),ylim=c(-3,3))+
  scale_x_continuous(breaks = c(-3,0,3))+
  scale_y_continuous(breaks = c(-3,0,3))+
  facet_grid(effect1~.)+
  ggthemes::theme_few()
cd <- ggplot(price_wide_z_FILTERED,aes(c,d))+
  geom_point(shape=".")+
  geom_abline(slope=1,intercept=0,linetype="dashed",alpha=.5,col="red")+
  coord_fixed(xlim=c(-3,3),ylim=c(-3,3))+
  scale_x_continuous(breaks = c(-3,0,3))+
  scale_y_continuous(breaks = c(-3,0,3))+
  facet_grid(effect1~.)+
  ggthemes::theme_few()
tc|td|cd
ggsave(filename=here("analysis","plots","price_z_corplot.jpeg"),width=6,height=4)

price_wide_z_FILTERED$d_best <- price_wide_z_FILTERED$d>price_wide_z_FILTERED$t & price_wide_z_FILTERED$d>price_wide_z_FILTERED$c
price_wide_z_FILTERED %>%
  group_by(effect1) %>%
  summarise(pdbest=mean(d_best))

price_wide_z_FILTERED$d_bestt <- price_wide_z_FILTERED$d>price_wide_z_FILTERED$t
price_wide_z_FILTERED %>%
  group_by(effect1) %>%
  summarise(pdbestt=mean(d_bestt))
