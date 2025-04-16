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
p %>% 
  arrange(desc(d))

pp <- d %>%
  group_by(participant,choice,category) %>%
  summarise(n=n()) %>%
  group_by(participant,category) %>%
  mutate(prop=n/sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = choice,
              values_from = prop,
              values_fill = 0) %>%
  arrange(desc(d))
pp

# att / rep ========================================================
d %>%
  group_by(participant,effect,choice) %>%
  summarise(N=n()) %>%
  group_by(participant,effect) %>%
  mutate(p=N/sum(N)) %>%
  select(-N) %>%
  group_by(effect,choice) %>%
  summarise(m=mean(p),
            s=sd(p),
            se=s/sqrt(n()),
            ci_lower=m-qt(.975,n())*se,
            ci_upper=m+qt(.975,n())*se) %>%
  ungroup() %>%
  ggplot(aes(effect,m,fill=choice))+
  geom_col(position="dodge",width=.6)+
  geom_errorbar(aes(ymin=ci_lower,ymax=ci_upper),position = position_dodge(width=.6),width=.2)+
  labs(x="choice set",y="mean choice proportion")+
  ggsci::scale_fill_startrek()+
  ggthemes::theme_few()
ggsave(filename=here("analysis","plots","crit_choice_meanprops.jpeg"),width=6,height=4)

d %>%
  group_by(effect,choice) %>%
  summarise(N=n()) %>%
  group_by(effect) %>%
  mutate(p=N/sum(N)) %>%
  select(-N) %>%
  group_by(effect,choice) %>%
  ggplot(aes(effect,p,fill=choice))+
  geom_col(position="dodge",width=.6)+
  labs(x="choice set",y="aggregate choice proportion")+
  ggsci::scale_fill_startrek()+
  ggthemes::theme_few()
ggsave(filename=here("analysis","plots","crit_choice_aggprops.jpeg"),width=6,height=4)

d %>%
  group_by(effect,category,choice) %>%
  summarise(N=n()) %>%
  group_by(effect,category) %>%
  mutate(p=N/sum(N)) %>%
  select(-N) %>%
  ggplot(aes(effect,p,fill=choice))+
  geom_col(position="dodge")+
  labs(x="choice set",y="aggregated choice proportion")+
  facet_grid(category~.)+
  ggsci::scale_fill_startrek()+
  ggthemes::theme_few()
ggsave(filename=here("analysis","plots","crit_choice_by_category.jpeg"),width=5,height=7)

