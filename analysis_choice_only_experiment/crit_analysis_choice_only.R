rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)

d <- here("data_choice_only/clean/choice.csv") %>%
  read_csv() %>%
  filter(effect!="catch")

# binary ==============================================================================
bi_prop <- d %>%
  filter(set=="binary") %>%
  group_by(effect, t_high, choice) %>%
  summarise(N=n()) %>%
  group_by(effect, t_high) %>%
  mutate(prop=N/sum(N)) %>%
  ungroup() %>%
  mutate(se=sqrt((prop*(1-prop))/n()),
         l=prop-se,
         u=prop+se)

bi_prop %>%
  mutate(effect=case_when(
    effect=="attraction"~effect,
    T~str_c(effect,"_", t_high))
  ) %>%
  ggplot(aes(effect,prop,fill=choice))+
  geom_col(position="dodge",width=.5)+
  geom_errorbar(aes(ymin=l,ymax=u),position=position_dodge(width=.5),width=.05)+
  scale_y_continuous(limits=c(0,1))+
  labs(y="aggregate choice prop.")+
  ggsci::scale_fill_startrek()+
  ggthemes::theme_few()+
  theme(text=element_text(size=16))
ggsave(filename=here("analysis_choice_only_experiment/plots/binary_props.jpeg"))

# trinary ==============================================================================
tri_prop <- d %>%
  filter(set=="trinary") %>%
  group_by(effect, t_high, choice) %>%
  summarise(N=n()) %>%
  group_by(effect, t_high) %>%
  mutate(prop=N/sum(N)) %>%
  ungroup() %>%
  mutate(se=sqrt((prop*(1-prop))/n()),
         l=prop-se,
         u=prop+se)

tri_prop %>%
  ggplot(aes(effect,prop,fill=choice))+
  geom_col(position="dodge",width=.5)+
  geom_errorbar(aes(ymin=l,ymax=u),position=position_dodge(width=.5),width=.05)+
  facet_grid(t_high~.,labeller=label_both)+
  ggsci::scale_fill_startrek()+
  ggthemes::theme_few()

tri_prop_collapsed <- d %>%
  filter(set=="trinary") %>%
  group_by(effect, choice) %>%
  summarise(N=n()) %>%
  group_by(effect) %>%
  mutate(prop=N/sum(N)) %>%
  ungroup() %>%
  mutate(se=sqrt((prop*(1-prop))/n()),
         l=prop-se,
         u=prop+se)

tri_prop_collapsed %>%
  ggplot(aes(effect,prop,fill=choice))+
  geom_col(position="dodge",width=.5)+
  geom_errorbar(aes(ymin=l,ymax=u),position=position_dodge(width=.5),width=.05)+
  labs(y="aggregate choice prop.")+
  ggsci::scale_fill_startrek()+
  ggthemes::theme_few()+
  theme(text=element_text(size=16))
ggsave(filename=here("analysis_choice_only_experiment/plots/trinary_props.jpeg"))

