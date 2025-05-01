rm(list=ls())
library(tidyverse)
library(here)
library(fs)

dat <- here("data_choice_only/clean/choice.csv") %>%
  read_csv() %>%
  filter(effect!="catch")
bin <- dat %>%
  filter(set=="binary")
tri <- dat %>%
  filter(set=="trinary")
# binary analysis - repulsion trials ==========================================================================================
bin_rep <- bin %>%
  filter(effect=="repulsion")
bin_rep_mprops <- bin_rep %>%
  group_by(participant,choice) %>%
  summarise(N=n()) %>%
  ungroup() %>%
  group_by(participant) %>%
  mutate(prop=N/sum(N)) %>% 
  ungroup() %>%
  group_by(choice) %>%
  summarise(m=mean(prop),
            se=sd(prop)/sqrt(n()),
            l=m-se,
            u=m+se) %>%
  ungroup()
bin_rep_mprops %>%
  ggplot(aes(choice,m))+
  geom_col(position="dodge",fill="lightblue",width=.5)+
  geom_errorbar(aes(ymin=l,ymax=u),position=position_dodge(width=.5),width=.01)+
  labs(y="mean choice prop.",title="repulsion binary")+
  ggthemes::theme_few()+
  theme(text=element_text(size=16))
ggsave(filename = here("analysis_choice_only_experiment/plots/bin_rep_mean_props.jpeg"),width=5,height=4)

# binary analysis - attraction trials ====================================================================================================
bin_att <- bin %>%
  filter(effect=="attraction")
bin_att_mprops <- bin_att %>%
  group_by(participant,choice) %>%
  summarise(N=n()) %>%
  ungroup() %>%
  group_by(participant) %>%
  mutate(prop=N/sum(N)) %>% 
  ungroup() %>%
  group_by(choice) %>%
  summarise(m=mean(prop),
            se=sd(prop)/sqrt(n()),
            l=m-se,
            u=m+se) %>%
  ungroup()
bin_att_mprops %>%
  ggplot(aes(choice,m))+
  geom_col(position="dodge",fill="lightblue",width=.5)+
  geom_errorbar(aes(ymin=l,ymax=u),position=position_dodge(width=.5),width=.01)+
  labs(y="mean choice prop.",title="attraction binary")+
  ggthemes::theme_few()+
  theme(text=element_text(size=16))
ggsave(filename = here("analysis_choice_only_experiment/plots/bin_att_mean_props.jpeg"),width=5,height=4)

bin_att_props_by_cat <- bin_att %>%
  group_by(choice,category) %>%
  summarise(N=n()) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(prop=N/sum(N)) %>% 
  ungroup() 
bin_att_props_by_cat %>%
  ggplot(aes(choice,prop))+
  geom_col(position="dodge",fill="lightblue",width=.5)+
  labs(y="agg. choice prop.",title="attraction binary")+
  facet_grid(category~.)+
  ggthemes::theme_few()+
  theme(text=element_text(size=16))
ggsave(filename = here("analysis_choice_only_experiment/plots/att_agg_props.jpeg"),width=5,height=4)

# trinary analysis =========================================================================================================
tri_props <- tri %>%
  group_by(effect,choice) %>%
  summarise(N=n()) %>%
  group_by(effect) %>%
  mutate(prop=N/sum(N)) %>%
  ungroup()
tri_props %>%
  ggplot(aes(choice,prop))+
  geom_col(position="dodge",fill="lightblue",width=.5)+
  labs(y="agg. choice prop.",title="trinary")+
  facet_grid(effect~.)+
  ggthemes::theme_few()+
  theme(text=element_text(size=16))
