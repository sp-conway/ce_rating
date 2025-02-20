library(here)
library(tidyverse)
library(fs)
rm(list=ls())

d <- here("banerjee_2024_data/RE data cleaned.csv") %>%
  read_csv() %>%
  filter(Experiment==5 & NumAlt==3)
d_unique <- d %>%
  mutate(Extremity=as.numeric(recode(HighDim,
                          "70-60"=1,
                          "80-70"=2,
                          "90-80"=3,
                          "100-90"=4))) %>%
  separate(LowDim, into=c("T_Low","C_Low"), convert = T) %>%
  separate(HighDim, into=c("T_High","C_High"), convert=T) %>%
  mutate(D_High=T_High, 
         D_Low=25) %>%
  group_by(Participant) %>%
  mutate(trial=1:n()) %>%
  ungroup() %>%
  pivot_longer(contains("_")) %>%
  separate(name, into = c("Option", "Dim")) %>%
  pivot_wider(names_from = Dim, values_from =  value) %>%
  distinct(HighValueColumn, Product, Order, Option, Low, High, Extremity) 
prod_cat <- unique(d_unique$Product)
prod_cat

stim <- d_unique %>% 
  mutate(x=if_else(HighValueColumn==1, High, Low), 
         y=if_else(HighValueColumn==1, Low,  High)) %>%
  mutate(t_high=recode(HighValueColumn,`1`="x",`2`="y")) %>%
  select(-c(Low,High)) %>%
  distinct(t_high, Option, x, y, Extremity) %>%
  rename(option=Option, extreme=Extremity) %>%
  bind_rows(
    tibble(
      t_high=c("x","x","x","y","y","y"),
      option=c("T","C","D","T","C","D"),
      x=c(60,50,60,50,60,45),
      y=c(50,60,45,60,50,60),
      extreme=0
    )
  ) %>%
  mutate(extreme=as.factor(extreme))
stim

stim %>%
  ggplot(aes(x,y,col=option))+
  geom_text(aes(label=option),size=2,alpha=.85)+
  ggsci::scale_color_cosmic()+
  facet_grid(t_high~extreme,labeller=label_both)+
  coord_fixed(xlim=c(0,120),ylim=c(0,120))+
  ggthemes::theme_few()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=10),legend.position = "none")
ggsave(filename=here("design/stim.jpeg"),
       width=5,height=4,dpi=700)
write_csv(stim, file=here("design/stim_crit.csv"))
