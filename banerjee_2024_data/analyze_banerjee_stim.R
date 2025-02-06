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

d_unique1 <- d_unique %>% 
  mutate(X=if_else(HighValueColumn==1, High, Low), 
         Y=if_else(HighValueColumn==1, Low,  High)) %>%
  mutate(T_Dim_High=recode(HighValueColumn,`1`="X",`2`="Y")) %>%
  select(-c(Low,High)) %>%
  distinct(T_Dim_High, Option, X, Y, Extremity) %>%
  bind_rows(
    tibble(
      T_Dim_High=c("X","X","X","Y","Y","Y"),
      Option=c("T","C","D","T","C","D"),
      X=c(60,50,60,50,60,45),
      Y=c(50,60,45,60,50,60),
      Extremity=0
    )
  ) %>%
  mutate(Extremity=as.factor(Extremity))
d_unique1


d_unique1 %>%
  ggplot(aes(X,Y,col=Option))+
  geom_text(aes(label=Option),size=2.5,alpha=.85)+
  ggsci::scale_color_cosmic()+
  facet_grid(T_Dim_High~Extremity,labeller=label_both)+
  coord_fixed(xlim=c(0,120),ylim=c(0,120))+
  ggthemes::theme_few()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=10),legend.position = "none")
ggsave(filename=here("banerjee_2024_data/plots/banerjee_stim.jpeg"),
       width=6,height=5,dpi=700)
