# code for plotting stimulus values
# separate figure for dissertation

rm(list=ls())
library(readxl)
library(tidyverse)
library(here)
s <- here("design","stim_crit.xlsx") %>%
  read_excel()

s$t_high <- factor(s$t_high, levels=c(1,2),
                   labels=c("target high: 1","target high: 2"))
s %>%
  ggplot(aes(d1,d2,col=option))+
  # geom_text(aes(label=option))+
  geom_point(size=.8,alpha=.75)+
  facet_grid(effect~t_high)+
  coord_fixed(xlim=c(0,120),ylim=c(0,120))+
  ggsci::scale_color_startrek()+
  ggthemes::theme_few()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=15),
        legend.position = "top")
ggsave(filename=here("design","stim.jpeg"),width=6,height=6,dpi=600)

# replotting for dissertaiton
s %>%
  mutate(effect=str_replace_all(effect,c("_1"="\ndecoy near","_2"="\ndecoy far"))) %>%
  ggplot(aes(d1,d2,col=option))+
  # geom_text(aes(label=option))+
  geom_point(size=.8,alpha=.75)+
  facet_grid(effect~t_high)+
  coord_fixed(xlim=c(0,120),ylim=c(0,120))+
  ggsci::scale_color_startrek()+
  labs(x="dimension 1",y="dimension 2")+
  ggthemes::theme_few()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=9),
        legend.position = "top")
ggsave(filename=here("design","ce_rating_stim_for_paper.jpeg"),width=5,height=4,dpi=600)
