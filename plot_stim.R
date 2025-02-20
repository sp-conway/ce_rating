rm(list=ls())
library(readxl)
library(tidyverse)
library(here)
s <- here("design","stim_crit.xlsx") %>%
  read_excel()

s$t_high <- factor(s$t_high, levels=c("x","y"),
                   labels=c("target high: x","target high: y"))
s %>%
  ggplot(aes(x,y,col=option))+
  # geom_text(aes(label=option))+
  geom_point(size=.8,alpha=.75)+
  facet_grid(effect~t_high)+
  coord_fixed(xlim=c(0,120),ylim=c(0,120))+
  ggthemes::theme_few()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(size=10))
ggsave(filename=here("design","stim.jpeg"),width=4,height=6,dpi=600)
