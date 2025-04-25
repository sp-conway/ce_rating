rm(list=ls())
library(readxl)
library(tidyverse)
library(patchwork)
library(here)
s <- here("design_choice_only","stim_crit.xlsx") %>%
  read_excel()


tri <- s %>%
  mutate(t_high=factor(t_high, levels=c(1,2),
                       labels=c("target high: 1","target high: 2"))) %>%
  filter(set=="trinary") %>%
  ggplot(aes(d1,d2,col=option))+
  # geom_text(aes(label=option))+
  geom_point(size=1.5,alpha=.75)+
  labs(title="trinary")+
  facet_grid(effect~t_high)+
  coord_fixed(xlim=c(0,120),ylim=c(0,120))+
  ggsci::scale_color_startrek()+
  ggthemes::theme_few()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top",
        plot.title = element_text(hjust=0.5))
tri

bi_rep <- s %>%
  filter(set=="binary" & effect=="repulsion") %>%
  mutate(t_high=factor(t_high, levels=c(1,2),
                       labels=c("target high: 1","target high: 2"))) %>%
  ggplot(aes(d1,d2,col=option))+
  # geom_text(aes(label=option))+
  geom_point(size=1.5,alpha=.75)+
  labs(title="binary repulsion")+
  facet_grid(.~t_high)+
  coord_fixed(xlim=c(0,120),ylim=c(0,120))+
  scale_color_manual(values=ggsci::pal_startrek()(3)[c(1,3)])+
  ggthemes::theme_few()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5))
bi_att <- s %>%
  filter(set=="binary" & effect=="attraction") %>%
  ggplot(aes(d1,d2,col=option))+
  # geom_text(aes(label=option))+
  geom_point(size=1.5,alpha=.75)+
  labs(title="binary attraction")+
  coord_fixed(xlim=c(0,120),ylim=c(0,120))+
  scale_color_manual(values=ggsci::pal_startrek()(3)[c(1,3)])+
  ggthemes::theme_few()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5))
bi <- (bi_rep/bi_att) + plot_layout(widths = c(2,1),guides = "collect") + plot_annotation(theme=theme(legend.position="top"))
(tri|bi)+
    plot_annotation(theme = theme(text=element_text(size=20)))
ggsave(filename=here("design_choice_only","stim.jpeg"),width=9,height=7,dpi=800)
