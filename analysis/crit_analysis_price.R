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
  mutate(category=str_replace(category," ","\n"),
         effect=str_replace_all(effect,c("_1"=" \ndecoy near","_2"=" \ndecoy far"))) %>%
  ggplot(aes(category,m,fill=option))+
  geom_col(position = "dodge",width=.6)+
  geom_hline(yintercept=0,alpha=.6)+
  geom_errorbar(aes(ymin=se_lower,ymax=se_upper),position = position_dodge(width=.6),width=.2)+
  labs(x="product category",y="mean price")+
  facet_grid(effect~.)+
  ggsci::scale_fill_startrek()+
  ggthemes::theme_few()+
  theme(text=element_text(size = 10))
ggsave(filename=here("analysis","plots","price_m_by_effect_category.jpeg"),width=4,height=5)

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


# scatterplot for thesis
upper_fcn=function(x,y,...) {
  points(x,y,...)
  abline(a=0,b=1,col='gray',lty=2)
}  

lower_fcn=function(x,y,...) {
  r <- cor(x,y)
  txt <- paste0('r=',format(c(r, 0.123456789), digits = 2)[1])
  text(0,0,txt,cex=1.5)
}

for(eff in c("attraction","repulsion")){
  tmp <- filter(price_wide_z_FILTERED, str_detect(effect,eff)) %>%
    select(t,c,d)
  jpeg(filename = here(glue("analysis/plots/price_z_corplot_{eff}.jpeg")),width=6,height=5,units="in",res=500)
  par(pty="s") 
  pairs(tmp, lower.panel = lower_fcn, upper.panel=upper_fcn, pch=".",
        gap=0, row1attop=FALSE, main=eff)
  dev.off()
}

