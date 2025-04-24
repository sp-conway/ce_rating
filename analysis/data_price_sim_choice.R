rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
library(latex2exp)
library(mvtnorm)

# settings
N <- 1e6 # n samples

# load price data first ============================================================
d <- here("data","clean","price.csv") %>%
  read_csv() %>%
  filter(effect!="catch") %>%
  mutate(effect=str_remove(effect,"_1|_2")) %>%
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

# figure out "choices" ============================================================
dd <- d %>%
  rowwise() %>%
  mutate(choice1=which.max(c(price_t,price_c,price_d)),
         choice=case_when(
           choice1==1~"t",
           choice1==2~"c",
           choice1==3~"d"
         )) %>%
  ungroup() %>%
  select(-choice1)

dd %>%
  group_by(effect,choice) %>%
  summarise(n=n()) %>%
  group_by(effect) %>%
  mutate(prop=n/sum(n)) %>%
  ggplot(aes(effect,prop,fill=choice))+
  geom_col(position="dodge")+
  ggthemes::theme_few()

# load choice data and get props ================================================================================================
choice <- here("data","clean","choice.csv") %>%
  read_csv() %>%
  filter(effect!="catch") %>%
  mutate(effect=str_remove(effect,"_1|_2"))
data_props <- choice %>%
  group_by(effect,choice) %>%
  summarise(N=n()) %>%
  group_by(effect) %>%
  mutate(p=N/sum(N)) %>%
  ungroup() %>%
  select(-N) %>%
  mutate(source="data")


# get mean pars from data ============================================================
z <- d %>%
  pivot_longer(c(price_t,price_c,price_d),names_to = "option",values_to = "price") %>%
  mutate(option=str_remove(option,"price_")) %>%
  group_by(participant) %>%
  mutate(mean=mean(price),
         s=sd(price)) %>%
  ungroup() %>%
  mutate(price=(price-mean)/s) 
m <- z %>%
  group_by(option,effect) %>%
  summarise(m=mean(price)) %>%
  ungroup() %>%
  pivot_wider(names_from = option, values_from = m) %>%
  relocate(t,.after=effect)
s <- rep(sd(z$price),3)

# sim model ==================================================================
mu_attraction <- m %>%
  filter(effect=="attraction") %>%
  select(t,c,d) %>%
  as.vector() %>%
  unlist()
mu_repulsion <- m %>%
  filter(effect=="repulsion") %>%
  select(t,c,d) %>%
  as.vector() %>%
  unlist()
cor_attraction <- z %>%
  filter(effect=="attraction") %>%
  pivot_wider(names_from = option, values_from = price) %>%
  select(t,c,d) %>%
  cor()
cor_repulsion <- z %>%
  filter(effect=="repulsion") %>%
  pivot_wider(names_from = option, values_from = price) %>%
  select(t,c,d) %>%
  cor()

a <- ( s %*% t(s) )
cv_attraction <- cor_attraction*a
cv_repulsion <- cor_repulsion*a

sim_model <- function(N, mu, sigma, effect){
  print(effect)
  X <- rmvnorm(N, mu, sigma)
  Y <- apply(X,1,which.max)
  p <- c(sum(Y==1),
         sum(Y==2),
         sum(Y==3))/N
  pd <- tibble(choice=c("t","c","d"),
               p=p,
               effect=effect,
               source="model")
  return(pd)
}

model_props <- pmap(
  list(
    list(N,N),
    list(mu_attraction,mu_repulsion),
    list(cv_attraction,cv_repulsion),
    list("attraction","repulsion")
  ),
  sim_model) %>%
  list_rbind()
# load banerjee data ============================================================
banerjee <- here("analysis/banerjee_2024_data/e1_props.csv") %>%
  read_csv()
banerjee_corr <- here("analysis/banerjee_2024_data/e1_props_corrected.csv") %>%
  read_csv()

all_props <- bind_rows(data_props,model_props)
ggplot()+
  geom_point(data=all_props,aes(choice,p,shape=source),alpha=.9,size=3,col="red")+
  labs(y="choice proportion")+
  scale_shape_manual(values=c(1,4),name="")+
  facet_grid(.~effect)+
  ggthemes::theme_few()+
  theme(text=element_text(size=14))
ggsave(filename=here("analysis","plots","choice_sim_preds_pars_from_data.jpeg"),width=5,height=4)
# 
all_props_banerjee_corr <- model_props %>%
  filter(effect=="repulsion") %>%
  bind_rows(
    filter(banerjee_corr,source=="corrected") %>% mutate(choice=tolower(Choice), source="banerjee (adjusted)") %>% rename(p=prop),
    filter(banerjee,Set=="ternary") %>% mutate(source="banerjee",choice=tolower(Choice),p=prop)
  ) %>%
  select(choice,p,source)
ggplot()+
  geom_point(data=all_props_banerjee_corr,aes(choice,p,shape=source),alpha=.3,size=3,col="red")+
  labs(y="choice proportion")+
  scale_shape_manual(values=c(1,2,3),name="")+
  ggthemes::theme_few()+
  theme(text=element_text(size=14))
# ggsave(filename=path(results_dir,"choice_sim_preds2.jpeg"),width=5,height=4)

all_props_banerjee_corr %>%
  pivot_wider(names_from = choice, values_from = p) %>%
  select(-d) %>%
  mutate(diff=c-t) %>%
  select(-c(t,c)) %>%
  pivot_wider(names_from = source, 
              values_from = diff) 
.310-.142
.168-.1
