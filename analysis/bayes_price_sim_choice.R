rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
library(latex2exp)
library(mvtnorm)

# settings
N <- 1e6 # n samples
which_model <- "bayes_price_1"
results_dir <- here("analysis","bayes_price",which_model)

# load data first ============================================================
d <- here("data","clean","choice.csv") %>%
  read_csv() %>%
  filter(effect!="catch") %>%
  mutate(effect=str_remove(effect,"_1|_2"))
data_props <- d %>%
  group_by(effect,choice) %>%
  summarise(N=n()) %>%
  group_by(effect) %>%
  mutate(p=N/sum(N)) %>%
  ungroup() %>%
  select(-N) %>%
  mutate(source="data")

# load modeling results ============================================================
load(path(results_dir,"fit_summary.RData"))
load(path(results_dir,"mu_data_model.RData"))

mu_attraction_df <- mu_data_model %>%
  filter(effect=="attraction" & source=="model") %>%
  select(option,m)
mu_attraction <- mu_attraction_df$m[c(3,1,2)] # r does c d t
ma <- mean(mu_attraction[1:2])
mu_attraction <- c(ma,ma,mu_attraction[3])

mu_repulsion_df <- mu_data_model %>%
  filter(effect=="repulsion" & source=="model") %>%
  select(option,m)
mu_repulsion <- mu_repulsion_df$m[c(3,1,2)] # r does c d t
mr <- mean(mu_repulsion[1:2])
mu_repulsion <- c(mr,mr,mu_repulsion[3])

cor_attraction_df <- fit_summary %>%
  filter(str_detect(variable,"cor_attraction")) 
cor_attraction <- matrix(cor_attraction_df$mean,nrow=3,ncol=3)

cor_repulsion_df <- fit_summary %>%
  filter(str_detect(variable,"cor_repulsion")) 
cor_repulsion <- matrix(cor_repulsion_df$mean,nrow=3,ncol=3)

s <- fit_summary %>%
  filter(str_detect(variable,"sigma\\["))  %>%
  pull(mean)

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

all_props <- bind_rows(data_props,model_props)
all_props %>%
  ggplot(aes(choice,p,col=choice,shape=source))+
  geom_point()+
  facet_grid(.~effect)+
  scale_shape_manual(values=c(1,4))+
  ggthemes::theme_few()

ggplot()+
  geom_point(data=all_props,aes(choice,p,shape=source),alpha=.9,size=3,col="red")+
  labs(y="choice proportion")+
  scale_shape_manual(values=c(1,4))+
  facet_grid(.~effect)+
  ggthemes::theme_few()+
  theme(text=element_text(size=14))
ggsave(filename=path(results_dir,"choice_sim_preds.jpeg"),width=5,height=4)
