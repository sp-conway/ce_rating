# dirichlet-multinomial model for dissertation
# setup =========================================================================================================
rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
library(rstan)
library(bayesplot)

# WHICH MODEL
which_model <- "bayes_choice_dm_hier_1" 

# file directory stuff
stan_file <- here("analysis","bayes_choice","stan",glue("{which_model}.stan"))
results_dir <- here(glue("analysis/bayes_choice/{which_model}"))
dir_create(results_dir)

# Model settings ========================================================================================
n_iter <- 2000
n_core <- n_chain <- 4

# DATA SETUP ========================================================================================
d <- here("data","clean","choice.csv") %>%
  read_csv() %>%
  filter(effect!="catch") %>%
  separate(effect,into=c("effect","distance"),sep="_") %>%
  select(participant,effect,distance,category,t_high,choice)

counts1 <- d %>%
  count(participant,effect,distance,category,t_high,choice) %>%
  pivot_wider(names_from = choice,
              values_from = n, 
              values_fill = 0)
# RENUMBER PARTICIPANT TO BE SEQUENTIAL ================================================
d_counts <- counts1 %>%
  distinct(participant) %>%
  mutate(participant_new=1:n()) %>%
  right_join(counts1)

# GET DATA READY FOR STAN =====================================================================================
counts_matrix <- as.matrix(d_counts[,c("t","c","d")])

# P - n participant
participant_new <- d_counts$participant_new
P <- length(unique(participant_new))

# E - attraction vs repulsion
effect <- as.numeric(str_replace_all(d_counts$effect,c("attraction"="1","repulsion"="2")))
E <- length(unique(effect))

# D - TDD
distance <- as.numeric(d_counts$distance)
D <- length(unique(distance))

# C - product category
category <- as.numeric(str_replace_all(d_counts$category,
                                       c("laptops"="1",
                                         "microwave ovens"="2",
                                         "televisions"="3",
                                         "washing machines"="4")))
C <- length(unique(category))

# H - target higher dimension
t_high <- d_counts$t_high
H <- length(unique(t_high))

# O <- N options
O <- 3
counts <- array(NA_integer_,dim=c(P,E,D,C,H,O))
for(p in 1:P){
  print(p)
  for(e in 1:E){
    for(d in 1:D){
      for(c in 1:C){
        for(h in 1:H){
          counts[p,e,d,c,h,] <- counts_matrix[participant_new==p & effect==e & distance==d & category==c & t_high==h,]
        }
      }
    }
  }
}

stan_data <- list(P=P,
                  E=E,
                  D=D,
                  C=C,
                  H=H,
                  O=O,
                  counts=counts)

# LOAD AND COMPILE MODEL ======================================================================
m <- stan_model(stan_file)
f <- m$sample(data=stan_data,
              parallel_chains=n_core,
              iter_sampling=n_iter,
              chains=n_chain,
              output_dir = results_dir)
draws <- f$draws()
summary <- f$summary()

color_scheme_set("red")
mcmc_trace(draws,pars = "lp__")
ggsave(filename=path(results_dir,"lp_trace.jpeg"),width=5,height=5)

mcmc_trace(draws,regex_pars="theta_m")
ggsave(filename=path(results_dir,"theta_m_trace.jpeg"),width=5,height=5)

theta_m_summary <- f$summary(variables = "theta_m",
                            m=~mean(.),
                            quantiles = ~ quantile(., probs = c(0.025, 0.975))) %>% 
  mutate(effect=case_when(
    str_detect(variable,"m\\[1")~"attraction",
    str_detect(variable,"m\\[2")~"repulsion"
  ),
  distance=case_when(
    str_detect(variable,",1,")~"near",
    str_detect(variable,",2,")~"far"
  ),
  choice=case_when(
    str_detect(variable,"1\\]")~"target",
    str_detect(variable,"2\\]")~"competitor",
    str_detect(variable,"3\\]")~"decoy"
  ))  %>%
  select(-variable) %>% 
  rename(lower=`2.5%`,
         upper=`97.5%`) %>%
  mutate(source="model")
save(theta_m_summary, file=path(results_dir,"theta_m_summary.RData"))

data_to_model <- d_counts %>%
  mutate(distance=str_replace_all(distance,c("1"="near","2"="far"))) %>%
  rename(target=t,
         competitor=c,
         decoy=d) %>%
  pivot_longer(c(target,decoy,competitor),
               names_to = "choice",
               values_to = "N") %>%
  group_by(effect,distance,choice) %>%
  summarise(N=sum(N)) %>%
  group_by(effect,distance) %>%
  mutate(m=N/sum(N)) %>%
  ungroup() %>%
  select(-N) %>%
  mutate(source="data") %>%
  bind_rows(theta_m_summary) %>%
  mutate(effect=str_c(effect,"\n decoy ",distance))

data_to_model %>%
  ggplot(aes(effect, m, col=choice, shape=source))+
  geom_point(alpha=.7,size=1.5)+
  geom_errorbar(aes(ymin=lower,ymax=upper),width=.01,alpha=.7)+
  ggsci::scale_color_startrek()+
  scale_shape_manual(values=c(1,4))+
  scale_y_continuous(limits=c(0,.8,breaks=seq(0,.8,.2)))+
  labs(x="trial type",y="aggregate choice proportion")+
  ggthemes::theme_few()
ggsave(filename=glue("{results_dir}/bayes_choice_model_data_plot.jpeg"),width=5,height=4)
