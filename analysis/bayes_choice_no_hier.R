# dirichlet-multinomial model for dissertation
# setup =========================================================================================================
rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
library(cmdstanr)
library(bayesplot)

# cmdstan path
set_cmdstan_path(here("cmdstan-2.36.0/"))

# WHICH MODEL
which_model <- "bayes_choice_dm_no_hier_1" 

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

d_counts <- d %>%
  count(effect,distance,category,t_high,choice) %>%
  pivot_wider(names_from = choice,
              values_from = n, 
              values_fill = 0)
counts_matrix <- as.matrix(d_counts[,c("t","c","d")])
effect <- as.numeric(str_replace_all(d_counts$effect,c("attraction"="1","repulsion"="2")))
E <- length(unique(effect))

distance <- as.numeric(d_counts$distance)
D <- length(unique(distance))

category <- as.numeric(str_replace_all(d_counts$category,
                                       c("laptops"="1",
                                         "microwave ovens"="2",
                                         "televisions"="3",
                                         "washing machines"="4")))
C <- length(unique(category))
t_high <- d_counts$t_high
H <- length(unique(t_high))
K <- 3
counts <- array(NA_integer_,dim=c(E,D,C,H,K))
for(e in 1:E){
  for(d in 1:D){
    for(c in 1:C){
      for(h in 1:H){
        counts[e,d,c,h,] <- counts_matrix[effect==e & distance==d & category==c & t_high==h,]
      }
    }
  }
}

stan_data <- list(E=E,
                  D=D,
                  C=C,
                  H=H,
                  K=K,
                  counts=counts)

# LOAD AND COMPILE MODEL ======================================================================
m <- cmdstan_model(stan_file)
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
  labs(x="trial type",y="mean choice proportion")+
  ggthemes::theme_few()
ggsave(filename=glue("{results_dir}/model_data_plot.jpeg"),width=5,height=4)
