rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
library(cmdstanr)
library(bayesplot)
library(latex2exp)

# settings
set_cmdstan_path(here("cmdstan-2.36.0"))
which_model <- "bayes_price_1"
model_file <- here("analysis","bayes_price","stan",glue("{which_model}.stan"))
results_dir <- here("analysis","bayes_price",which_model)
dir_create(results_dir)

# sampler settings
n_iter <- 1000
n_chain <- 4

# read in data and figure out target ========================================================
# also z score within ppt
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
  select(-c(price_1,price_2,price_3)) %>%
  pivot_longer(contains("price"),names_to="opt",values_to="price") %>%
  group_by(participant) %>%
  mutate(m=mean(price),
         s=sd(price)) %>%
  ungroup() %>%
  mutate(price=(price-m)/s) %>%
  pivot_wider(names_from=opt, values_from=price) %>%
  mutate(keep_t=abs(price_t)<=3,
         keep_c=abs(price_c)<=3,
         keep_d=abs(price_d)<=3) %>%
  filter(keep_t & keep_c & keep_d) %>%
  select(-c(m,s,keep_t,keep_c,keep_d)) %>%
  mutate(effect=str_remove(effect,"_1|_2")) %>%
  arrange(participant, effect, category) 

N_att <- d %>%
  filter(effect=="attraction") %>%
  count(category) %>%
  pull(n)
N_rep <- d %>%
  filter(effect=="repulsion") %>%
  count(category) %>%
  pull(n)
laptops_att <- as.matrix(filter(d,effect=="attraction" & category=="laptops") %>% select(price_t,price_c,price_d))
microwaves_att <- as.matrix(filter(d,effect=="attraction" & category=="microwave ovens") %>% select(price_t,price_c,price_d))
tv_att <- as.matrix(filter(d,effect=="attraction" & category=="televisions") %>% select(price_t,price_c,price_d))
washers_att <- as.matrix(filter(d,effect=="attraction" & category=="washing machines") %>% select(price_t,price_c,price_d))

laptops_rep <- as.matrix(filter(d,effect=="repulsion" & category=="laptops") %>% select(price_t,price_c,price_d))
microwaves_rep <- as.matrix(filter(d,effect=="repulsion" & category=="microwave ovens") %>% select(price_t,price_c,price_d))
tv_rep <- as.matrix(filter(d,effect=="repulsion" & category=="televisions") %>% select(price_t,price_c,price_d))
washers_rep <- as.matrix(filter(d,effect=="repulsion" & category=="washing machines") %>% select(price_t,price_c,price_d))

stan_data <- list(N_att=N_att,
                  N_rep=N_rep,
                  laptops_att=laptops_att,
                  microwaves_att=microwaves_att,
                  tv_att=tv_att,
                  washers_att=washers_att,
                  laptops_rep=laptops_rep,
                  microwaves_rep=microwaves_rep,
                  tv_rep=tv_rep,
                  washers_rep=washers_rep)
# sample from posterior ==================================================================
m <- cmdstan_model(model_file)
fit <- m$sample(data=stan_data,
                iter_sampling=n_iter,
                chains=n_chain,
                parallel_chains=n_chain,
                output_dir=results_dir,
                output_basename=which_model)
fit_summary <- fit$summary(
  variables=NULL,
  posterior::default_summary_measures(),
  extra_quantiles=~posterior::quantile2(., probs=c(.0275, .975))
)
save(fit_summary, file=path(results_dir,"fit_summary.RData"))
sampler_diagnostics <- fit$sampler_diagnostics()
rhat(fit)

# analyze model ========================================================================
color_scheme_set("red")
mcmc_trace(fit$draws(variables="lp__"))
ggsave(filename=path(results_dir,"lp__trace.jpeg"),width=4,height=4)
mcmc_trace(fit$draws(variables="mu_attraction"))
ggsave(filename=path(results_dir,"mu_attraction_trace.jpeg"),width=7,height=6)
mcmc_trace(fit$draws(variables="mu_repulsion"))
ggsave(filename=path(results_dir,"mu_repulsion_trace.jpeg"),width=7,height=6)
mcmc_trace(fit$draws(variables=c("s")))
ggsave(filename=path(results_dir,"s_trace.jpeg"),width=4,height=4)
mcmc_trace(fit$draws(variables=c("cor_attraction[1,2]","cor_attraction[1,3]","cor_attraction[2,3]")))
ggsave(filename=path(results_dir,"cor_attraction_trace.jpeg"),width=7,height=6)
mcmc_trace(fit$draws(variables=c("cor_repulsion[1,2]","cor_repulsion[1,3]","cor_repulsion[2,3]")))
ggsave(filename=path(results_dir,"cor_repulsion_trace.jpeg"),width=7,height=6)

mcmc_hist(fit$draws(variables=c("cor_repulsion[1,2]","cor_repulsion[1,3]","cor_repulsion[2,3]")))
ggsave(filename=path(results_dir,"cor_repulsion_hist.jpeg"),width=5,height=5)

mcmc_hist(fit$draws(variables=c("cor_attraction[1,2]","cor_attraction[1,3]","cor_attraction[2,3]")))
ggsave(filename=path(results_dir,"cor_attraction_hist.jpeg"),width=5,height=5)

# analyze mu =================================================================================
mu_attraction <- fit$draws(variables="mu_attraction",format="df")
mu_repulsion <- fit$draws(variables="mu_repulsion",format="df")

summarise_mu <- function(mu,effect){
  mu %>%
    pivot_longer(contains("mu")) %>%
    mutate(option=case_when(
      str_detect(name,"1\\]")~"t",
      str_detect(name,"2\\]")~"c",
      str_detect(name,"3\\]")~"d")) %>%
    group_by(.chain,.iteration,option) %>%
    summarise(mean=mean(value)) %>%
    ungroup() %>%
    group_by(option) %>%
    summarise(m=mean(mean),
              hdi_lower=HDInterval::hdi(mean)[1],
              hdi_upper=HDInterval::hdi(mean)[2]
    ) %>%
    mutate(effect=effect)
}
mu_model <- map2(list(mu_attraction,mu_repulsion),
                 list("attraction","repulsion"),
                 summarise_mu) %>%
  list_rbind() %>%
  mutate(source="model")
mu_data <- d %>%
  pivot_longer(contains("price"),names_to="option",values_to="price") %>%
  mutate(option=str_remove(option,"price_")) %>%
  group_by(effect,option) %>%
  summarise(m=mean(price)) %>%
  ungroup() %>%
  mutate(source="data")
mu_data_model <- bind_rows(mu_model, mu_data)

mu_data_model %>%
  ggplot(aes(option,m,shape=source))+
  geom_point(alpha=.5)+
  geom_errorbar(aes(ymin=hdi_lower,ymax=hdi_upper),width=.1,alpha=.2)+
  scale_shape_manual(values=c(1,4))+
  labs(x="stimulus",y="mean price (z units)")+
  facet_grid(effect~.)+
  ggthemes::theme_few()
ggsave(filename=path(results_dir,"mu_model_data.jpeg"),width=4,height=5)
save(mu_data_model, file=path(results_dir,"mu_data_model.RData"))
fit_summary %>%
  filter(str_detect(variable, "cor")) %>%
  filter(str_detect(variable, "1,2|1,3|2,3")) %>%
  select(variable, mean, q2.75, q97.5) %>%
  rename(lower=q2.75,
         upper=q97.5) %>%
  mutate(
    param_label=case_when(
      str_detect(variable, "1,2") ~ "$\\rho_{tc}$",
      str_detect(variable, "1,3") ~ "$\\rho_{td}$",
      str_detect(variable, "2,3") ~ "$\\rho_{cd}$"
    ),
    cond=str_extract(variable, "attraction|repulsion")
  ) %>%
  ggplot(aes(x=param_label, y=mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
  scale_y_continuous(limits=c(0.65, 0.9)) +
  scale_x_discrete(labels=TeX) + 
  labs(x="parameter", y="estimate") +
  coord_flip() +
  facet_grid(cond ~ .) +
  ggthemes::theme_few() +
  theme(text=element_text(size=15))
ggsave(filename=path(results_dir,"omega_posteriors.jpeg"),width=5,height=4)
