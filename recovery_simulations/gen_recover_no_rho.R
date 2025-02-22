rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
library(rstan)
library(mvtnorm)
library(bayesplot)

# Stan file
f <- here("recovery_simulations","model_no_rho.stan")

# sampler params
n_core <- 4
n_iter <- 2000

# FIXED EXP PARAMS
n_cat <- 8
n_per_effect <- 2
n_effect <- 2
n_trial_per_ppt <- n_cat*n_per_effect*n_effect

# Repulsion effect correlations
r_tc_rep <- r_cd_rep <- .5
r_td_rep <- .6
cors_rep <- matrix(c(1,r_tc_rep,r_td_rep,r_tc_rep,1,r_cd_rep,r_td_rep,r_cd_rep,1),nrow=3,ncol=3,byrow=T)

# attraction effect correlations
r_tc_att <- r_cd_att <- .6
r_td_att <- .5
cors_att <- matrix(c(1,r_tc_att,r_td_att,r_tc_att,1,r_cd_att,r_td_att,r_cd_att,1),nrow=3,ncol=3,byrow=T)

# s and mu
# just estimating freely in the model
s <- c(1,1,1) 
mu <- c(1,1,.8)

# Get variance-covariance matrix
get_cv <- function(s,cors) cors * (s %*% t(s) )
cv_rep <- get_cv(s, cors_rep)
cv_att <- get_cv(s, cors_att)

# Varying sample size
n_ppt <- seq(100, 500, 100)

for(samp_size in n_ppt){
  print(samp_size)
  
  # fit directory and file name
  dir_name <- here("recovery_simulations",glue("N_{samp_size}_sim_no_rho"))
  dir_create(dir_name)
  fit_name <- path(dir_name,"fit.RData")
  
  # only do simulation if haven't done so yet for this sample size
  if(!file_exists(fit_name)){
    # figure out total N
    N <- n_trial_per_ppt*samp_size
    
    # simulate repulsion trials
    X_rep <- rmvnorm(N/2, mean = mu, sigma = cv_rep)
    
    # simulate attraction trials
    X_att <- rmvnorm(N/2, mean = mu, sigma = cv_att)
    
    # combine simulated values
    X <- rbind(X_rep,X_att)
    
    # index of trial type (1=repulsion, 2=attraction)
    set <- c(rep(1,N/2),rep(2,N/2))
    
    # compile stan model 
    model <- stan_model(f) # might not actually need to do this every time but just a failsafe
    
    # all data for stan
    stan_data <- list(X=X, set=set, J=n_effect, N=N)
    
    # fit model
    fit <- sampling(model, data=stan_data, chains=n_core, iter=n_iter, cores=n_core)
    
    # save results (also generated data!)
    save(fit, stan_data, samp_size, file=fit_name)
    
    fit_summary <- summary(fit, probs=c(.025, .975))
    save(fit_summary, file=path(dir_name,"fit_summary.RData"))
    
    # plot posterior distributions
    post <- as.array(fit)
    color_scheme_set("red")
    try({
      p <- mcmc_trace(post, regex_pars = "mu")
      p
      ggsave(path(dir_name, "mu_trace.jpeg"), width=5,height=5)
      rm(p)
    })
    try({
      p <- mcmc_trace(post, regex_pars = "omega\\[1")
      p
      ggsave(path(dir_name, "omega_rep_trace.jpeg"), width=5,height=5)
      rm(p)
    })
    try({
      p <- mcmc_trace(post, regex_pars = "omega\\[2")
      p
      ggsave(path(dir_name, "omega_att_trace.jpeg"), width=5,height=5)
      rm(p)
    })
    try({
      p <- mcmc_hist(post, regex_pars = "mu")
      p
      ggsave(path(dir_name, "mu_hist.jpeg"), width=5,height=5)
      rm(p)
    })
    try({
      p <- mcmc_hist(post, regex_pars = "omega\\[1")
      p
      ggsave(path(dir_name, "omega_rep_hist.jpeg"), width=5,height=5)
      rm(p)
    })
    try({
      p <- mcmc_hist(post, regex_pars = "omega\\[2")
      p
      ggsave(path(dir_name, "omega_att_hist.jpeg"), width=5,height=5)
      rm(p)
    })
    
    
    try({
      p <- mcmc_dens(post, regex_pars = "mu")
      p
      ggsave(path(dir_name, "mu_dens.jpeg"), width=5,height=5)
      rm(p)
    })
    try({
      p <- mcmc_dens(post, regex_pars = "omega\\[1")
      p
      ggsave(path(dir_name, "omega_rep_dens.jpeg"), width=5,height=5)
      rm(p)
    })
    try({
      p <- mcmc_dens(post, regex_pars = "omega\\[2")
      p
      ggsave(path(dir_name, "omega_att_dens.jpeg"), width=5,height=5)
      rm(p)
    })
    try({
      p <- mcmc_dens_chains(post, regex_pars = "mu")
      p
      ggsave(path(dir_name, "mu_dens_chains.jpeg"), width=5,height=5)
      rm(p)
    })
    try({
      p <- mcmc_dens_chains(post, regex_pars = "omega\\[1")
      p
      ggsave(path(dir_name, "omega_rep_dens_chains.jpeg"), width=5,height=5)
      rm(p)
    })
    try({
      p <- mcmc_dens_chains(post, regex_pars = "omega\\[2")
      p
      ggsave(path(dir_name, "omega_att_dens_chains.jpeg"), width=5,height=5)
      rm(p)
    })
    
    
    try(rm(fit))
    try(rm(stan_data))
    try(rm(N))
    try(rm(model))
    try(rm(fit_summary))
    try(gc())
  }
}