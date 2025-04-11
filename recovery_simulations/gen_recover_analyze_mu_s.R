rm(list=ls())
library(fs)
library(here)
library(tidyverse)

inc_rho <- F # whether to use model with rho or not
dirs <- dir_ls(here("recovery_simulations"),
               type = "directory",
               regexp = ifelse(inc_rho, "sim$", "sim_no_rho"))

read_pars <- function(folder){
  load(path(folder,"fit_summary.RData"))
  fit_summary1 <- as.data.frame(fit_summary$summary)
  fit_summary1$param <- rownames(fit_summary1)
  fit_summary1$N <- as.numeric(str_extract(folder,"(?<=N_)[:digit:]{3}"))
  rownames(fit_summary1) <- NULL
  fit_summary2 <- fit_summary1 %>%
    filter(str_detect(param,"s\\[|mu")) %>%
    relocate(c(N,param),.before=everything())
  return(fit_summary2)
}

fits <- map(dirs, read_pars) %>%
  list_rbind()

fits %>%
  filter(str_detect(param,"s\\["))

fits %>%
  filter(str_detect(param,"mu\\["))
