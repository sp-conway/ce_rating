rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)
library(latex2exp)

# FIXED MODEL PARAMS 
rho_gen <- .5 # contribution of global correlations
cor_global_gen <- diag(3)
cor_global_gen[which(cor_global_gen==0)] <- .5

# Repulsion effect correlations
r_tc_rep <- r_cd_rep <- .5
r_td_rep <- .7
cors_rep <- matrix(c(1,r_tc_rep,r_td_rep,r_tc_rep,1,r_cd_rep,r_td_rep,r_cd_rep,1),nrow=3,ncol=3,byrow=T)
cors_rep_gen <- rho_gen*cor_global_gen + (1-rho_gen)*cors_rep

# attraction effect correlations
r_tc_att <- r_cd_att <- .7
r_td_att <- .5
cors_att <- matrix(c(1,r_tc_att,r_td_att,r_tc_att,1,r_cd_att,r_td_att,r_cd_att,1),nrow=3,ncol=3,byrow=T)
cors_att_gen <- rho_gen*cor_global_gen + (1-rho_gen)*cors_att

dirs <- dir_ls(here("recovery_simulations"),type = "directory",regexp = "sim$")

read_omega <- function(folder){
  load(path(folder,"fit_summary.RData"))
  fit_summary1 <- as.data.frame(fit_summary$summary)
  fit_summary1$param <- rownames(fit_summary1)
  fit_summary1$N <- as.numeric(str_extract(folder,"(?<=N_)[:digit:]{3}"))
  rownames(fit_summary1) <- NULL
  fit_summary2 <- fit_summary1 %>%
    filter(str_detect(param,"omega|rho|Omega")) %>%
    relocate(c(N,param),.before=everything())
  return(fit_summary2)
}

omega_post <- map(dirs, read_omega) %>%
  list_rbind()

# plot rho ================================================================================
rho_post <- omega_post %>%
  filter(str_detect(param,"rho")) %>%
  mutate(gen=rho_gen)

rho_post %>%
  rename(est=mean) %>%
  select(c(N,est,`2.5%`,`97.5%`,`gen`)) %>%
  pivot_longer(-c(N,`2.5%`,`97.5%`)) %>%
  mutate(`2.5%`=if_else(name=="est",`2.5%`,NA_real_),
         `97.5%`=if_else(name=="est",`97.5%`,NA_real_)) %>%
  ggplot(aes(N,value,col=name,shape=name))+
  geom_errorbar(aes(ymin=`2.5%`,ymax=`97.5%`))+
  geom_point()+
  ggsci::scale_color_d3(name="")+
  scale_shape_manual(values=c(1,4),name="")+
  scale_y_continuous(limits=c(.2,1),breaks=seq(.2,1,.1))+
  labs(y=TeX("mean estimated $\\rho$"))+
  ggthemes::theme_few()+
  theme(text=element_text(size=14))
ggsave(filename=here("recovery_simulations","plots","gen_recover_rho.jpeg"),width=7,height=5)

# plot omega local ================================================================================
string <- "omega_local\\[1,1,2\\]|omega_local\\[1,1,3\\]|omega_local\\[1,2,3\\]|omega_local\\[2,1,2\\]|omega_local\\[2,1,3\\]|omega_local\\[2,2,3\\]"
omega_local_post <- omega_post %>%
  filter(str_detect(param,string)) %>%
  mutate(cor=case_when(
    str_detect(param,"omega_local\\[1,1,2\\]")~"tc_rep",
    str_detect(param,"omega_local\\[1,1,3\\]")~"td_rep",
    str_detect(param,"omega_local\\[1,2,3\\]")~"cd_rep",
    str_detect(param,"omega_local\\[2,1,2\\]")~"tc_att",
    str_detect(param,"omega_local\\[2,1,3\\]")~"td_att",
    str_detect(param,"omega_local\\[2,2,3\\]")~"cd_att",
  )) %>%
  left_join(tibble(
    cor=c("tc_rep","td_rep","cd_rep","tc_att","td_att","cd_att"),
    gen=c(r_tc_rep,r_td_rep,r_cd_rep,r_tc_att,r_td_att,r_cd_att),
  )) %>%
  select(N, mean, `2.5%`,`97.5%`,cor,gen) %>%
  rename(est=mean)
omega_local_post %>%
  pivot_longer(-c(N,cor,`2.5%`,`97.5%`)) %>%
  mutate(`2.5%`=if_else(name=="est",`2.5%`,NA_real_),
         `97.5%`=if_else(name=="est",`97.5%`,NA_real_)) %>%
  separate(cor,into = c("cor","effect")) %>%
  ggplot(aes(N,value,col=name,shape=name))+
  geom_point()+
  geom_errorbar(aes(ymin=`2.5%`,ymax=`97.5%`),alpha=.4)+
  geom_hline(yintercept=0,linetype="dashed",alpha=.5)+
  ggsci::scale_color_d3(name="")+
  scale_shape_manual(values=c(1,4),name="")+
  facet_grid(effect~cor,scales = "fixed")+
  ggthemes::theme_few()+
  theme(text=element_text(size=14))
ggsave(filename=here("recovery_simulations","plots","gen_recover_omega_local.jpeg"),width=7,height=5)

# plot omega global =======================================================================
omega_global_post <- omega_post %>%
  filter(str_detect(param,"omega_global")) %>%
  filter(str_detect(param,"\\[1,2\\]|\\[1,3\\]|\\[2,3\\]")) %>%
  mutate(cor=case_when(
    str_detect(param,"\\[1,2\\]")~"tc",
    str_detect(param,"\\[1,3\\]")~"td",
    str_detect(param,"\\[2,3\\]")~"cd",
  )) %>%
  left_join(tibble(
    cor=c("tc","td","cd"),
    gen=c(cor_global_gen[1,2],cor_global_gen[1,3],cor_global_gen[2,3])
  )) %>%
  select(N, mean, `2.5%`,`97.5%`,cor,gen) %>%
  rename(est=mean)

omega_global_post %>%
  pivot_longer(-c(N,cor,`2.5%`,`97.5%`)) %>%
  mutate(`2.5%`=if_else(name=="est",`2.5%`,NA_real_),
         `97.5%`=if_else(name=="est",`97.5%`,NA_real_)) %>%
  ggplot(aes(N,value,col=name,shape=name))+
  geom_point()+
  geom_errorbar(aes(ymin=`2.5%`,ymax=`97.5%`),alpha=.4)+
  ggsci::scale_color_d3(name="")+
  scale_shape_manual(values=c(1,4),name="")+
  scale_y_continuous(limits=c(0,1))+
  facet_grid(.~cor,scales = "fixed")+
  ggthemes::theme_few()+
  theme(text=element_text(size=14))
ggsave(filename=here("recovery_simulations","plots","gen_recover_omega_global.jpeg"),width=7,height=5)

# plot combined omega ================================================================================
omega_combined_post <- omega_post %>%
  filter(str_detect(param,"Omega")) %>%
  mutate(cor=case_when(
    str_detect(param,"Omega\\[1,1,2\\]")~"tc_rep",
    str_detect(param,"Omega\\[1,1,3\\]")~"td_rep",
    str_detect(param,"Omega\\[1,2,3\\]")~"cd_rep",
    str_detect(param,"Omega\\[2,1,2\\]")~"tc_att",
    str_detect(param,"Omega\\[2,1,3\\]")~"td_att",
    str_detect(param,"Omega\\[2,2,3\\]")~"cd_att",
  )) %>%
  filter(!is.na(cor)) %>%
  left_join(tibble(
    cor=c("tc_rep","td_rep","cd_rep","tc_att","td_att","cd_att"),
    gen=c(cors_rep_gen[1,2],cors_rep_gen[1,3],cors_rep_gen[2,3],cors_att_gen[1,2],cors_att_gen[1,3],cors_att_gen[2,3]),
  )) %>%
  select(N, mean, `2.5%`,`97.5%`,cor,gen) %>%
  rename(est=mean) 
omega_combined_post
omega_combined_post %>%
  pivot_longer(-c(N,cor,`2.5%`,`97.5%`)) %>%
  mutate(`2.5%`=if_else(name=="est",`2.5%`,NA_real_),
         `97.5%`=if_else(name=="est",`97.5%`,NA_real_)) %>%
  separate(cor,into = c("cor","effect")) %>%
  ggplot(aes(N,value,col=name,shape=name))+
  geom_point()+
  geom_errorbar(aes(ymin=`2.5%`,ymax=`97.5%`),alpha=.4)+
  ggsci::scale_color_d3(name="")+
  scale_shape_manual(values=c(1,4),name="")+
  scale_y_continuous(limits=c(.3,.75))+
  facet_grid(effect~cor,scales = "fixed")+
  ggthemes::theme_few()+
  theme(text=element_text(size=14))
ggsave(filename=here("recovery_simulations","plots","gen_recover_omega_combined.jpeg"),width=7,height=5)

