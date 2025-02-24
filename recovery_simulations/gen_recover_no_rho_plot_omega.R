rm(list=ls())
library(tidyverse)
library(here)
library(fs)

dirs <- here("recovery_simulations") %>%
  dir_ls(type="directory",regexp = "no_rho")

# Repulsion effect correlations
r_tc_rep <- r_cd_rep <- .5
r_td_rep <- .6
cors_rep <- matrix(c(1,r_tc_rep,r_td_rep,r_tc_rep,1,r_cd_rep,r_td_rep,r_cd_rep,1),nrow=3,ncol=3,byrow=T)

# attraction effect correlations
r_tc_att <- r_cd_att <- .6
r_td_att <- .5
cors_att <- matrix(c(1,r_tc_att,r_td_att,r_tc_att,1,r_cd_att,r_td_att,r_cd_att,1),nrow=3,ncol=3,byrow=T)

load_omega <- function(f){
  load(path(f,"fit_summary.RData"))
  fit_summary1 <- as.data.frame(fit_summary$summary)
  fit_summary1$param <- rownames(fit_summary$summary)
  fit_summary1$N <- as.numeric(str_extract(f,"(?<=N_)[:digit:]{3}"))
  fit_summary2 <- fit_summary1 %>%
    filter(str_detect(param,"omega")) %>%
    mutate(cor=case_when(
      str_detect(param,"\\[1,1,2\\]")~"tc_rep",
      str_detect(param,"\\[1,1,3\\]")~"td_rep",
      str_detect(param,"\\[1,2,3\\]")~"cd_rep",
      str_detect(param,"\\[2,1,2\\]")~"tc_att",
      str_detect(param,"\\[2,1,3\\]")~"td_att",
      str_detect(param,"\\[2,2,3\\]")~"cd_att",
    )) %>%
    filter(!is.na(cor)) %>%
    separate(cor,into=c("cor","effect")) %>%
    select(-c(se_mean,sd,n_eff,Rhat,param))
  return(fit_summary2)
}
omega_post <- map(dirs,load_omega) %>%
  list_rbind() %>%
  rename(est=mean) %>%
  left_join(
    tibble(cor=c("tc","td","cd","tc","td","cd"),
           effect=c(rep("rep",3),rep("att",3)),
           gen=c(r_tc_rep,r_td_rep,r_cd_rep,r_tc_att,r_td_att,r_cd_att))
  )
omega_post %>%
  pivot_longer(c(gen,est)) %>%
  mutate(`2.5%`=if_else(str_detect(name,"est"),`2.5%`,NA_real_),
         `97.5%`=if_else(str_detect(name,"est"),`97.5%`,NA_real_)) %>%
  ggplot(aes(N,value,col=name,shape=name))+
  geom_point()+
  geom_errorbar(aes(ymin=`2.5%`,ymax=`97.5%`),alpha=.4)+
  ggsci::scale_color_d3(name="")+
  scale_shape_manual(values=c(1,4),name="")+
  scale_y_continuous(limits=c(.3,.75))+
  facet_grid(effect~cor,scales = "fixed")+
  ggthemes::theme_few()+
  theme(text=element_text(size=14))
ggsave(filename=here("recovery_simulations","plots","gen_recover_omega_no_rho.jpeg"),width=7,height=5)

