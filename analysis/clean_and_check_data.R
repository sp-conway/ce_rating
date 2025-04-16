# reading and cleaning data
# also checking to see how many people passed catch trials

# setup, read raw data ======================================================================================
rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)

catch_criterion <- 5 # need to have >=5/8 trials correct in both price and choice to pass
f <- here("data/raw/sc-pref-rating") %>% 
  dir_ls(regexp = ".csv")

read <- function(ff){
  print(ff)
  d <- data.table::fread(ff) %>%
    as_tibble()
  if(!("age" %in% colnames(d))){
    return(NULL)
  }else{
    if(!is.na(unique(d$age))){
      print(unique(d$age))
    }
    dd <- d %>%
      select(c(screen_id,trial_type,trial_index,run_id,age,race,ethnicity,gender,
               exp_duration,rt,response,phase,order,effect,t_high,category,d1_best,d2_best,d1_worst1,
               d2_worst1,d1_worst2,d2_worst2,first_dim,d1_t,d2_t,d1_c,d2_c,d1_d,d2_d,
               d1_name,d2_name,strategy_response,question_order)) %>%
      rename(participant=run_id) %>%
      mutate(across(c(d1_best,d2_best,d1_worst1,d2_worst1,d1_worst2,d2_worst2,t_high,d1_t,d2_t,d1_c,d2_c,d1_d,d2_d),
                    ~na_if(.x,"null")),
             age=as.numeric(age)) # JS uses null for na values
  }
  return(dd)
}

# ppt numbers are not sequential due to people starting and quitting task
# renumbering people sequentially
renumber <- function(dat){
  n_ppt <- length(unique(dat$participant))
  ppt_key <- tibble(
    participant = sort(unique(dat$participant)),
    participant_new = seq(1,n_ppt,1)
  )
  dat1 <- dat %>%
    left_join(ppt_key) %>%
    select(-participant) %>%
    rename(participant=participant_new) %>%
    relocate(participant,.before = everything())
  return(dat1)
}

d <- map(f,read) %>%
  list_rbind() %>%
  renumber()

# clean data =====================================================================================
clean_price <- function(dat){
  dat1 <- dat %>%
    filter(screen_id=="rating_trial") %>%
    separate(response,into=c("r1","r2","r3"),sep=",") %>%
    mutate(price_1=as.numeric(str_extract(r1,'(?<=")[:digit:]{1,}')),
           price_2=as.numeric(str_extract(r2,'(?<=")[:digit:]{1,}')),
           price_3=as.numeric(str_extract(r3,'(?<=")[:digit:]{1,}'))) %>%
    select(-c(age,trial_type,race,ethnicity,question_order,strategy_response,r1,r2,r3))
  return(dat1)
}
clean_choice <- function(dat){
  dat1  <- dat %>%
    filter(screen_id=="choice_trial") %>%
    mutate(response=str_extract(response,'[:upper:]{1}'),
           choice=case_when(
             response=="A"~str_sub(order,1,1),
             response=="B"~str_sub(order,2,2),
             response=="C"~str_sub(order,3,3)
           )) %>%
    select(-c(age,trial_type,race,ethnicity,question_order,strategy_response))
 return(dat1)
}

clean_demo <- function(dat){
  dat1 <- dat %>%
    distinct(participant,age,race,ethnicity,gender,strategy_response,exp_duration) %>%
    filter(!is.na(strategy_response)) %>%
    mutate(across(everything(),~str_remove_all(.x,paste(c('\\[','\\]','"'),collapse="|"))))
  return(dat1)
}
price <- clean_price(d)
choice <- clean_choice(d)
demo <- clean_demo(d)

# check catch trials ============================================================
price_catch <- price %>%
  filter(effect=="catch") %>%
  mutate(price_b=case_when(
    str_sub(order,1,1)=="b"~price_1,
    str_sub(order,2,2)=="b"~price_2,
    str_sub(order,3,3)=="b"~price_3
  ),
  price_w1=case_when(
    order=="wbw"~price_1,
    order=="wwb"~price_1,
    order=="bww"~price_2,
  ),
  price_w2=case_when(
    order=="wbw"~price_3,
    order=="wwb"~price_2,
    order=="bww"~price_3,
  )) %>%
  rowwise() %>%
  mutate(correct=price_b>=price_w1 & price_b>=price_w2) %>%
  ungroup()

choice_catch <- choice %>%
  filter(effect=="catch") %>%
  mutate(correct=choice=="b")


check_counts <- function(d,type){
  d %>%
    group_by(participant) %>%
    summarise(N=sum(correct)) %>%
    ungroup() %>%
    mutate(type=type)
}
 
catch_correct <- map2(list(price_catch,choice_catch),c("price","choice"),check_counts) %>%
  list_rbind() %>%
  pivot_wider(names_from = type,values_from = N)

# IMPORTANT - FIGURE OUT WHO WE ARE KEEPING
ppt_keep <- catch_correct %>%
  filter(price>=5 & choice>=5) %>%
  pull(participant)

# filter !!!! ALSO CHANGE RT TO SECS =======================================================
filter_data <- function(data,keep) data %>% filter(participant %in% ppt_keep) 
price_filtered <- filter_data(price,ppt_keep) %>%
  mutate(rt=as.numeric(rt)/1000)
choice_filtered <- filter_data(choice,ppt_keep) %>%
  mutate(rt=as.numeric(rt)/1000)
demo_filtered <- filter_data(demo,ppt_keep)

# Print out exp. info =====================================================================================================================================================================
cat("\n============\nInitial Sample Size: N=",length(unique(d$participant)),"\n============\n",sep="")
cat("\n============\n",length(unique(d$participant))-length(ppt_keep)," Participants Removed\n============\n",sep="")
cat("\n============\nFinal Sample Size: N=",length(unique(price_filtered$participant)),"\n============\n",sep="")

# write cleaned data to files =====================================================================================
write_csv(price_filtered,here("data","clean","price.csv"))
write_csv(choice_filtered,here("data","clean","choice.csv"))
write_csv(demo_filtered,here("data","clean","demo.csv"))

