# reading and cleaning data
# also checking to see how many people passed catch trials
# choice only experiment
# setup, read raw data ======================================================================================
rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)

catch_criterion <- 5 # need to have >=5/8 trials correct to pass
f <- here("data_choice_only/raw/sc-pref-choice") %>% 
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
      select(-c(success,timeout,failed_images,failed_audio,failed_video,recorded_at,source_code_version,
                browser_version,platform,platform_version,referer,accept_language)) %>%
      rename(participant=run_id) %>%
      mutate(across(c(d1_best,d2_best,d1_worst1,d2_worst1,d1_worst2,d2_worst2,t_high,d1_t,d2_t,d1_c,d2_c,d1_d,d2_d),
                    ~na_if(.x,"null")),
             age=as.numeric(age),
             other_countries=as.character(other_countries)) # JS uses null for na values
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
    distinct(participant,age,race,ethnicity,gender,strategy_response,exp_duration,prev_study) %>%
    filter(!is.na(strategy_response)) %>%
    mutate(across(everything(),~str_remove_all(.x,paste(c('\\[','\\]','"'),collapse="|"))))
  return(dat1)
}
choice <- clean_choice(d)
demo <- clean_demo(d)

# check catch trials ============================================================
choice_catch <- choice %>%
  filter(effect=="catch") %>%
  mutate(correct=choice=="b")


check_counts <- function(d){
  d %>%
    group_by(participant) %>%
    summarise(N=sum(correct)) %>%
    ungroup() 
}
 
catch_correct <- check_counts(choice_catch)

# IMPORTANT - FIGURE OUT WHO WE ARE KEEPING
ppt_keep <- catch_correct %>%
  filter(N>=catch_criterion) %>%
  pull(participant)

# filter !!!! ALSO CHANGE RT TO SECS =======================================================
filter_data <- function(data,keep) data %>% filter(participant %in% ppt_keep) 
choice_filtered <- filter_data(choice,ppt_keep) %>%
  mutate(rt=as.numeric(rt)/1000)
demo_filtered <- filter_data(demo,ppt_keep)

# Print out exp. info =====================================================================================================================================================================
cat("\n============\nInitial Sample Size: N=",length(unique(d$participant)),"\n============\n",sep="")
cat("\n============\n",length(unique(d$participant))-length(ppt_keep)," Participants Removed\n============\n",sep="")
cat("\n============\nFinal Sample 
    Size: N=",length(unique(choice_filtered$participant)),"\n============\n",sep="")

# write cleaned data to files =====================================================================================
write_csv(choice_filtered,here("data_choice_only","clean","choice.csv"))
write_csv(demo_filtered,here("data_choice_only","clean","demo.csv"))

