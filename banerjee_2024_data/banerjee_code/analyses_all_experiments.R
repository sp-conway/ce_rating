library(dplyr)
library(lme4)
library(car)
library(ggplot2)

dat <- read.table('RE data cleaned.csv', header=T, sep=',')

# ----- Experiment 1 -----

dat_Exp1 <- dat %>% filter(Experiment == 1) %>%
  mutate(Choice = factor(Choice, levels=c('T','C','D')),
         Decoy = factor(NumAlt, levels=c(3,2), labels=c('Present','Absent')),
         Product = factor(Product, levels=c('Grills','Mp 3 players','Backpack'), 
                          labels=c('Grills','MP3 players','Backpacks')),
         LowDim = factor(LowDim))

#Create Table 1
View(dat_Exp1 %>% group_by(Product, LowDim, NumAlt) %>% count(Choice, .drop=F))
View(dat_Exp1 %>% group_by(LowDim, NumAlt) %>% count(Choice, .drop=F))

#Logistic regression model
dat_Exp1 <- dat_Exp1 %>%
  filter(Choice == 'T' | Choice == 'C') %>%
  mutate(chose_C = as.integer(Choice == 'C')) 

fit.logistic <- glm(chose_C ~ Decoy * LowDim * Product, family='binomial', 
                    data=dat_Exp1,
                    contrasts = list(Decoy = 'contr.sum',
                                     LowDim = 'contr.sum',
                                     Product = 'contr.sum'))
summary(fit.logistic)
head(model.matrix(fit.logistic))


# ----- Experiment 2 -----

dat_Exp2 <- dat %>% filter(Experiment == 2) %>%
  mutate(Choice = factor(Choice, levels=c('T','C','D')),
         Decoy = ifelse(NumAlt==3, 1, 0),
         LowDim3040 = ifelse(LowDim=="30-40", 1, 0),
         LowDim7080 = ifelse(LowDim=="70-80", 1, 0),
         HighDim1st = ifelse(HighValueColumn==1, 1, 0))

#Create Table 3
View(dat_Exp2 %>% group_by(Product, LowDim, NumAlt) %>% count(Choice, .drop=F))
View(dat_Exp2 %>% group_by(LowDim, NumAlt) %>% count(Choice, .drop=F))

#Logistic regression modeling
dat_Exp2 <- dat_Exp2 %>%
  filter(Choice == 'T' | Choice == 'C') %>%
  mutate(chose_C = as.integer(Choice == 'C')) %>%
  mutate(Subj = factor(Participant),
         Item = factor(Product))

fit1 <- glmer(chose_C ~ Decoy * LowDim7080 + (1 | Subj) + (1 | Item),
              family = 'binomial',
              data = dat_Exp2)
summary(fit1)
head(model.matrix(fit1), n=30)

fit2 <- glmer(chose_C ~ Decoy * LowDim3040 + (1 | Subj) + (1 | Item),
              family = 'binomial',
              data = dat_Exp2)
summary(fit2)
head(model.matrix(fit2), n=30)

fit3 <- glmer(chose_C ~ Decoy * HighDim1st * LowDim7080 + (1 | Subj) + (1 | Item),
              family = 'binomial',
              data = dat_Exp2,
              control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(fit3)
head(model.matrix(fit3), n=30)


# ----- Experiment 2 alternate (Footnote 3) -----

dat_Exp2alt <- dat %>% filter(Experiment == 'Footnote_3') %>%
  mutate(Choice = factor(Choice, levels=c('T','C','D')),
         Decoy = ifelse(NumAlt==3, 1, 0),
         LowDim3040 = ifelse(LowDim=="30-40", 1, 0),
         LowDim4050 = ifelse(LowDim=="40-50", 1, 0),
         HighDim1st = ifelse(HighValueColumn==1, 1, 0))

#Choice frequencies
View(dat_Exp2alt %>% group_by(Product, LowDim, NumAlt) %>% count(Choice, .drop=F))
View(dat_Exp2alt %>% group_by(LowDim, NumAlt) %>% count(Choice, .drop=F))

#Logistic regression modeling
dat_Exp2alt <- dat_Exp2alt %>%
  filter(Choice == 'T' | Choice == 'C') %>%
  mutate(chose_C = as.integer(Choice == 'C')) %>%
  mutate(Subj = factor(Participant),
         Item = factor(Product))

fit1 <- glmer(chose_C ~ Decoy * LowDim4050 + (1 + LowDim4050 | Subj) + (1 | Item),
              family = 'binomial',
              data = dat_Exp2alt)
summary(fit1)
head(model.matrix(fit1), n=30)

# fit2 <- glmer(chose_C ~ Decoy * LowDim3040 + (1 + LowDim3040 | Subj) + (1 | Item),
#               family = 'binomial',
#               data = dat_Exp2alt)
# summary(fit2)
# head(model.matrix(fit2), n=30)
# 
# fit3 <- glmer(chose_C ~ Decoy * HighDim1st * LowDim4050 + (1 + LowDim4050 | Subj) + (1 | Item),
#               family = 'binomial',
#               data = dat_Exp2,
#               control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
# summary(fit3)
# head(model.matrix(fit3), n=30)



# ----- Experiment 3 -----

dat_Exp3 <- dat %>% filter(Experiment == 3) %>%
  mutate(Choice = factor(Choice, levels=c('T','C','D')),
         Decoy = ifelse(NumAlt==3, 1, 0),
         HighDim7060 = ifelse(HighDim=="70-60", 1, 0),
         HighDim10090 = ifelse(HighDim=="100-90", 1, 0),
         HighDim1st = ifelse(HighValueColumn==1, 1, 0))

#Create Table 5
View(dat_Exp3 %>% group_by(Product, HighDim, NumAlt) %>% count(Choice, .drop=F))
View(dat_Exp3 %>% group_by(HighDim, NumAlt) %>% count(Choice, .drop=F))

#Logistic regression modeling
dat_Exp3 <- dat_Exp3 %>%
  filter(Choice == 'T' | Choice == 'C') %>%
  mutate(chose_C = as.integer(Choice == 'C')) %>%
  mutate(Subj = factor(Participant),
         Item = factor(Product))

fit1 <- glmer(chose_C ~ Decoy * HighDim10090 + (1 + HighDim10090 | Subj) + (1 | Item),
              family = 'binomial',
              data = dat_Exp3)
summary(fit1)
head(model.matrix(fit1), n=30)

fit2 <- glmer(chose_C ~ Decoy * HighDim7060 + (1 + HighDim7060 | Subj) + (1 | Item),
              family = 'binomial',
              data = dat_Exp3)
summary(fit2)
head(model.matrix(fit2), n=30)

fit3 <- glmer(chose_C ~ Decoy * HighDim1st * HighDim10090 + (1 + HighDim10090 | Subj) + (1 | Item),
              family = 'binomial',
              data = dat_Exp3,
              control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(fit3)
head(model.matrix(fit3), n=30)

# ----- Experiment 4 -----

dat_Exp4 <- dat %>% filter(Experiment == 4) %>%
  mutate(Choice = factor(Choice, levels=c('T','C','D')),
         Decoy = factor(NumAlt, levels=c(2,3), labels=c('Absent','Present')),
         LowDim = factor(LowDim, levels=c('30-40','40-50','50-60','60-70')),
         HighDim1st = ifelse(HighValueColumn==1, 1, 0))

#Logistic regression modeling
dat_Exp4 <- dat_Exp4 %>%
  filter(Choice == 'T' | Choice == 'C') %>%
  mutate(chose_C = as.integer(Choice == 'C')) %>%
  mutate(Subj = factor(Participant),
         Item = factor(Product),
         Decoy = relevel(Decoy, ref='Present'))

contrasts(dat_Exp4$Decoy) <- 'contr.sum'
contrasts(dat_Exp4$LowDim) <- 'contr.poly'

fit1 <- glmer(chose_C ~ Decoy * LowDim + (1 | Subj) + (1 | Item),
              family = 'binomial',
              data = dat_Exp4)
summary(fit1)
head(model.matrix(fit1), n=30)

Anova(fit1, type=3, test='Chisq')

dat_Exp4 <- dat_Exp4 %>%
  mutate(Config = factor(HighDim1st, levels=c(1, 0)))

contrasts(dat_Exp4$Config) <- 'contr.sum'

fit2 <- glmer(chose_C ~ Decoy * Config * LowDim + (1 | Subj) + (1 | Item),
              family = 'binomial',
              data = dat_Exp4)
summary(fit2)
head(model.matrix(fit2), n=30)

#Make Figure 3
temp1 <- dat_Exp4 %>%
  mutate(Decoy = relevel(Decoy, ref='Absent')) %>%
  group_by(Decoy, LowDim, Item) %>%
  summarise(Prop_C = mean(chose_C)) %>%
  mutate(LowDim = factor(LowDim, levels=c('30-40','40-50','50-60','60-70'),
                         labels=c('T = (100, 30)\nC =   (90, 40)\nD = (100, 10)',
                                  'T = (100, 40)\nC =   (90, 50)\nD = (100, 10)',
                                  'T = (100, 50)\nC =   (90, 60)\nD = (100, 10)',
                                  'T = (100, 60)\nC =   (90, 70)\nD = (100, 10)')))


temp2 <- temp1 %>%
  group_by(Decoy, LowDim) %>%
  summarise(Mean_Prop_C = mean(Prop_C))

ggplot(temp1, aes(x=Decoy, y=Prop_C)) +
  geom_bar(data=temp2, aes(x=Decoy, y=Mean_Prop_C), stat = 'identity', fill='gray70', alpha=.5, col='black') +
  geom_point(aes(col=Item, pch=Item)) +
  geom_line(aes(group=Item, col=Item), alpha=0.5, show.legend=F) +
  facet_wrap(~ LowDim, ncol=4) +
  scale_shape_manual(values=1:12) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size=12),
        legend.title = element_text(hjust=.5)) +
  guides(shape = guide_legend(title = 'Product'), col = guide_legend(title = 'Product')) +
  ylab('P(C) / (P(T) + P(C))')


# ----- Experiment 5 -----

dat_Exp5 <- dat %>% filter(Experiment == 5) %>%
  mutate(Choice = factor(Choice, levels=c('T','C','D')),
         Decoy = factor(NumAlt, levels=c(2,3), labels=c('Absent','Present')),
         HighDim = factor(HighDim, levels=c('70-60','80-70','90-80','100-90')),
         HighDim1st = ifelse(HighValueColumn==1, 1, 0))

#Logistic regression modeling
dat_Exp5 <- dat_Exp5 %>%
  filter(Choice == 'T' | Choice == 'C') %>%
  mutate(chose_C = as.integer(Choice == 'C')) %>%
  mutate(Subj = factor(Participant),
         Item = factor(Product),
         Decoy = relevel(Decoy, ref='Present'))

contrasts(dat_Exp5$Decoy) <- 'contr.sum'
contrasts(dat_Exp5$HighDim) <- 'contr.poly'

fit1 <- glmer(chose_C ~ Decoy * HighDim + (1 | Subj),
              family = 'binomial',
              data = dat_Exp5)
summary(fit1)
head(model.matrix(fit1), n=30)

Anova(fit1, type=3, test='Chisq')

dat_Exp5 <- dat_Exp5 %>%
  mutate(Config = factor(HighDim1st, levels=c(1, 0)))

contrasts(dat_Exp5$Config) <- 'contr.sum'

fit2 <- glmer(chose_C ~ Decoy * Config * HighDim + (1 | Subj),
              family = 'binomial',
              data = dat_Exp5)
summary(fit2)

#Make Figure 4
temp1 <- dat_Exp5 %>%
  mutate(Decoy = relevel(Decoy, ref='Absent')) %>%
  group_by(Decoy, HighDim, Item) %>%
  summarise(Prop_C = mean(chose_C)) %>%
  mutate(HighDim = factor(HighDim, levels=c('70-60','80-70','90-80','100-90'),
                          labels=c('T = (70, 30)\nC = (60, 40)\nD = (70, 25)',
                                   'T = (80, 30)\nC = (70, 40)\nD = (80, 25)',
                                   'T = (90, 30)\nC = (80, 40)\nD = (90, 25)',
                                   'T = (100, 30)\nC =   (90, 40)\nD = (100, 25)')))


temp2 <- temp1 %>%
  group_by(Decoy, HighDim) %>%
  summarise(Mean_Prop_C = mean(Prop_C))


ggplot(temp1, aes(x=Decoy, y=Prop_C)) +
  geom_bar(data=temp2, aes(x=Decoy, y=Mean_Prop_C), stat = 'identity', fill='gray70', alpha=.5, col='black') +
  geom_point(aes(col=Item, pch=Item)) +
  geom_line(aes(group=Item, col=Item), alpha=0.5, show.legend=F) +
  facet_wrap(~ HighDim, ncol=4) +
  scale_shape_manual(values=1:12) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size=12),
        legend.title = element_text(hjust=.5)) +
  guides(shape = guide_legend(title = 'Product'), col = guide_legend(title = 'Product')) +
  ylab('P(C) / (P(T) + P(C))')
