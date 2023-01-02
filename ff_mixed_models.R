library(readxl)
library(tidyverse)
library(lme4)
library(lmerTest)
library(effectsize)
library(emmeans)
emm_options(lmer.df = "satterthwaite")
emm_options(lmerTest.limit = 51126)

###################### P1 ###################### No effects

#importing data
data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\P1_amps_long_format.xlsx")
data = transform(data, emotion=factor(faces), task=factor(task), masking=factor(masking), subject = factor(subject))

#plotting 
hist(data$erp_amp)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#filtering out outliers
outliers <- data %>% filter(abs(scale(erp_amp)) > 3)
data = data %>% filter(abs(scale(erp_amp)) <= 3)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#coding and contrasts
data$e <- 0.5
data$e[data$emotion == "fearful"] <- -0.5
data$t <- 0.5
data$t[data$task == "ID"] <- -0.5
data$m <- 0.5
data$m[data$masking == "masked"] <- -0.5

contrasts(data$emotion) <- rbind(-.5, .5)
colnames(contrasts(data$emotion)) <- levels(data$emotion)[2]
contrasts(data$task) <- rbind(-.5, .5)
colnames(contrasts(data$task)) <- levels(data$task)[2]
contrasts(data$masking) <- rbind(-.5, .5)
colnames(contrasts(data$masking)) <- levels(data$masking)[2]

#top-down procedure to establish non-parsimonious model
full_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + e * m * t| subject), data=data, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_full_model <- lmer(erp_amp ~ 1 + e * t * m + (1 + e * m * t || subject), data=data, 
                          control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_full_model)
summary(rePCA(uncorr_full_model))


reduced_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + m * t| subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_reduced_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + m * t || subject), data=data, 
                             control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_reduced_model)
summary(rePCA(uncorr_reduced_model))


reduced_model_2 <- lmer(erp_amp ~ 1 + e * m * t + (1 + t| subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

#testing model against intercept model
intercept_model <- lmer(erp_amp ~ 1 + e * m * t + (1| subject), data=data, 
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


anova(intercept_model, reduced_model_2) # reduced_model_2 is better according to AIC


# specifying the final model
P1_model <- lmer(erp_amp ~ 1 + emotion * task * masking + (1 + task| subject), data=data, 
                    control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(P1_model)

#checking normality assumptions
qqnorm(resid(P1_model))
qqline(resid(P1_model))


#extracting anova
P1_model <- P1_model

P1_rand_table <- rand(P1_model) #values are for the model without the listed effect!!!
P1_rand_table

P1_anova_table <- anova(P1_model)
P1_anova_table

###################### N170 ###################### effect of emotion and masking

#importing data
data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\N170_amps_long_format.xlsx")
data = transform(data, emotion=factor(faces), task=factor(task), masking=factor(masking), subject = factor(subject))

#plotting 
hist(data$erp_amp)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#filtering out outliers
outliers <- data %>% filter(abs(scale(erp_amp)) > 3)
data = data %>% filter(abs(scale(erp_amp)) <= 3)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#coding and contrasts
data$e <- 0.5
data$e[data$emotion == "fearful"] <- -0.5
data$t <- 0.5
data$t[data$task == "ID"] <- -0.5
data$m <- 0.5
data$m[data$masking == "masked"] <- -0.5

contrasts(data$emotion) <- rbind(-.5, .5)
colnames(contrasts(data$emotion)) <- levels(data$emotion)[2]
contrasts(data$task) <- rbind(-.5, .5)
colnames(contrasts(data$task)) <- levels(data$task)[2]
contrasts(data$masking) <- rbind(-.5, .5)
colnames(contrasts(data$masking)) <- levels(data$masking)[2]

#top-down procedure to establish non-parsimonious model
full_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + e * m * t| subject), data=data, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_full_model <- lmer(erp_amp ~ 1 + e * t * m + (1 + e * m * t || subject), data=data, 
                          control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_full_model)
summary(rePCA(uncorr_full_model))


reduced_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + m + t| subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


#testing model against intercept model
intercept_model <- lmer(erp_amp ~ 1 + e * m * t + (1| subject), data=data, 
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


anova(intercept_model, reduced_model) # reduced_model is better according to AIC

# specifying the final model
N170_model <- lmer(erp_amp ~ 1 + emotion * task * masking + (1 + masking + task| subject), data=data, 
                    control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(N170_model)

#checking normality assumptions
qqnorm(resid(N170_model))
qqline(resid(N170_model))


#extracting anova
N170_rand_table <- rand(N170_model) 
N170_rand_table

N170_anova_table <- anova(N170_model)
N170_anova_table

N170_emotion_contr_table <- summary(emmeans(N170_model, pairwise~ emotion | masking* task, combine = T))$contrasts
N170_emotion_contr_table

N170_emotion_emmeans <- summary(emmeans(N170_model, pairwise~emotion, combine = T))$emmeans
N170_emotion_emmeans

N170_masking_emmeans = summary(emmeans(N170_model, pairwise~masking, combine = T))$emmeans
N170_masking_emmeans



###################### P2 ###################### effects of masking, task and masking*task

#importing data
data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\P2_amps_long_format.xlsx")
data = transform(data, emotion=factor(faces), task=factor(task), masking=factor(masking), subject = factor(subject))

#plotting 
hist(data$erp_amp)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#filtering out outliers
outliers <- data %>% filter(abs(scale(erp_amp)) > 3)
data = data %>% filter(abs(scale(erp_amp)) <= 3)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#coding and contrasts
data$e <- 0.5
data$e[data$emotion == "fearful"] <- -0.5
data$t <- 0.5
data$t[data$task == "ID"] <- -0.5
data$m <- 0.5
data$m[data$masking == "masked"] <- -0.5

contrasts(data$emotion) <- rbind(-.5, .5)
colnames(contrasts(data$emotion)) <- levels(data$emotion)[2]
contrasts(data$task) <- rbind(-.5, .5)
colnames(contrasts(data$task)) <- levels(data$task)[2]
contrasts(data$masking) <- rbind(-.5, .5)
colnames(contrasts(data$masking)) <- levels(data$masking)[2]

#top-down procedure to establish non-parsimonious model
full_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + e * m * t| subject), data=data, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_full_model <- lmer(erp_amp ~ 1 + e * t * m + (1 + e * m * t || subject), data=data, 
                          control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_full_model)
summary(rePCA(uncorr_full_model))


reduced_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + m * t| subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


#testing model against intercept model
intercept_model <- lmer(erp_amp ~ 1 + e * m * t + (1| subject), data=data, 
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


anova(intercept_model, reduced_model) # reduced_model is better according to AIC

# specifying the final model
P2_model <- lmer(erp_amp ~ 1 + emotion * task * masking + (1 + masking * task| subject), data=data, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(P2_model)

#checking normality assumptions
qqnorm(resid(P2_model))
qqline(resid(P2_model))


#extracting anova
P2_rand_table <- rand(P2_model) 
P2_rand_table

P2_anova_table <- anova(P2_model)
P2_anova_table

P2_masking_contr_table = summary(emmeans(P2_model, pairwise~ masking | task, combine = T))$contrasts
P2_masking_contr_table

P2_masking_emmeans <- summary(emmeans(P2_model, pairwise~masking, combine = T))$emmeans
P2_masking_emmeans

P2_task_contr_table = summary(emmeans(P2_model, pairwise~ task | masking, combine = T))$contrasts
P2_task_contr_table


P2_task_emmeans <- summary(emmeans(P2_model, pairwise~task | masking, combine = T))$emmeans
P2_task_emmeans


###################### frontal N2 ###################### effects of masking, task and masking*emotion

#importing data
data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\N2_amps_long_format.xlsx")
data = transform(data, emotion=factor(faces), task=factor(task), masking=factor(masking), subject = factor(subject))

#plotting 
hist(data$erp_amp)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#filtering out outliers
outliers <- data %>% filter(abs(scale(erp_amp)) > 3)
data = data %>% filter(abs(scale(erp_amp)) <= 3)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#coding and contrasts
data$e <- 0.5
data$e[data$emotion == "fearful"] <- -0.5
data$t <- 0.5
data$t[data$task == "ID"] <- -0.5
data$m <- 0.5
data$m[data$masking == "masked"] <- -0.5

contrasts(data$emotion) <- rbind(-.5, .5)
colnames(contrasts(data$emotion)) <- levels(data$emotion)[2]
contrasts(data$task) <- rbind(-.5, .5)
colnames(contrasts(data$task)) <- levels(data$task)[2]
contrasts(data$masking) <- rbind(-.5, .5)
colnames(contrasts(data$masking)) <- levels(data$masking)[2]

#top-down procedure to establish non-parsimonious model
full_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + e * m * t| subject), data=data, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_full_model <- lmer(erp_amp ~ 1 + e * t * m + (1 + e * m * t || subject), data=data, 
                          control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_full_model)
summary(rePCA(uncorr_full_model))


reduced_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + m * t +e| subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


#testing model against intercept model
intercept_model <- lmer(erp_amp ~ 1 + e * m * t + (1| subject), data=data, 
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


anova(intercept_model, reduced_model) # reduced_model is better according to AIC

# specifying the final model
N2_model <- lmer(erp_amp ~ 1 + emotion * task * masking + (1 + masking * task + emotion| subject), data=data, 
                  control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(N2_model)

#checking normality assumptions
qqnorm(resid(N2_model))
qqline(resid(N2_model))


#extracting anova
N2_rand_table <- rand(N2_model) 
N2_rand_table

N2_anova_table <- anova(N2_model)
N2_anova_table


N2_emotion_contr_table = summary(emmeans(N2_model, pairwise~ emotion | masking, combine = T))$contrasts
N2_emotion_contr_table

N2_emotion_emmeans <- summary(emmeans(N2_model, pairwise~emotion | masking, combine = T))$emmeans
N2_emotion_emmeans


N2_task_emmeans <- summary(emmeans(N2_model, pairwise~task, combine = T))$emmeans
N2_task_emmeans

N2_masking_emmeans <- summary(emmeans(N2_model, pairwise~masking, combine = T))$emmeans
N2_masking_emmeans


###################### EPN ###################### effects of masking, task and masking*task

#importing data
data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\EPN_amps_long_format.xlsx")
data = transform(data, emotion=factor(faces), task=factor(task), masking=factor(masking), subject = factor(subject))

#plotting 
hist(data$erp_amp)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#filtering out outliers
outliers <- data %>% filter(abs(scale(erp_amp)) > 3)
data = data %>% filter(abs(scale(erp_amp)) <= 3)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#coding and contrasts
data$e <- 0.5
data$e[data$emotion == "fearful"] <- -0.5
data$t <- 0.5
data$t[data$task == "ID"] <- -0.5
data$m <- 0.5
data$m[data$masking == "masked"] <- -0.5

contrasts(data$emotion) <- rbind(-.5, .5)
colnames(contrasts(data$emotion)) <- levels(data$emotion)[2]
contrasts(data$task) <- rbind(-.5, .5)
colnames(contrasts(data$task)) <- levels(data$task)[2]
contrasts(data$masking) <- rbind(-.5, .5)
colnames(contrasts(data$masking)) <- levels(data$masking)[2]

#top-down procedure to establish non-parsimonious model
full_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + e * m * t| subject), data=data, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_full_model <- lmer(erp_amp ~ 1 + e * t * m + (1 + e * m * t || subject), data=data, 
                          control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_full_model)
summary(rePCA(uncorr_full_model))


reduced_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + m * t| subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


#testing model against intercept model
intercept_model <- lmer(erp_amp ~ 1 + e * m * t + (1| subject), data=data, 
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


anova(intercept_model, reduced_model) # reduced_model is better according to AIC

# specifying the final model
EPN_model <- lmer(erp_amp ~ 1 + emotion * task * masking + (1 + masking * task| subject), data=data, 
                 control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(EPN_model)

#checking normality assumptions
qqnorm(resid(EPN_model))
qqline(resid(EPN_model))


#extracting anova
EPN_rand_table <- rand(EPN_model) 
EPN_rand_table

EPN_anova_table <- anova(EPN_model)
EPN_anova_table


EPN_task_contr_table = summary(emmeans(EPN_model, pairwise~ task | masking, combine = T))$contrasts
EPN_task_contr_table

EPN_masking_contr_table = summary(emmeans(EPN_model, pairwise~ masking | task, combine = T))$contrasts
EPN_masking_contr_table

EPN_emmeans <- summary(emmeans(EPN_model, pairwise~emotion | masking, combine = T))$emmeans
EPN_emmeans



###################### N2pc ###################### effects of side, side*masking, masking, task and masking*task

#importing data
data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\N2pc_amps_long_format.xlsx")
data = transform(data, side=factor(side), task=factor(task), masking=factor(masking), subject = factor(subject))

#plotting 
hist(data$erp_amp)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#filtering out outliers
outliers <- data %>% filter(abs(scale(erp_amp)) > 3)
data = data %>% filter(abs(scale(erp_amp)) <= 3)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#coding and contrasts
data$s <- 0.5
data$s[data$side == "ipsi"] <- -0.5
data$t <- 0.5
data$t[data$task == "ID"] <- -0.5
data$m <- 0.5
data$m[data$masking == "masked"] <- -0.5

contrasts(data$side) <- rbind(-.5, .5)
colnames(contrasts(data$side)) <- levels(data$side)[2]
contrasts(data$task) <- rbind(-.5, .5)
colnames(contrasts(data$task)) <- levels(data$task)[2]
contrasts(data$masking) <- rbind(-.5, .5)
colnames(contrasts(data$masking)) <- levels(data$masking)[2]

#top-down procedure to establish non-parsimonious model
full_model <- lmer(erp_amp ~ 1 + s * m * t + (1 + s * m * t| subject), data=data, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_full_model <- lmer(erp_amp ~ 1 + s * t * m + (1 + s * m * t || subject), data=data, 
                          control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_full_model)
summary(rePCA(uncorr_full_model))


reduced_model <- lmer(erp_amp ~ 1 + s * m * t + (1 + m * t | subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


#testing model against intercept model
intercept_model <- lmer(erp_amp ~ 1 + s * m * t + (1| subject), data=data, 
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


anova(intercept_model, reduced_model) # reduced_model is better according to AIC

# specifying the final model
N2pc_model <- lmer(erp_amp ~ 1 + side * task * masking + (1 + masking * task| subject), data=data, 
                 control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(N2pc_model)

#checking normality assumptions
qqnorm(resid(N2pc_model))
qqline(resid(N2pc_model))


#extracting anova
N2pc_rand_table <- rand(N2pc_model) 
N2pc_rand_table

N2pc_anova_table <- anova(N2pc_model)
N2pc_anova_table


N2pc_side_contr_table = summary(emmeans(N2pc_model, pairwise~ side | masking, combine = T))$contrasts
N2pc_side_contr_table

N2pc_side_emmeans <- summary(emmeans(N2pc_model, pairwise~side | masking, combine = T))$emmeans
N2pc_side_emmeans


N2pc_task_contr_table <- summary(emmeans(N2pc_model, pairwise~task| masking, combine = T))$contrasts
N2pc_task_contr_table

N2pc_task_emmeans <- summary(emmeans(N2pc_model, pairwise~task| masking, combine = T))$emmeans
N2pc_task_emmeans



###################### SPCN ###################### effects of masking, task and masking*task

#importing data
data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\SPCN_amps_long_format.xlsx")
data = transform(data, side=factor(side), task=factor(task), masking=factor(masking), subject = factor(subject))

#plotting 
hist(data$erp_amp)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#filtering out outliers
outliers <- data %>% filter(abs(scale(erp_amp)) > 3)
data = data %>% filter(abs(scale(erp_amp)) <= 3)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#coding and contrasts
data$s <- 0.5
data$s[data$side == "ipsi"] <- -0.5
data$t <- 0.5
data$t[data$task == "ID"] <- -0.5
data$m <- 0.5
data$m[data$masking == "masked"] <- -0.5

contrasts(data$side) <- rbind(-.5, .5)
colnames(contrasts(data$side)) <- levels(data$side)[2]
contrasts(data$task) <- rbind(-.5, .5)
colnames(contrasts(data$task)) <- levels(data$task)[2]
contrasts(data$masking) <- rbind(-.5, .5)
colnames(contrasts(data$masking)) <- levels(data$masking)[2]

#top-down procedure to establish non-parsimonious model
full_model <- lmer(erp_amp ~ 1 + s * m * t + (1 + s * m * t| subject), data=data, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_full_model <- lmer(erp_amp ~ 1 + s * t * m + (1 + s * m * t || subject), data=data, 
                          control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_full_model)
summary(rePCA(uncorr_full_model))


reduced_model <- lmer(erp_amp ~ 1 + s * m * t + (1 + m * t | subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


#testing model against intercept model
intercept_model <- lmer(erp_amp ~ 1 + s * m * t + (1| subject), data=data, 
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


anova(intercept_model, reduced_model) # reduced_model is better according to AIC

# specifying the final model
SPCN_model <- lmer(erp_amp ~ 1 + side * task * masking + (1 + masking * task| subject), data=data, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(SPCN_model)

#checking normality assumptions
qqnorm(resid(SPCN_model))
qqline(resid(SPCN_model))


#extracting anova
SPCN_rand_table <- rand(SPCN_model) 
SPCN_rand_table

SPCN_anova_table <- anova(SPCN_model)
SPCN_anova_table


SPCN_side_masking_contr_table = summary(emmeans(SPCN_model, pairwise~ side | masking, combine = T))$contrasts
SPCN_side_masking_contr_table

SPCN_side_masking_emmeans <- summary(emmeans(SPCN_model, pairwise~side | masking, combine = T))$emmeans
SPCN_side_masking_emmeans


SPCN_side_task_contr_table = summary(emmeans(SPCN_model, pairwise~ side | task, combine = T))$contrasts
SPCN_side_task_contr_table

SPCN_side_task_emmeans <- summary(emmeans(SPCN_model, pairwise~side | task, combine = T))$emmeans
SPCN_side_task_emmeans

SPCN_side_contr_table = summary(emmeans(SPCN_model, pairwise~ side | task *masking, combine = T))$contrasts
SPCN_side_contr_table

SPCN_masking_contr_table <- summary(emmeans(SPCN_model, pairwise~masking | side, combine = T))$contrasts
SPCN_masking_contr_table

SPCN_masking_emmeans <- summary(emmeans(SPCN_model, pairwise~masking, combine = T))$emmeans
SPCN_masking_emmeans




###################### P3 ###################### effects of masking, task and masking*task

#importing data
data <- read_excel("D:\\ff_experiment\\Results\\EEG\\comps_long\\P3b_amps_long_format.xlsx")
data = transform(data, emotion=factor(faces), task=factor(task), masking=factor(masking), subject = factor(subject))

#plotting 
hist(data$erp_amp)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#filtering out outliers
outliers <- data %>% filter(abs(scale(erp_amp)) > 3)
data = data %>% filter(abs(scale(erp_amp)) <= 3)
qqnorm(data$erp_amp)
qqline(data$erp_amp)

#coding and contrasts
data$e <- 0.5
data$e[data$emotion == "fearful"] <- -0.5
data$t <- 0.5
data$t[data$task == "ID"] <- -0.5
data$m <- 0.5
data$m[data$masking == "masked"] <- -0.5

contrasts(data$emotion) <- rbind(-.5, .5)
colnames(contrasts(data$emotion)) <- levels(data$emotion)[2]
contrasts(data$task) <- rbind(-.5, .5)
colnames(contrasts(data$task)) <- levels(data$task)[2]
contrasts(data$masking) <- rbind(-.5, .5)
colnames(contrasts(data$masking)) <- levels(data$masking)[2]

#top-down procedure to establish non-parsimonious model
full_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + e * m * t| subject), data=data, 
                   control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


uncorr_full_model <- lmer(erp_amp ~ 1 + e * t * m + (1 + e * m * t || subject), data=data, 
                          control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(uncorr_full_model)
summary(rePCA(uncorr_full_model))


reduced_model <- lmer(erp_amp ~ 1 + e * m * t + (1 + m * t| subject), data=data, 
                      control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


#testing model against intercept model
intercept_model <- lmer(erp_amp ~ 1 + e * m * t + (1| subject), data=data, 
                        control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


anova(intercept_model, reduced_model) # reduced_model is better according to AIC

# specifying the final model
P3_model <- lmer(erp_amp ~ 1 + emotion * task * masking + (1 + masking * task| subject), data=data, 
                 control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(P3_model)

#checking normality assumptions
qqnorm(resid(P3_model))
qqline(resid(P3_model))


#extracting anova
P3_rand_table <- rand(P3_model) 
P3_rand_table

P3_anova_table <- anova(P3_model)
P3_anova_table


P3_emotion_contr_table = summary(emmeans(P3_model, pairwise~ emotion | masking * task, combine = T))$contrasts
P3_emotion_contr_table
p.adjust(P3_emotion_contr_table$p.value, method = 'holm')

P3_emmeans <- summary(emmeans(P3_model, pairwise~emotion | masking * task, combine = T))$emmeans
P3_emmeans

P3_task_contr_table = summary(emmeans(P3_model, pairwise~ task | masking, combine = T))$contrasts
P3_task_contr_table

P3_masking_contr_table = summary(emmeans(P3_model, pairwise~ masking | emotion * task, combine = T))$contrasts
P3_masking_contr_table

P3_task_emmeans <- summary(emmeans(P3_model, pairwise~task|masking, combine = T))$emmeans
P3_task_emmeans

