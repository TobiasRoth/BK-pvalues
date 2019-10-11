rm(list = ls())

#------------------------------------------------------------------------------------------------------
# Settings
#------------------------------------------------------------------------------------------------------
# Libraries
library(readxl)
library(tidyverse)
library(arm)

# Daten einlesen
dat <- read_xlsx("Data/Masterfile R.xlsx") %>% 
  filter(!is.na(date))

# Plausibilitätskontrolle
sum(table(dat$ID_person) == 10)
apply(table(dat$ID_person, dat$CI), 2, function(x) sum(x == 5))

# Richtige Antworten?
mean(apply(dat %>% dplyr::select(length_CI2, length_CI1), 1, function(x) which(x == min(x)) - 1) == dat$say_left)

#------------------------------------------------------------------------------------------------------
# Auswertung
#------------------------------------------------------------------------------------------------------
LM linear model
GLM generalized linear model
GLMM generalized linear mixed model

glmer(say_correct ~ p_value_shown + language + (1|location) + (1|ID_person), family = binomial, data = dat) %>% 
  summary

glmer(say_correct ~ p_value_shown * experience + (1|location) + (1|ID_person), family = binomial, 
      data = dat %>% filter(language == "e")) %>% 
  summary


10 years exp mit p-Wert
plogis(0.81853 - 0.35819) 
plogis(0.81853 + 3 * 0.25257)
plogis(0.81853 + 0.1385) 
plogis(0.81853 + 0.1385 + 3 * 0.25257 + 3 * -0.25932)

tapply(dat$say_correct, dat$p_value_shown, mean)

glmer

lm(y ~ x + z, data = dat)
summary(glmer(say_correct ~ experience * p_value_shown + (1|ID_person), family = binomial, data = dat))

summary(glmer(say_lower_pvalue ~ experience * p_value_shown + (1|ID_person), family = binomial, data = dat))


plogis(0.27549)
plogis(0.27549 + 0.34693)

tt <- dat %>% filter(experience == 0 & p_value_shown == 0)
mean(tt$say_correct)
tt <- dat %>% filter(experience == 0 & p_value_shown == 1)
mean(tt$say_correct)


dat$say_lower_pvalue
# 





