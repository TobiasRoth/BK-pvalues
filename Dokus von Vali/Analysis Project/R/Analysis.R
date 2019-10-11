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
mean(apply(dat %>% select(length_CI2, length_CI1), 1, function(x) which(x == min(x)) - 1) == dat$say_left)

#------------------------------------------------------------------------------------------------------
# Auswertung
#------------------------------------------------------------------------------------------------------

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





