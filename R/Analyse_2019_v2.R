rm(list = ls())

#------------------------------------------------------------------------------------------------------
# Settings and read data
#------------------------------------------------------------------------------------------------------
# Libraries
library(readxl)
library(tidyverse)
library(arm)

# Daten einlesen f?r Tobias
dat18 <- read_xlsx("Data/Masterfile2018.xlsx") %>% 
  filter(!is.na(date))
dat19 <- read_xlsx("Data/Masterfile2019.xlsx") %>% 
  filter(!is.na(date))

#Daten einlesen f?r Estelle
dat18 <- read_xlsx("Data files/Masterfile2018.xlsx") %>% 
  filter(!is.na(date))
dat19 <- read_xlsx("Data files/Masterfile2019.xlsx") %>% 
  filter(!is.na(date))

#------------------------------------------------------------------------------------------------------
# Small tests
#------------------------------------------------------------------------------------------------------

#Check if Masterfile is okay
summary(dat19)

#Check the number of participants
sum(table(dat19$ID_person) == 10)

#Check if every person had 5 boxplot and 5 CI
table(dat19$ID_person, dat19$CI)

#Compare the experience between 2018 and 2019
table(dat18$experience)
table(dat19$experience)

#Proportion of correct answers in 2018 and 2019
mean(dat18$say_correct)
mean(dat19$say_correct)

#Number of men and women in 2018 and 2019
table(dat18$gender)
table(dat19$gender)

#Number of subjects interrogated for each observer
table(dat18$observer)
table(dat19$observer)

#Representation of the experience (Person x10)
hist(dat19$experience)
plot(table(dat19$experience))
plot(table(dat18$experience))

#Representation of the confidence
plot(table(dat19$confidence))

#------------------------------------------------------------------------------------------------------
# Group the data
#------------------------------------------------------------------------------------------------------
#Group data of 2018
groupdat18 <- dat18 %>% 
  mutate(confidence = NA) %>% 
  group_by(ID_person, experience, confidence, p_value_shown, gender, observer) %>% 
  summarise(
    Anz = n(),
    Richtig = mean(say_correct),
    Jahr = 2018
  )

#Group data of 2019
groupdat19 <- dat19 %>% 
  group_by(ID_person, experience, confidence, p_value_shown, gender, observer) %>% 
  summarise(
    Anz = n(),
    Richtig = mean(say_correct),
    Jahr = 2019
)

#Group 2018 and 2019 together
dattot <- rbind(groupdat18, groupdat19)
dattot$ID <- paste(dattot$ID_person, dattot$Jahr, sep = "_")
dattot$ID <-1:nrow(dattot)


#------------------------------------------------------------------------------------------------------
# Differences between gender
#------------------------------------------------------------------------------------------------------
#Number of correct answers per gender in 2018
table(dat18$say_correct, dat18$gender)

#Proportion of correct answers per gender in 2018
correct18 <- tapply(X = dat18$say_correct, INDEX= dat18$gender, FUN = mean) 
correct18

#Number of correct answers per gender in 2019
table(dat19$say_correct, dat19$gender)

#Proportion of correct answers per gender in 2019
correct19 <- tapply(X = dat19$say_correct, INDEX= dat19$gender, FUN = mean) 
correct19

#------------------------------------------------------------------------------------------------------
# Years with statistical experience
#------------------------------------------------------------------------------------------------------
#2018
plot(table(groupdat18$experience), ylab = "Number of persons", 
     xlab="Years with statistical experience", 
     main="2018",
     ylim = c(0,60), xlim = c(0,30))

#2019
plot(table(groupdat19$experience), ylab = "Number of persons", 
     xlab="Years with statistical experience", 
     main="2019",
     ylim = c(0,60), xlim = c(0,30))


#------------------------------------------------------------------------------------------------------
# Confidence
#------------------------------------------------------------------------------------------------------
#Normal plot witht dots
plot(groupdat19$experience, groupdat19$confidence)

#Correlation test
cor.test(groupdat19$experience, groupdat19$confidence)

# Trendlinie mit einem Smoother
ggplot(groupdat19, aes(x = experience, y = confidence)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Years with statistical experience", y = "Confidence",
       title = "Confidence according to experience")

# Trendlinie mit einem Linearen Modell
ggplot(groupdat19, aes(x = experience, y = confidence)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = "Years with statistical experience", y = "Confidence",
       title = "Confidence according to experience")

#Group 6 and + years together
groupdat19$experience_Kat <- as.factor(groupdat19$experience)
groupdat19$experience_Kat[groupdat19$experience >=  6] <- 6

#Boxplot of confidence
ggplot(groupdat19, aes(x = experience_Kat,  y = confidence)) +
  geom_boxplot() +
  labs(x = "Years with statistical experience", y = "Confidence",
       title = "Confidence according to experience")

#Violin plot of confidence
ggplot(groupdat19, aes(x = experience_Kat,  y = confidence)) +
  geom_violin() + 
  ylim(0, 10) +
  labs(x = "Years with statistical experience", y = "Confidence",
       title = "Confidence according to experience")

#------------------------------------------------------------------------------------------------------
# Figure: Deceived by p-values (BP+ CI)
#------------------------------------------------------------------------------------------------------
#Model 2018
mod1 <- glmer(say_correct ~ log(experience + 1) * p_value_shown + (1|ID_person), 
                    family = binomial, data = dat18)
summary(mod1)

nsim <- 100
newdat <- expand.grid(experience = seq(0, 5, 0.1), pvalue = c(0, 1))
simres <- sim(mod1, nsim)@fixef
tmpres <- array(NA, dim = c(nrow(newdat), nsim))
for(x in 1:nsim) {
  tmpres[, x] <- plogis(simres[x, 1] + 
                          simres[x, 2] * newdat$experience + 
                          simres[x, 3] * newdat$pvalue + 
                          simres[x, 4] * newdat$experience * newdat$pvalue)
}
newdat$mean <- apply(tmpres, 1, mean)
newdat$low <- apply(tmpres, 1, quantile, probs = 0.025)
newdat$up <- apply(tmpres, 1, quantile, probs = 0.975)
newdat$pvaluel <- "no p-values presented"
newdat$pvaluel[newdat$pvalue == 1] <- "p-values presented"
ggplot(newdat) +  ylim(0, 1) +
  aes(experience, mean, fill = pvaluel) +
  geom_ribbon(aes(ymin = low, ymax = up), alpha = 0.3) +
  geom_line(aes(colour = pvaluel), lwd = 1.5) +
  labs(x = "Years with statistical experience", y = "Proportion of correct answers",
       title = "2018 glmer with interaction") +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom")


#Model 2019
mod2 <- glmer(say_correct ~ log(experience + 1) * p_value_shown + (1|ID_person), 
                    family = binomial, data = dat19)
summary(mod2)

nsim <- 100
newdat <- expand.grid(experience = seq(0, 5, 0.1), pvalue = c(0, 1))
simres <- sim(mod2, nsim)@fixef
tmpres <- array(NA, dim = c(nrow(newdat), nsim))
for(x in 1:nsim) {
  tmpres[, x] <- plogis(simres[x, 1] + 
                          simres[x, 2] * newdat$experience + 
                          simres[x, 3] * newdat$pvalue + 
                          simres[x, 4] * newdat$experience * newdat$pvalue)
}
newdat$mean <- apply(tmpres, 1, mean)
newdat$low <- apply(tmpres, 1, quantile, probs = 0.025)
newdat$up <- apply(tmpres, 1, quantile, probs = 0.975)
newdat$pvaluel <- "no p-values presented"
newdat$pvaluel[newdat$pvalue == 1] <- "p-values presented"
ggplot(newdat) +  ylim(0, 1) +
  aes(experience, mean, fill = pvaluel) +
  geom_ribbon(aes(ymin = low, ymax = up), alpha = 0.3) +
  geom_line(aes(colour = pvaluel), lwd = 1.5) +
  labs(x = "Years with statistical experience", y = "Proportion of correct answers",
       title = "2019 glmer with interaction") +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom")

#Model without interaction 2019
mod3 <- glmer(say_correct ~ log(experience + 1) + p_value_shown + (1|ID_person), 
                    family = binomial, data = dat19)
summary(mod3)


#Lm with interaction 2018
mod4 <- lm(Richtig ~ log(experience + 1) * p_value_shown, data = groupdat18)
summary(mod4)

#Lm with interaction 2019
mod5 <- lm(Richtig ~ log(experience + 1) * p_value_shown, data = groupdat19)
summary(mod5)

#Lm without interaction 2019
mod6 <- lm(Richtig ~ log(experience + 1) + p_value_shown, data = groupdat19)
summary(mod6)

#Lm with interaction for total
dattot$yr <- dattot$Jahr - 2018
mod7 <- lm(Richtig ~ log(experience + 1) * p_value_shown + yr + gender, data = dattot)
summary(mod7)

#Lm with gender interaction in total
mod8 <- lm(Richtig ~ p_value_shown * gender, data = dattot)
summary(mod8)


#------------------------------------------------------------------------------------------------------
# Figure: Deceived by p-values (BP+ CI)
#------------------------------------------------------------------------------------------------------
dat18CI <- subset(dat18, CI == 1)

mod9 <- glmer(say_correct ~ log(experience + 1) * p_value_shown + (1|ID_person), 
              family = binomial, data = dat18CI)
summary(mod9)

nsim <- 100
newdat <- expand.grid(experience = seq(0, 5, 0.1), pvalue = c(0, 1))
simres <- sim(mod9, nsim)@fixef
tmpres <- array(NA, dim = c(nrow(newdat), nsim))
for(x in 1:nsim) {
  tmpres[, x] <- plogis(simres[x, 1] + 
                          simres[x, 2] * newdat$experience + 
                          simres[x, 3] * newdat$pvalue + 
                          simres[x, 4] * newdat$experience * newdat$pvalue)
}
newdat$mean <- apply(tmpres, 1, mean)
newdat$low <- apply(tmpres, 1, quantile, probs = 0.025)
newdat$up <- apply(tmpres, 1, quantile, probs = 0.975)
newdat$pvaluel <- "no p-values presented"
newdat$pvaluel[newdat$pvalue == 1] <- "p-values presented"
ggplot(newdat) +  ylim(0, 1) +
  aes(experience, mean, fill = pvaluel) +
  geom_ribbon(aes(ymin = low, ymax = up), alpha = 0.3) +
  geom_line(aes(colour = pvaluel), lwd = 1.5) +
  labs(x = "Years with statistical experience", y = "Proportion of correct answers",
       title = "2018 glmer with interaction only CI") +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom")


#Model 2019
dat19CI <- subset(dat19, CI == 1)

mod10 <- glmer(say_correct ~ log(experience + 1) * p_value_shown + (1|ID_person), 
              family = binomial, data = dat19CI)
summary(mod2)

nsim <- 100
newdat <- expand.grid(experience = seq(0, 5, 0.1), pvalue = c(0, 1))
simres <- sim(mod10, nsim)@fixef
tmpres <- array(NA, dim = c(nrow(newdat), nsim))
for(x in 1:nsim) {
  tmpres[, x] <- plogis(simres[x, 1] + 
                          simres[x, 2] * newdat$experience + 
                          simres[x, 3] * newdat$pvalue + 
                          simres[x, 4] * newdat$experience * newdat$pvalue)
}
newdat$mean <- apply(tmpres, 1, mean)
newdat$low <- apply(tmpres, 1, quantile, probs = 0.025)
newdat$up <- apply(tmpres, 1, quantile, probs = 0.975)
newdat$pvaluel <- "no p-values presented"
newdat$pvaluel[newdat$pvalue == 1] <- "p-values presented"
ggplot(newdat) +  ylim(0, 1) +
  aes(experience, mean, fill = pvaluel) +
  geom_ribbon(aes(ymin = low, ymax = up), alpha = 0.3) +
  geom_line(aes(colour = pvaluel), lwd = 1.5) +
  labs(x = "Years with statistical experience", y = "Proportion of correct answers",
       title = "2019 glmer with interaction only CI") +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom")

#------------------------------------------------------------------------------------------------------
# Raphi-effect
#------------------------------------------------------------------------------------------------------
dattot$observer_sex <- 0
dattot$observer_sex[dattot$observer == 0 & dattot$Jahr == 2018] <- 1
table(dattot$observer_sex, dattot$Jahr)

mod <- lm(Richtig ~ log(experience + 1) * p_value_shown + observer_sex, data = dattot)
summary(mod)
