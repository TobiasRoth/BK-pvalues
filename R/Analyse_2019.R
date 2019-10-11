rm(list = ls())

#------------------------------------------------------------------------------------------------------
# Settings and read data
#------------------------------------------------------------------------------------------------------
# Libraries
library(readxl)
library(tidyverse)
library(arm)

# Daten einlesen
dat18 <- read_xlsx("Data/Masterfile2018.xlsx") %>% 
  filter(!is.na(date))
dat19 <- read_xlsx("Data/Masterfile2019.xlsx") %>% 
  filter(!is.na(date))

#------------------------------------------------------------------------------------------------------
# Small tests
#------------------------------------------------------------------------------------------------------
summary(dat19)
sum(table(dat19$ID_person) == 10)
table(dat19$ID_person, dat19$CI)

table(dat18$experience)
table(dat19$experience)

mean(dat18$say_correct)
mean(dat19$say_correct)

table(dat18$gender)
table(dat19$gender)

table(dat18$observer)
table(dat19$observer)

hist(dat19$experience)
plot(table(dat19$experience))

plot(table(dat19$confidence))

#------------------------------------------------------------------------------------------------------
# Small tests
#------------------------------------------------------------------------------------------------------
groupdat18 <- dat18 %>% 
  mutate(confidence = NA) %>% 
  group_by(ID_person, experience, confidence, p_value_shown, gender) %>% 
  summarise(
    Anz = n(),
    Richtig = mean(say_correct),
    Jahr = 2018
  )

groupdat19 <- dat19 %>% 
  group_by(ID_person, experience, confidence, p_value_shown, gender) %>% 
  summarise(
    Anz = n(),
    Richtig = mean(say_correct),
    Jahr = 2019
)

dattot <- rbind(groupdat18, groupdat19)
dattot$ID <- paste(dattot$ID_person, dattot$Jahr, sep = "_")
dattot$ID <-1:nrow(dattot)


plot(table(groupdat19$experience), ylab = "Anzahl Personen")
plot(table(groupdat19$confidence))
plot(table(dattot$experience, dattot$Jahr), ylab = "Anzahl Personen")

par(mfrow = c(1,2))
plot(table(groupdat18$experience), ylab = "Anzahl Personen", ylim = c(0,60), xlim = c(0,30))
plot(table(groupdat19$experience), ylab = "Anzahl Personen", ylim = c(0,60), xlim = c(0,30))


#------------------------------------------------------------------------------------------------------
# 
#------------------------------------------------------------------------------------------------------
plot(groupdat19$experience, groupdat19$confidence)
cor.test(groupdat19$experience, groupdat19$confidence)

# Trendlinie mit einem Smoother
ggplot(groupdat19, aes(x = experience, y = confidence)) +
  geom_point() +
  geom_smooth() 

# Trendlinie mit einem Linearen Modell
ggplot(groupdat19, aes(x = experience, y = confidence)) +
  geom_point() +
  geom_smooth(method = lm) 

#
groupdat19$experience_Kat <- as.factor(groupdat19$experience)
groupdat19$experience_Kat[groupdat19$experience >=  4] <- 4
ggplot(groupdat19, aes(x = experience_Kat,  y = confidence)) +
  geom_boxplot() 
ggplot(groupdat19, aes(x = experience_Kat,  y = confidence)) +
  geom_violin() + 
  ylim(0, 10) +
  labs(x = "Years with statistical experience",
       title = "(A) Titel")

#------------------------------------------------------------------------------------------------------
# Figure: Deceived by p-values
#------------------------------------------------------------------------------------------------------
mod <- glmer(say_correct ~ log(experience + 1) * p_value_shown + (1|ID_person), 
                    family = binomial, data = dat18)

mod <- glmer(say_correct ~ log(experience + 1) * p_value_shown + (1|ID_person), 
                    family = binomial, data = dat19)

mod <- glmer(say_correct ~ log(experience + 1) + p_value_shown + (1|ID_person), 
                    family = binomial, data = dat19)
summary(mod)


mod <- lm(Richtig ~ log(experience + 1) * p_value_shown, data = groupdat18)
summary(mod)

mod <- lm(Richtig ~ log(experience + 1) * p_value_shown, data = groupdat19)
summary(mod)

dattot$yr <- dattot$Jahr - 2018
mod <- lm(Richtig ~ log(experience + 1) * p_value_shown + yr + gender, data = dattot)
summary(mod)

mod <- lm(Richtig ~ log(experience + 1) * p_value_shown + yr + gender, data = dattot)
summary(mod)

mod <- lm(Richtig ~ p_value_shown * gender, data = dattot)
summary(mod)


nsim <- 100
newdat <- expand.grid(experience = seq(0, 5, 0.1), pvalue = c(0, 1))
simres <- sim(mod, nsim)@fixef
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
  labs(x = "Years with statistical experience", y = "Proportion of correct answers") +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom")  
