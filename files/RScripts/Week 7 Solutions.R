#####################
# WORKSHEET WEEK 7
#####################


# WORKING DIRECTORY
#####################

setwd()


# PACKAGES
############

library(haven)
library(tidyverse)



# Import Data
#############

prz <- read_dta("prz.dta")



# Exercise 1
#####################


democracy <- glm(democ ~ g + gdpw + oil, 
                   data = prz,
                   na.action = na.exclude,
                   family = binomial(link = "probit"))




summary(democracy) #detailed results
nobs(democracy) # number of observations


## >> What is possibly wrong? Democracy needs to develop and thus the effect of growth will only unfold later.
## >> Interpretation: Wealth increases the probability of democracy. Oil reduces the probability of democracy. Growth is insignificant


# Exercise 2
#####################


prz <- prz %>%
  group_by(country) %>%
  mutate(l.democ = lag(democ)) %>%
  ungroup()

prz_lag <- select(prz, country, year, democ, l.democ) # in case you want to have a look at how the lagged the original value of 'democracy' look next to each other

dynamic <- glm(democ ~ g + gdpw + oil + l.democ, 
                 data = prz,
                 na.action = na.exclude,
                 family = binomial(link = "probit")) 

summary(dynamic)
nobs(dynamic)

## >> by including a lagged variable we assume a time dependency between last year's condition of democracy and the following year
  
  
  
# Exercise 3
#####################
  
prz <- prz %>%
  group_by(country) %>%
  mutate(l.g = lag(g)) %>%
  ungroup()

prz <- prz %>%
  group_by(country) %>%
  mutate(l.gdpw = lag(gdpw)) %>%
  ungroup()

prz <- prz %>%
  group_by(country) %>%
  mutate(l.oil = lag(oil)) %>%
  ungroup()
  
prz_democ0 <- filter(prz, l.democ==0) 

emergence <- glm(democ ~ l.gdpw + l.g + l.oil, 
                   data = prz_democ0,
                   na.action = na.exclude,
                   family = binomial(link = "probit"))

summary(emergence)
nobs(emergence)

## >> growth: if economy grows, democracy is less likely to emerge
## >> wealth: wealth increases the probability of transition to democracy
## >> oil: Oil makes democracy less likely to emerge
  
  
# Exercise 4
#####################

prz_democ1 <- filter(prz, l.democ==1)
  
survive <- glm(democ ~ l.gdpw + l.g + l.oil, 
                 data = prz_democ1,
                 na.action = na.exclude,
                 family = binomial(link = "probit"))

summary(survive)
nobs(survive)

## >>oil is insignificant
## >> growth and wealth both increase the probability of a democracy to survive
  
  
# Exercise 5
#####################

prz$l.democgdpw <- prz$l.democ * prz$l.gdpw
prz$l.democg <- prz$l.democ * prz$l.g
prz$l.democoil <- prz$l.democ * prz$l.oil
  
  
joint <- glm(democ ~ l.gdpw + l.g + l.oil + l.democ + l.democgdpw + l.democg + l.democoil, 
               data = prz,
               na.action = na.exclude,
               family = binomial(link = "probit"))

summary(joint)
nobs(joint)

## >> x coefficients are equal to the "onset-model" while the sum of lagged x coefficients and x coefficients are equal to the x coefficients of the transition model

## >> for example for wealth: .000057+.0000263  = .0000833

## >> coefficients of interacted variables can only be interpreted if democracy existed the previous year because the lagged dependent variable is significant in the full model, the coefficients are significantly different from each other
  


# WALD TEST FOR JOINT ESTMATION
###############################

# http://r-survey.r-forge.r-project.org/survey/html/regTermTest.html

library(survey)
regTermTest(joint, ~ l.gdpw + l.democgdpw, method="Wald")




# Exercise 6
#####################

library(pROC)

# Democracy model: 

prob_democracy <- predict(democracy, type="response")
prz$prob_democracy <- unlist(prob_democracy)

roc_democracy <- roc(prz$democ, prz$prob_democracy)

plot(roc_democracy, print.auc=TRUE)


# Dynamic Model

prob_dynamic <- predict(dynamic, type="response")
prz$prob_dynamic <- unlist(prob_dynamic)

roc_dynamic <- roc(prz$democ, prz$prob_dynamic)

plot(roc_dynamic, print.auc=TRUE)


# emergence model
prob_emergence <- predict(emergence, type="response")
prz_democ0$prob_emergence <- unlist(prob_emergence)

roc_emergence <- roc(prz_democ0$democ, prz_democ0$prob_emergence)


# survive model

prob_survive <- predict(survive, type="response")
prz_democ1$prob_survive <- unlist(prob_survive)

roc_survive <- roc(prz_democ1$democ, prz_democ1$prob_survive)


# Joint Model
  
  
prob_joint <- predict(joint, type="response")
prz$prob_joint <- unlist(prob_joint)

roc_joint <- roc(prz$democ, prz$prob_joint)

plot(roc_joint, print.auc=TRUE)




  
  