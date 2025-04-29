###############################################################
# PO33Q - Week 7, Stargazer Code
###############################################################


setwd()

# PACKAGES
############

library(haven)
library(tidyverse)
library(stargazer)


# Import Data
#############

prz <- read_dta("prz.dta")



# Exercise 1
#####################


democracy <- glm(democ ~ g + gdpw + oil, 
                 data = prz,
                 na.action = na.exclude,
                 family = binomial(link = "probit"))


# Exercise 2
#####################


prz <- prz %>%
  group_by(country) %>%
  mutate(l.democ = lag(democ)) %>%
  ungroup()



dynamic <- glm(democ ~ g + gdpw + oil + l.democ, 
               data = prz,
               na.action = na.exclude,
               family = binomial(link = "probit")) 


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

emergence <- glm(democ ~ l.g + l.gdpw + l.oil, 
                 data = prz_democ0,
                 na.action = na.exclude,
                 family = binomial(link = "probit"))


# Exercise 4
#####################

prz_democ1 <- filter(prz, l.democ==1)

survive <- glm(democ ~ l.g + l.gdpw + l.oil, 
               data = prz_democ1,
               na.action = na.exclude,
               family = binomial(link = "probit"))


# Exercise 5
#####################

prz$l.democgdpw <- prz$l.democ * prz$l.gdpw
prz$l.democg <- prz$l.democ * prz$l.g
prz$l.democoil <- prz$l.democ * prz$l.oil


joint <- glm(democ ~ l.g + l.gdpw + l.oil + l.democ + l.democg + l.democgdpw + l.democoil, 
             data = prz,
             na.action = na.exclude,
             family = binomial(link = "probit"))

# Exercise 6
#####################

library(pROC)

# Democracy model: 

prob_democracy <- predict(democracy, type="response")
prz$prob_democracy <- unlist(prob_democracy)

roc_democracy <- roc(prz$democ, prz$prob_democracy)


# Dynamic Model

prob_dynamic <- predict(dynamic, type="response")
prz$prob_dynamic <- unlist(prob_dynamic)

roc_dynamic <- roc(prz$democ, prz$prob_dynamic)

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

# stargazer table

stargazer(democracy, dynamic, emergence, survive, joint,
          header=F, 
          font.size = "small", 
          covariate.labels = c("Growth rate",
                               "GDP per Worker",
                               "Oil Producer",
                               "Lagged Growth rate",
                               "Lagged GDP per Worker",
                               "Lagged Oil Producer",
                               "Growth*L.Democracy",
                               "GDP*L.Democracy",
                               "Oil*L.Democracy",
                               "Lagged Democracy"),
          order=c(1,2,3,8,9,10,5,6,7,4),
          dep.var.labels   = "Democracy",
          column.labels = c("Probit", "Probit (lagged)", "Emergence", "Survival", "Full Interaction"),
          omit.stat = c("aic", "ll"),
          add.lines = list(c("ROC Curve", round(auc(roc_democracy),2), 
                             round(auc(roc_dynamic),2), 
                             round(auc(roc_emergence),2), 
                             round(auc(roc_survive),2), 
                             round(auc(roc_joint),2))),
          title = "Results, Worksheet Week 7")