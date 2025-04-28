#############################################
# PO33Q - WORKSHEET WEEK 3
#############################################


# PACKAGES
##########

library(haven)
library(stargazer)
library(tidyverse)

# WORKING DIRECTORY
###################

setwd("~/OneDrive - University of Warwick/Warwick/Modules/PO33Q/R/Worksheets/WEEK 3")

# LOAD DATA SET
###############

europe <- read_dta("Europe.dta")
world <- read_dta("world.dta")



# PROBIT
########

# data prep

europe2000 <- filter(europe, year==2000)

europe2000$lifeexp <- as.numeric(as.character(europe2000$lifeexp))


# run the probit model

probit <- glm(democracy ~ lifeexp, 
              data = europe2000, 
              family = binomial(link = "probit"))

# model summary
summary(probit)

# Life expectancy at mean

summary(europe2000$lifeexp)

setx = data.frame(lifeexp=75.11)

# predicting the probability

predict(probit, setx, type="response")

# Life expectancy at minimum

setx = data.frame(lifeexp=min(europe2000$lifeexp, na.rm = T))

predict(probit, setx, type="response")


# Life expectancy at maximum

setx = data.frame(lifeexp=max(europe2000$lifeexp, na.rm = T))

predict(probit, setx, type="response")


# HOW TO REPORT RESULTS
#######################

# Filter to year=2000

world2000 <- filter(world, year==2000)

# turn 'gdp_pc' into a numerical variable

world2000$gdp_pc <- as.numeric(as.character(world2000$gdp_pc))


# run the probit model
probit <- glm(democracy ~ gdp_pc,
              data = world2000, 
              family = binomial(link = "probit"))

# produce a summary of the model

summary(probit)

# if working with LaTeX, this is Table 1 from the worksheet

stargazer(probit, 
          header=F, 
          font.size = "scriptsize", 
          omit.stat = c("f", "ser", "ll", "aic"),
          dep.var.labels   = "Democracy",
          covariate.labels = c("per capita GDP"),
          report=('vc*p'))
