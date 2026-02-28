########################################
# PO33Q - Determinants of Democracy
# Dr Flo Linke
# Seminar, Week 3
########################################


# PACKAGES
##########

library(tidyverse)

# WORKING DIRECTORY
###################

setwd("")

# LOAD DATA SET
###############

europe <- read.csv("Europe.csv")
world <- read.csv("world.csv")



# PROBIT
########

# data prep

europe2000 <- filter(europe, year==2000)

europe2000$life <- as.numeric(as.character(europe2000$life))


# run the probit model

probit <- glm(democracy ~ life, 
              data = europe2000, 
              family = binomial(link = "probit"))

# model summary
summary(probit)

# Life expectancy at mean

summary(europe2000$life)

setx = data.frame(life=75.43)

# predicting the probability

predict(probit, setx, type="response")

# Life expectancy at minimum

setx = data.frame(life=min(europe2000$life, na.rm = T))

predict(probit, setx, type="response")


# Life expectancy at maximum

setx = data.frame(life=max(europe2000$life, na.rm = T))

predict(probit, setx, type="response")


# HOW TO REPORT RESULTS
#######################

# Filter to year=2000

world2000 <- filter(world, year==2000)


# run the probit model
probit <- glm(democracy ~ gdppc,
              data = world2000, 
              family = binomial(link = "probit"))

# produce a summary of the model

summary(probit)

# if working with Quarto or LaTeX, this is Table 1 from the worksheet

library(modelsummary)
library(tinytable)


models <- list(probit)


cm <- c(
  'gdppc'="GDP per capita",
  '(Intercept)' = 'Constant')


modelsummary(models, 
             gof_omit = 'DF|Deviance|Log.Lik|F|AIC|BIC|RMSE', 
             stars=TRUE,
             coef_map = cm)|>
  group_tt(j = list(" " = 1, "Dependent Variable:<br>Democracy" = 2))

