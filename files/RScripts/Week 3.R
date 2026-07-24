#########################################################################################
# PO33Q - The Life and Death of Democracies and Dictatorships: A Quantitative Perspective
# Dr Flo Linke
# Seminar, Week 3
#########################################################################################

# -----------------------
# Load Required Packages
# -----------------------

library(tidyverse)
library(modelsummary)
library(tinytable)

# -----------------------
# set working directory
# -----------------------

setwd("")

# -----------------------
# Load data
# -----------------------

europe <- read.csv("Europe.csv")
world <- read.csv("world.csv")


###############################################
# Probit Model: Europe
###############################################

# -----------------------
# Data prep
# -----------------------

europe2000 <- filter(europe, year==2000)
europe2000$life <- as.numeric(as.character(europe2000$life))

# -----------------------
# Estimate probit model
# -----------------------

probit <- glm(democracy ~ life, 
              data = europe2000, 
              family = binomial(link = "probit"))

summary(probit)


# ------------------------------------------
# Predicted probabilities at key values
# ------------------------------------------

# Life expectancy at mean
summary(europe2000$life)
setx = data.frame(life=75.43)
predict(probit, setx, type="response")

# Life expectancy at minimum
setx = data.frame(life=min(europe2000$life, na.rm = T))
predict(probit, setx, type="response")

# Life expectancy at maximum
setx = data.frame(life=max(europe2000$life, na.rm = T))
predict(probit, setx, type="response")


###############################################
# Reporting Results: World
###############################################

# -----------------------
# Filter to year=2000
# -----------------------

world2000 <- filter(world, year==2000)

# -----------------------
# Estimate probit model
# -----------------------

probit <- glm(democracy ~ gdppc,
              data = world2000, 
              family = binomial(link = "probit"))

summary(probit)

# ------------------------------------------
# modelsummary Table (for Quarto or LaTeX)
# ------------------------------------------

# this is Table 1 from the worksheet

models <- list(probit)

cm <- c(
  'gdppc'="GDP per capita",
  '(Intercept)' = 'Constant')

modelsummary(models, 
             gof_omit = 'DF|Deviance|Log.Lik|F|AIC|BIC|RMSE', 
             stars=TRUE,
             coef_map = cm)|>
  group_tt(j = list(" " = 1, "Dependent Variable:<br>Democracy" = 2))