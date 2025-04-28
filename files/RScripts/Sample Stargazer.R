#################################################
# Example stargazer Code
#################################################

# set working directory

setwd()

# load packages

library(tidyverse)
library(stargazer)

# load daat set

wdi <- read.csv("world.csv")
wdi_2015 <- filter(wdi, year==2015)

# Run Regression Models

wdi_life <- lm(gdppc ~ lifeexp, data = wdi_2015)

wdi_multi <- lm(gdppc ~ lifeexp + urban, data = wdi_2015)


# Set Table

stargazer(wdi_life,                                # this is the object in which the regression results are stored
          header=F,                                # suppress the header
          omit.stat = c("f", "ser"),               # suppress f statistic and residual standard errors
          dep.var.labels   = "per capita GDP",     # label the dependent variable
          covariate.labels = c("Life Expectancy"), # label the independent variable
          out="documentname.doc",                  # this is the document in which the table will be stored
          type="html")                             # this ensures a workable table format for word


# Set Table to dsplay multiple models 

stargazer(wdi_life,wdi_multi,                                      # multiple regression objects
          header=F,                                                # suppress the header
          omit.stat = c("f", "ser"),                               # suppress f statistic and residual standard errors
          dep.var.labels   = "per capita GDP",                     # label the dependent variable
          covariate.labels = c("Life Expectancy", "Urbanisation"), # IV labels in the order in which they appear in the models
          out="documentname.doc",                                  # this is the document in which the table will be stored
          type="html")                                             # this ensures a workable table format for word