###########################
# PO3B3 - Exercises WEEK 5
###########################


# WORKING DIRECTORY
#####################

setwd()


# PACKAGES
############

library(tidyverse)



# Import Data
#############

wvs <- read.csv("wvs.csv")




# DESCRIPTIVES
# What is the average value for survival/self-expression values? 
# What does this mean?
###############################################################

summary(wvs$surv_self)



# Filtering through Waves, how has this average changed?
###############################################################

wvs1 <- filter(wvs, wvs_wave==1)
summary(wvs1$surv_self)

wvs2 <- filter(wvs, wvs_wave==2)
summary(wvs2$surv_self)

wvs3 <- filter(wvs, wvs_wave==3)
summary(wvs3$surv_self)

wvs4 <- filter(wvs, wvs_wave==4)
summary(wvs4$surv_self)

wvs5 <- filter(wvs, wvs_wave==5)
summary(wvs5$surv_self)




# Repeat these two steps with traditional/rational values.
###############################################################

summary(wvs$traditional_rational) 
  
summary(wvs1$traditional_rational) 

summary(wvs2$traditional_rational) 

summary(wvs3$traditional_rational) 

summary(wvs4$traditional_rational) 

summary(wvs5$traditional_rational) 




# Why is the comparison of these values over time difficult?
###############################################################

# >> different countries in the sample


# LINEAR REGRESSION
#For which waves do traditional–rational values in the population explain a country’s level of democracy?
###############################################################

reg_wvs1 <- lm(polity ~ traditional_rational, data=wvs1)
summary(reg_wvs1)

reg_wvs2 <- lm(polity ~ traditional_rational, data=wvs2)
summary(reg_wvs2)

reg_wvs3 <- lm(polity ~ traditional_rational, data=wvs3)
summary(reg_wvs3)

reg_wvs4 <- lm(polity ~ traditional_rational, data=wvs4)
summary(reg_wvs4)

reg_wvs5 <- lm(polity ~ traditional_rational, data=wvs5)
summary(reg_wvs5)


# >>> waves 4 and 5

# For which waves do survival–self-expression values in the population explain a country’s level of democracy?
###############################################################


reg_wvs1_ss <- lm(polity ~ surv_self, data=wvs1)
summary(reg_wvs1_ss)

reg_wvs2_ss <- lm(polity ~ surv_self, data=wvs2)
summary(reg_wvs2_ss)

reg_wvs3_ss <- lm(polity ~ surv_self, data=wvs3)
summary(reg_wvs3_ss)

reg_wvs4_ss <- lm(polity ~ surv_self, data=wvs4)
summary(reg_wvs4_ss)

reg_wvs5_ss <- lm(polity ~ surv_self, data=wvs5)
summary(reg_wvs5_ss)


# >>>  waves 3, 4, and 5



# Identify reasons why earlier waves fail to explain the level of democracy.
###############################################################

# >> very low number of observations



# Assessed jointly, how much more or less democratic do survival/self-expression value and GDP growth make countries in wave 5 (2005-2009)?
###############################################################

reg_wvs5_joint <- lm(polity ~ gdp_growth + surv_self, data=wvs5)
summary(reg_wvs5_joint)

## >> ceteris paribus, for every additional 1% annual growth in GDP, the level of democracy drops by 0.68 points on the Polity IV scale on average

## >> ceteris paribus, for an increase in survival vs. self expression values by 1 unit, the Polity IV scale increases by 5.06 units on average 

  
  
  
# PROBIT
# For which waves do traditional–rational values in the  population explain a country’s probability to be a democracy?
###############################################################

probit_wvs1 <- glm(democracy ~ traditional_rational, 
                     data = wvs1, 
                     family = binomial(link = "probit"))

summary(probit_wvs1)


## >>>the p-values are huge here, due to perfect predictions, see below

probit_wvs2 <- glm(democracy ~ traditional_rational, 
                   data = wvs2, 
                   family = binomial(link = "probit"))

summary(probit_wvs2)


probit_wvs3 <- glm(democracy ~ traditional_rational, 
                   data = wvs3, 
                   family = binomial(link = "probit"))

summary(probit_wvs3)



probit_wvs4 <- glm(democracy ~ traditional_rational, 
                   data = wvs4, 
                   family = binomial(link = "probit"))

summary(probit_wvs4)


probit_wvs5 <- glm(democracy ~ traditional_rational, 
                   data = wvs5, 
                   family = binomial(link = "probit"))

summary(probit_wvs5)

## >> waves 3 and 5; wave 1 is too small, hence the perfect prediction


## For which waves do survival–self-expression values in the population explain a country’s probability to be a democracy?
###############################################################

probit_wvs1_ss <- glm(democracy ~ surv_self, 
                        data = wvs1, 
                        family = binomial(link = "probit"))

summary(probit_wvs1_ss)


probit_wvs2_ss <- glm(democracy ~ surv_self, 
                      data = wvs2, 
                      family = binomial(link = "probit"))

summary(probit_wvs2_ss)


probit_wvs3_ss <- glm(democracy ~ surv_self, 
                      data = wvs3, 
                      family = binomial(link = "probit"))

summary(probit_wvs3_ss)


probit_wvs4_ss <- glm(democracy ~ surv_self, 
                      data = wvs4, 
                      family = binomial(link = "probit"))

summary(probit_wvs4_ss)


probit_wvs5_ss <- glm(democracy ~ surv_self, 
                      data = wvs5, 
                      family = binomial(link = "probit"))

summary(probit_wvs5_ss)


## >> waves 3, 4, and 5

## Identify reasons why earlier waves fail to explain regime type.
###############################################################

## >> low number of observations



## Calculate a model assessing the impact of GDP growth on the probability of democracy for countries in wave 5.
###############################################################

probit_wvs5_gdp <- glm(democracy ~ gdp_growth, 
                         data = wvs5, 
                         family = binomial(link = "probit"))

summary(probit_wvs5_gdp)



## How much less likely is a country to be a democracy moving  from minimum GDP growth to maximum GDP growth in wave 5?
###############################################################

min = data.frame(gdp_growth = min(wvs5$gdp_growth, na.rm = T))

predict(probit_wvs5_gdp, min, type="response")

max = data.frame(gdp_growth = max(wvs5$gdp_growth, na.rm = T))

predict(probit_wvs5_gdp, max, type="response")


## >> A country is 93.21% less likely to be a democracy when it moves from minimum to maximum growth in GDP. This runs counter the propositions of modernisation and cultural modernisation theory.
  
  
  
## Calculate a model assessing the impact of traditional–rational values, and GDP growth on the probability of democracy for countries in wave 5. Interpret the results.
###############################################################


probit_wvs5_mix <- glm(democracy ~ traditional_rational + gdp_growth, 
                       data = wvs5, 
                       family = binomial(link = "probit"))

summary(probit_wvs5_mix)


## >> traditional-rational values lose their significance compared to the model which only assessed their impact without GDP growth

## >> GDP growth assumes primacy in explaining democracy here, providing evidence against the causal chain of argument in cultural modernisation theory.
  
  
  
# Considering the main propositions of cultural modernisation, what are the implications of these results? Assess the results of each of the previous tasks in turn.
###############################################################

## >> see above


