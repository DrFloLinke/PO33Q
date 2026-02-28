########################################
# PO33Q - Determinants of Democracy
# Dr Flo Linke
# Seminar, Week 4
########################################

rm(list = ls())

setwd()

## Load data
wdi <- read.csv("world.csv")

world <- wdi[wdi$year == 2010, ]

## ----------------------------
## Exercise 1
## ----------------------------

## (a)
#########

# HA: Higher levels of income are associated with a higher probability of democracy
# H0: No relationship between income and democracy

m1 <- glm(democracy ~ gdppc,
          data = world,
          family = binomial(link = "probit"))
summary(m1)

# -> Positive and significant coefficient for gdppc, so we reject H0



## (b)
#########


biv_lit <- glm(democracy ~ literacy,
          data = world,
          family = binomial(link = "probit"))
summary(biv_lit)

# -> Literacy is insignificant, so we fail to reject H0

biv_urban <- glm(democracy ~ urban,
          data = world,
          family = binomial(link = "probit"))
summary(biv_urban)

# -> Positive and significant coefficient for urbanisation, so we reject H0

biv_life <- glm(democracy ~ life,
          data = world,
          family = binomial(link = "probit"))
summary(biv_life)

# -> Positive and significant coefficient for life expectancy, so we reject H0

## (c)
#########


## Add education
m2 <- glm(democracy ~ gdppc + enrol_net,
          data = world,
          family = binomial(link = "probit"))
summary(m2)

# -> Positive and significant coefficients for gdppc and enrol_net


## Add life expectancy
m3 <- glm(democracy ~ gdppc + enrol_net + life,
          data = world,
          family = binomial(link = "probit"))
summary(m3)

# -> Nothing is significant here, only enrol_net at a 90% confidence level
# which is a weaker, and generally inacceptable level


## Add urbanisation
m4 <- glm(democracy ~ gdppc + enrol_net + life + urban,
          data = world,
          family = binomial(link = "probit"))
summary(m4)

# -> Everything loses significance here


## (d)
#########

nobs(m1)
nobs(m2)
nobs(m3)
nobs(m4)

# -> Sample size decreases with each additional variable due to missing data

## Re-estimate simpler models on common sample
common_vars <- c("democracy", "gdppc", "enrol_net", "life", "urban")
dat_common <- world[complete.cases(world[, common_vars]), ]

m1_c <- glm(democracy ~ gdppc,
            data = dat_common,
            family = binomial(link = "probit"))
m2_c <- glm(democracy ~ gdppc + enrol_net,
            data = dat_common,
            family = binomial(link = "probit"))
summary(m1_c)
summary(m2_c)

# gdppc remains significant on its own, but loses significance when enrol_net is added,
# unless we accept a 90% confidence level. This suggests that the loss of significance in the full model is not solely due to the inclusion of additional variables, but also due to sample size reduction.


## (e)
#########

# Individually, all variables (gdppc, enrol_net, urban, and life) can explain democracy. However, when combined, gdppc is the most robust bivariate predictor
# This suggests that income may be the primary driver of democratisation. 
# It is worthwhile to play around with other operationalisations of education, for example, to see if the issue persists (whether significance is sensitive to the measure). We will do this further below. 


## ------------------------------------------
## Exercise 2
## ------------------------------------------

## (a) & (b)
#############

## Inspect missing data for education measures - there are many different ways, but this is the most elegant, I think:

colMeans(is.na(world[, c("literacy",
                       "enrol_gross",
                       "enrol_net",
                       "primcomp")]))

# Explanation of code:
# is.na() returns a logical matrix indicating missing values (TRUE for NA, FALSE otherwise)
# colMeans() computes the mean of each column, effectively giving the proportion of missing values per column


## (c)
#############

## Estimate competing models
m_lit <- glm(democracy ~ gdppc + literacy,
             data = world,
             family = binomial(link = "probit"))

m_gross <- glm(democracy ~ gdppc + enrol_gross,
               data = world,
               family = binomial(link = "probit"))

m_net <- glm(democracy ~ gdppc + enrol_net,
             data = world,
             family = binomial(link = "probit"))

## (d)
#############

summary(m_lit)
summary(m_gross)
summary(m_net)

nobs(m_lit)
nobs(m_gross)
nobs(m_net)

# Both enrolment measures of education yield similar results, with positive and significant coefficients.
# Literacy is insignificant
# However, the sample size differs substantially between models, with literacy having the smallest sample size and enrol_gross the largest. 
# In the context of developing countries, gross enrolment is preferable to net enrolment, as it captures over-age and under-age enrolment, which is common in these countries.
# But literacy might have greater measurement validity, depending on the causal chain you construct.


## (e)
#############

# -> This very much depends on what exactly you want to show. See comments in (d). 






## --------------------------------
## Exercise 3
## --------------------------------

## (a)
#############

## Log income
world$log_gdppc <- log(world$gdppc)

m_log <- glm(democracy ~ log_gdppc,
             data = world,
             family = binomial(link = "probit"))
summary(m_log)

# Coefficient for log_gdppc is positive and significant.
# We would have to make a judgement call based on model fit whether 
# this is preferable to the linear specification. We will cover this in Week 7. 



## (b)
#############

world$log_gdppc_sq <- world$log_gdppc^2

## Polynomial specification
m_poly <- glm(democracy ~ log_gdppc + log_gdppc_sq,
              data = world,
              family = binomial(link = "probit"))
summary(m_poly)

# A second order polynomial is insignificant
# This model specification is inappropriate based on statistical criteria.


## (c)
#############

## The quadratic term in log income is statistically insignificant, providing no
## evidence of curvature in the income–democracy relationship. As a result, the 
## data do not support diminishing returns or threshold effects; instead, they are 
## consistent with no meaningful non-linearity (or an approximately linear effect) 
## in this specification.


## (d)
#############

## Cross-national data span very large income differences. A linear model assumes
## identical effects of income at all development levels, which is theoretically implausible.
## Logarithms and polynomials allow for diminishing returns, thresholds, and reduce
## the influence of extreme values, making models more theoretically appropriate.
## But we still might end up rejecting some of these theoretical transformations 
## based on statistical criteria, as we did here.



## -------------------------------
## Exercise 4
## -------------------------------


## (a)
#############

m_int <- glm(democracy ~ log_gdppc + enrol_net + log_gdppc * enrol_net,
             data = world,
             family = binomial(link = "probit"))
summary(m_int)

## (b)
#############

# The interaction term log_gdppc:enrol_net is insignificant.
# This means the effect of income (log_gdppc) on the probability of democracy 
# does not depend on levels of education.
# 
# HAD IT BEEN SIGNIFICANT
# We could say that income matters more for democracy in countries with higher education levels.
# For low-education societies, the marginal effect of income on democracy is smaller; 
# for high-education societies, each additional increase in income has a stronger 
# positive effect on the probability of democracy.
# Substantively, this would suggest that wealth alone is less likely to foster democracy
# unless citizens are relatively well-educated. Education amplifies the democratic 
# impact of rising income.

## (c)
#############

library(tidyverse)

# Predicted probabilities at 25th and 75th percentile of enrol_net
enrl_low <- quantile(world$enrol_net, 0.25, na.rm = TRUE)
enrl_high <- quantile(world$enrol_net, 0.75, na.rm = TRUE)

# create values for plotting
plot_wdi <- expand.grid(
  log_gdppc = seq(min(world$log_gdppc, na.rm = TRUE),
                  max(world$log_gdppc, na.rm = TRUE),
                  length.out = 100),
  enrol_net = c(enrl_low, enrl_high)
)

# predict probabilities
plot_wdi$pred <- predict(m_int, newdata = plot_wdi, type = "response")

# create factor for plotting
plot_wdi$education_level <- factor(
  plot_wdi$enrol_net,
  levels = c(enrl_low, enrl_high),
  labels = c("Low education", "High education")
)

colors <- c("black", "#e57726")

ggplot(plot_wdi, 
       aes(x = log_gdppc, 
           y = pred, 
           linetype = education_level, 
           color = education_level)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = colors) +
  labs(
    x = "Log GDP per capita",
    y = "Pr(Democracy)",
    linetype = "Education level",
    color = "Education level"
  ) +
  theme_classic()

## Substantively, this curve has very limited use.
## If all coefficients are statistically insignificant, a predicted probability 
## curve does not represent an estimated relationship supported by the data.



## (d)
#############

# No, we have no evidence that education moderates the effect of income on democracy.
# As such there are bno conditional effects. 




## ----------------------------
## Exercise 5
## ----------------------------


# We clearly need logarithmised income based on previous exercises.
# We also need to include education, as this is the only other of our theoretical
# variables that consistently shows significance.
# But to test the full theoretical chain, let us include life and urban in a first attempt:

m_final <- glm(democracy ~ log_gdppc + enrol_net + life + urban,
               data = world,
               family = binomial(link = "probit"))
summary(m_final)

# Nothing is significant here, so let us drop urbanisation


m_final <- glm(democracy ~ log_gdppc + enrol_net + life,
               data = world,
               family = binomial(link = "probit"))
summary(m_final)

# Now let's also drop life 

m_final <- glm(democracy ~ log_gdppc + enrol_net,
               data = world,
               family = binomial(link = "probit"))
summary(m_final)


# Out of our theoretical chain, we find evidence consistent with wealth 
# and education being relevant predictors. 

# Factors other than those included here might also be important, such as external factors, culture, etc. Therefore, some control variables might be worth including based on your theoretical framework to bring the model closer to reality. 

