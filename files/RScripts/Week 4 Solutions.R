###################################################################
# PO33Q - Weeek 4, Solutions
###################################################################

setwd()

## Load data
world <- read.csv("world.csv")

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

# -> Positive and significant coefficient for literacy, so we reject H0

biv_urban <- glm(democracy ~ urban,
          data = world,
          family = binomial(link = "probit"))
summary(biv_urban)

# -> Positive and significant coefficient for urbanisation, so we reject H0

biv_life <- glm(democracy ~ life,
          data = world,
          family = binomial(link = "probit"))
summary(biv_life)

# -> Positive and significant coefficient for urbanisation, so we reject H0

## (c)
#########


## Add education
m2 <- glm(democracy ~ gdppc + literacy,
          data = world,
          family = binomial(link = "probit"))
summary(m2)

# -> Positive and significant coefficients for gdppc and literacy

## Add urbanisation
m3 <- glm(democracy ~ gdppc + literacy + urban,
          data = world,
          family = binomial(link = "probit"))
summary(m3)

# -> Only gdppc remains significant, literacy and urban lose significance


## Add health (example: life expectancy)
m4 <- glm(democracy ~ gdppc + literacy + urban + life,
          data = world,
          family = binomial(link = "probit"))
summary(m4)

# -> Only gdppc remains significant, all other variables lose significance


## (d)
#########

nobs(m1)
nobs(m2)
nobs(m3)
nobs(m4)

# -> Sample size decreases with each additional variable due to missing data

## Re-estimate simpler models on common sample
common_vars <- c("democracy", "gdppc", "literacy", "urban", "life")
dat_common <- world[complete.cases(world[, common_vars]), ]

m1_c <- glm(democracy ~ gdppc,
            data = dat_common,
            family = binomial(link = "probit"))
m2_c <- glm(democracy ~ gdppc + literacy,
            data = dat_common,
            family = binomial(link = "probit"))
summary(m1_c)
summary(m2_c)

# -> On a common sample, both gdppc and literacy remain significant


## (e)
#########

# Individually, all variables can explain democracy. However, when combined, only income remains significant.
# This suggests that income may be the primary driver of democratisation. 
# It is worthwhile to play around with other operationalisations of education, for example, to see if the issue persists (whether significance is sensitive to the measure). We will do this further below. 


## ------------------------------------------
## Exercise 2
## ------------------------------------------

## (a) & (b)
#############

## Inspect missing data for education measures - there are many different ways, but thiws is the most elegant, I think:

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

# -> In all models the education measure is significant and positive, so as far as education is concerned, the results are robust to the choice of measure.
# However, the sample size differs substantially between models, with literacy having the smallest sample size and enrol_gross the largest. 
# In the context of developing countries, gross enrolment is preferable to net enrolment, as it captures over-age and under-age enrolment, which is common in these countries.
# Also, literacy turns the influence of gdppc negative, which is counter-intuitive.
# But might literacy have greater measurement validity?


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

# -> Coefficient for log_gdppc is positive and significant.
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

# -> Coefficient for log_gdppc is positive and significant, 
# coefficient for log_gdppc_sq is negative and significant, indicating a 
# concave relationship. Again, we would have to make a judgement call based 
# on model fit whether this is preferable to the linear specification. 
# We will cover this in Week 7.


## (c)
#############

## The positive and highly significant coefficient on log GDP per capita, together
## with the negative and highly significant squared term, provides strong evidence
## of diminishing returns. Income substantially increases the probability of
## democracy at low and middle levels of development, but the marginal effect
## declines as countries become richer. There is no indication of a sharp threshold;
## instead, the results are consistent with a concave, flattening relationship
## between income and democracy.

## If you wanted to visualise this, here is the code:

library(tidyverse)

# Sequence of log GDP per capita values
log_gdppc_seq <- seq(5, 12, length.out = 100)

# Predicted value on the probit scale
probit_pred <- -4.416584 + 0.787203 * log_gdppc_seq - 0.025921 * log_gdppc_seq^2

# Convert to probability using the standard normal CDF
prob_pred <- pnorm(probit_pred)

# Put into a data frame for plotting
dat_plot <- data.frame(log_gdppc = log_gdppc_seq, prob_democracy = prob_pred)

# Plot
ggplot(dat_plot, aes(x = log_gdppc, y = prob_democracy)) +
  geom_line(linewidth = 1.2, color = "#e57726") +
  labs(x = "log(GDP per capita)", y = "Pr(Democracy)") +
  theme_classic()


## (d)
#############

## Cross-national data span very large income differences. A linear model assumes
## identical effects of income at all development levels, which is implausible.
## Logarithms and polynomials allow for diminishing returns, thresholds, and reduce
## the influence of extreme values, making models more theoretically and empirically
## appropriate.



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

# The interaction term log_gdppc:enrol_net is positive and highly significant (0.010565, p < 0.001).
# This means the effect of income (log_gdppc) on the probability of democracy 
# increases as education (enrol_net) rises.
# We could also say that income matters more for democracy in countries with higher education levels.
# For low-education societies, the marginal effect of income on democracy is smaller; 
# for high-education societies, each additional increase in income has a stronger 
# positive effect on the probability of democracy.
# Substantively, this suggests that wealth alone is less likely to foster democracy
# unless citizens are relatively well-educated. Education amplifies the democratic 
# impact of rising income.

## (c)
#############

# Predicted probabilities at 25th and 75th percentile of enrol_net
enrl_low <- quantile(world$enrol_net, 0.25, na.rm = TRUE)
enrl_high <- quantile(world$enrol_net, 0.75, na.rm = TRUE)

# create values for plotting
plot_wdi <- expand.grid(
  log_gdppc = seq(min(world$log_gdppc, na.rm = TRUE),
                  max(world$log_gdppc, na.rm = TRUE),
                  length.out = 100),
  enrol_net = c(enrl_low, enrl_high)  # use low and high levels
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

## (d)
#############

# Yes, we have strong evidence that education moderates the effect of income on democracy.
# The positive and significant interaction term indicates that the impact of income
# on the probability of democracy increases with higher education levels.
# This suggests that education enhances the democratic benefits of rising income,
# making it a crucial factor in the income-democracy relationship.



## ----------------------------
## Exercise 5
## ----------------------------


# We clearly need logarithmised income and an interaction between income and education based on previous exercises.
# We also need to include life expectancy (significant in Exercise 1) and urbanisation (theoretically important).
# This leads to the folloing model:

m_final <- glm(democracy ~ log_gdppc + enrol_net + log_gdppc:enrol_net + life + urban,
               data = world,
               family = binomial(link = "probit"))
summary(m_final)

# Here, we show that urbanisation is not important statistically - we have now tested the entire theoretical chain, but we can reduce the final model by removing urbanisation.


m_final <- glm(democracy ~ log_gdppc + enrol_net + log_gdppc:enrol_net + life,
               data = world,
               family = binomial(link = "probit"))
summary(m_final)

# Income matters more in highly educated societies
# Life expectancy boosts democracy
# Wealth alone isn't enough; education amplifies impact


# Factors other than those included here might also be important, such as external factors, culture, etc. Therefore, some control variables might be worth including based on your theoretical framework to bring the model closer to reality. 

