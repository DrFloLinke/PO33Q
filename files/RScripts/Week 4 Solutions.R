#########################################################################################
# PO33Q - The Life and Death of Democracies and Dictatorships: A Quantitative Perspective
# Dr Flo Linke
# Seminar, Week 4
#########################################################################################

rm(list = ls())

# -----------------------
# set working directory
# -----------------------

setwd("")

# -----------------------
# Load Required Packages
# -----------------------

library(tidyverse)
library(modelsummary)
library(tinytable)

# -----------------------
# Load data
# -----------------------

wdi <- read.csv("world.csv")

world <- wdi[wdi$year == 2010, ]


###############################################
# Exercise 1
###############################################

# ------------------------------------------
# (a) Bivariate probit: income and democracy
# ------------------------------------------

# HA: Higher levels of income are associated with a higher probability of democracy
# H0: No relationship between income and democracy

m1 <- glm(democracy ~ gdppc,
          data = world,
          family = binomial(link = "probit"))
summary(m1)

# -> Positive and significant coefficient for gdppc, so we reject H0


# ------------------------------------------
# (b) Bivariate probits: other predictors
# ------------------------------------------

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


# ------------------------------------------
# (c) Multivariate probits: adding controls
# ------------------------------------------

# Add education
m2 <- glm(democracy ~ gdppc + enrol_net,
          data = world,
          family = binomial(link = "probit"))
summary(m2)

# -> Positive and significant coefficients for gdppc and enrol_net

# Add life expectancy
m3 <- glm(democracy ~ gdppc + enrol_net + life,
          data = world,
          family = binomial(link = "probit"))
summary(m3)

# -> Nothing is significant here, only enrol_net at a 90% confidence level
# which is a weaker, and generally inacceptable level

# Add urbanisation
m4 <- glm(democracy ~ gdppc + enrol_net + life + urban,
          data = world,
          family = binomial(link = "probit"))
summary(m4)

# -> Everything loses significance here


# ------------------------------------------
# (d) Sample size and common sample
# ------------------------------------------

nobs(m1)
nobs(m2)
nobs(m3)
nobs(m4)

# -> Sample size decreases with each additional variable due to missing data

# Re-estimate simpler models on common sample
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
# unless we accept a 90% confidence level. This suggests that the loss of significance 
# in the full model is not solely due to the inclusion of additional variables, but also 
# due to sample size reduction.


# ------------------------------------------
# (e) Summary
# ------------------------------------------

# Individually, all variables (gdppc, enrol_net, urban, and life) can explain democracy. 
# However, when combined, gdppc is the most robust bivariate predictor.
# This suggests that income may be the primary driver of democratisation. 
# It is worthwhile to play around with other operationalisations of education, 
# for example, to see if the issue persists (whether significance is sensitive 
# to the measure). We will do this further below.


###############################################
# Exercise 2
###############################################

# ------------------------------------------
# (a) & (b) Missing data in education measures
# ------------------------------------------

# Inspect missing data for education measures

colMeans(is.na(world[, c("literacy",
                         "enrol_gross",
                         "enrol_net",
                         "primcomp")]))

# Explanation of code:
# is.na() returns a logical matrix indicating missing values (TRUE for NA, FALSE otherwise)
# colMeans() computes the mean of each column, effectively giving the proportion of 
# missing values per column


# ------------------------------------------
# (c) Estimate competing models
# ------------------------------------------

m_lit <- glm(democracy ~ gdppc + literacy,
             data = world,
             family = binomial(link = "probit"))

m_gross <- glm(democracy ~ gdppc + enrol_gross,
               data = world,
               family = binomial(link = "probit"))

m_net <- glm(democracy ~ gdppc + enrol_net,
             data = world,
             family = binomial(link = "probit"))


# ------------------------------------------
# (d) Compare results
# ------------------------------------------

summary(m_lit)
summary(m_gross)
summary(m_net)

nobs(m_lit)
nobs(m_gross)
nobs(m_net)

# Both enrolment measures of education yield similar results, with positive and 
# significant coefficients.
# Literacy is insignificant.
# However, the sample size differs substantially between models, with literacy having 
# the smallest sample size and enrol_gross the largest. 
# In the context of developing countries, gross enrolment is preferable to net enrolment, 
# as it captures over-age and under-age enrolment, which is common in these countries.
# But literacy might have greater measurement validity, depending on the causal chain 
# you construct.


# ------------------------------------------
# (e) Choice of education measure
# ------------------------------------------

# -> This very much depends on what exactly you want to show. See comments in (d).


###############################################
# Exercise 3
###############################################

# ------------------------------------------
# (a) Log income
# ------------------------------------------

world$log_gdppc <- log(world$gdppc)

m_log <- glm(democracy ~ log_gdppc,
             data = world,
             family = binomial(link = "probit"))
summary(m_log)

# Coefficient for log_gdppc is positive and significant.
# We would have to make a judgement call based on model fit whether 
# this is preferable to the linear specification. We will cover this in Week 7.


# ------------------------------------------
# (b) Polynomial specification
# ------------------------------------------

world$log_gdppc_sq <- world$log_gdppc^2

m_poly <- glm(democracy ~ log_gdppc + log_gdppc_sq,
              data = world,
              family = binomial(link = "probit"))
summary(m_poly)

# A second order polynomial is insignificant.
# This model specification is inappropriate based on statistical criteria.


# ------------------------------------------
# (c) Interpretation
# ------------------------------------------

# The quadratic term in log income is statistically insignificant, providing no
# evidence of curvature in the income-democracy relationship. As a result, the 
# data do not support diminishing returns or threshold effects; instead, they are 
# consistent with no meaningful non-linearity (or an approximately linear effect) 
# in this specification.


# ------------------------------------------
# (d) Why transform income?
# ------------------------------------------

# Cross-national data span very large income differences. A linear model assumes
# identical effects of income at all development levels, which is theoretically 
# implausible. Logarithms and polynomials allow for diminishing returns, thresholds, 
# and reduce the influence of extreme values, making models more theoretically 
# appropriate. But we still might end up rejecting some of these theoretical 
# transformations based on statistical criteria, as we did here.


###############################################
# Exercise 4
###############################################

# ------------------------------------------
# (a) Categorical moderator: income x resource wealth
# ------------------------------------------

# Build a high/low resource dummy by splitting natural resource rents at the median
world$resource_hi <- ifelse(world$natural > median(world$natural, na.rm = TRUE), 1, 0)

m_int_res <- glm(democracy ~ log_gdppc * resource_hi,
                 data = world,
                 family = binomial(link = "probit"))
summary(m_int_res)

# The interaction (log_gdppc:resource_hi) tests whether the effect of income on the
# probability of democracy DIFFERS between resource-rich and resource-poor countries.
# Rentier logic would predict a flatter income slope in resource-rich states: where
# the regime is funded by rents rather than a productive, taxed citizenry, rising
# income need not translate into pressure for democratisation.


# ------------------------------------------
# (b) Interpretation
# ------------------------------------------

# Coefficients are on the latent (z-score) scale, not in probability units.
# With the interaction present:
#   - log_gdppc      = effect of income when resource_hi = 0 (i.e. in LOW-resource countries)
#   - resource_hi    = shift in the index for high-resource countries at log_gdppc = 0
#   - the interaction = how much the income slope DIFFERS in high-resource countries
#
# RESULT: 
# - the interaction is significant and negative (-0.443, p = 0.008).
# - Income's effect on democracy is concentrated in resource-POOR countries:
#   - latent income slope = 0.486 (low resources) vs ~0.043 (high resources).
# - avg_slopes() confirms this on the probability scale:
#   - dPr/d(log GDP) = 0.106 (p < 0.001) for low-resource countries,
#                      0.017 (p = 0.67)   for high-resource countries.
# -> Consistent with the rentier argument: in resource-rich states, rising income
#    does not raise the probability of democracy.
#
# Note: beta3 is a slope difference on the latent scale, not a probability effect.
# You still need the predicted probabilities / avg_slopes in (c) for the substantive
# magnitudes - and to see that the two curves cross at low income - even though here
# they point the same way as beta3.


# ------------------------------------------
# (c) Predicted probabilities by resource group
# ------------------------------------------

# Predicted Pr(democracy) across income, separately for low/high resource countries
plot_res <- expand.grid(
  log_gdppc   = seq(min(world$log_gdppc, na.rm = TRUE),
                    max(world$log_gdppc, na.rm = TRUE),
                    length.out = 100),
  resource_hi = c(0, 1)
)
plot_res$pred <- predict(m_int_res, newdata = plot_res, type = "response")
plot_res$resource <- factor(plot_res$resource_hi,
                            levels = c(0, 1),
                            labels = c("Low resources", "High resources"))

ggplot(plot_res,
       aes(x = log_gdppc, y = pred,
           color = resource, linetype = resource)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("black", "#e57726")) +
  labs(x = "Log GDP per capita", y = "Pr(Democracy)",
       color = "Resource wealth", linetype = "Resource wealth") +
  theme_classic()

# To judge the interaction on the probability scale, look at the income slope WITHIN
# each group rather than at the raw coefficient:
# install.packages("marginaleffects")   # if needed
library(marginaleffects)
avg_slopes(m_int_res, variables = "log_gdppc", by = "resource_hi")

# What the plot adds beyond the coefficient:
# The two curves CROSS at about log GDP ~ 6.2 (= -coef(resource_hi)/coef(interaction)
# = 2.754/0.443, the same intersection logic as the OLS chapter). At the very poorest
# end the model puts resource-rich countries slightly ABOVE resource-poor ones, before
# the low-resource curve overtakes and pulls away. A single interaction coefficient
# cannot tell you the between-group gap changes sign across income - the
# predicted-probability plot can.
#
# In a probit, Pr(democracy) is a non-linear (S-shaped) function of the index, so the
# same index-scale slope maps onto different probability changes depending on where a
# country sits on the curve. Here beta3 and the marginal effects agree on the headline
# (income matters in low-resource, not high-resource countries), but beta3 alone still
# misses the crossover and the actual probability magnitudes. Read the substance off
# predicted probabilities / avg_slopes, not the coefficient.

# PUZZLE: In the OLS chapter, beta3 = 0 meant parallel lines because the outcome is a
# LINEAR function of the index - the slope difference IS the coefficient. In a probit the
# link is non-linear, so two groups with the same index-scale slope still map onto
# differently-curved probability paths; equal slopes on the latent scale do not give
# parallel curves on the probability scale.


# ------------------------------------------
# (d) Continuous moderator: income x education
# ------------------------------------------

m_int <- glm(democracy ~ log_gdppc * enrol_net,
             data = world,
             family = binomial(link = "probit"))
summary(m_int)

# The interaction term log_gdppc:enrol_net is insignificant.
# This means the effect of income on the probability of democracy does not depend on
# levels of education.
#
# Mind the sample: this model uses 122 observations, vs 184 for the resource model in (a),
# because enrol_net is missing for many more countries (72 dropped here). So the contrast
# between a SIGNIFICANT resource interaction and a NULL education interaction is partly a
# power/sample-size story, not purely a substantive one - cf. Exercise 1(d). 
# Don't over-read it.
#
# HAD IT BEEN SIGNIFICANT (and positive):
# Income would matter more for democracy in better-educated societies - in low-education
# countries the marginal effect of income would be smaller, and in high-education
# countries each additional increase in income would have a stronger positive effect on
# the probability of democracy. Substantively: wealth alone is less likely to foster
# democracy unless citizens are relatively well-educated; education amplifies the
# democratic impact of rising income.


# ------------------------------------------
# (e) Predicted probability plot (Q1 vs Q3 of education)
# ------------------------------------------

enrl_low  <- quantile(world$enrol_net, 0.25, na.rm = TRUE)
enrl_high <- quantile(world$enrol_net, 0.75, na.rm = TRUE)

plot_wdi <- expand.grid(
  log_gdppc = seq(min(world$log_gdppc, na.rm = TRUE),
                  max(world$log_gdppc, na.rm = TRUE),
                  length.out = 100),
  enrol_net = c(enrl_low, enrl_high)
)
plot_wdi$pred <- predict(m_int, newdata = plot_wdi, type = "response")
plot_wdi$education_level <- factor(plot_wdi$enrol_net,
                                   levels = c(enrl_low, enrl_high),
                                   labels = c("Low education", "High education"))

ggplot(plot_wdi,
       aes(x = log_gdppc, y = pred,
           linetype = education_level, color = education_level)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("black", "#e57726")) +
  labs(x = "Log GDP per capita", y = "Pr(Democracy)",
       linetype = "Education level", color = "Education level") +
  theme_classic()

# This curve has very limited use here: if the coefficients (and the interaction) are
# insignificant, the curve does not represent an estimated relationship supported by the
# data - it is drawing a difference the model cannot distinguish from zero.


# ------------------------------------------
# (f) Conclusion
# ------------------------------------------

# For education: no evidence that it moderates the effect of income on democracy, so no
# support for conditional modernisation effects.
# Here the two ways of judging conditionality AGREE - the interaction coefficient is
# insignificant AND the predicted-probability curves are effectively indistinguishable.
# But (c) showed they need not agree in general: in a non-linear model the coefficient is
# not the final word, so conditional effects should always be assessed on the probability
# scale (predicted probabilities / average marginal effects), not from beta3 alone.


###############################################
# Exercise 5
###############################################

# ------------------------------------------
# Building the final model
# ------------------------------------------

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

# Factors other than those included here might also be important, such as external 
# factors, culture, etc. Therefore, some control variables might be worth including 
# based on your theoretical framework to bring the model closer to reality.