########################################
# PO33Q - Determinants of Democracy
# Dr Flo Linke
# Results Table, Week 7
########################################

# -----------------------
# set working directory
# -----------------------

setwd("")

# -----------------------
# Load Required Packages
# -----------------------

library(haven)
library(tidyverse)
library(pROC)          # for ROC curves
library(modelsummary)
library(tinytable)


# -----------------------
# Load data
# -----------------------

prz <- read_dta("prz.dta")


###############################################
# Estimate Models
###############################################

# ------------------------------------------
# Exercise 1: Static probit model
# ------------------------------------------

static <- glm(democ ~ g + gdpw + oil, 
              data = prz,
              na.action = na.exclude,
              family = binomial(link = "probit"))


# ------------------------------------------
# Exercise 2: Dynamic probit model
# ------------------------------------------

prz <- prz %>%
  group_by(country) %>%
  mutate(l.democ = lag(democ)) %>%
  ungroup()

dynamic <- glm(democ ~ g + gdpw + oil + l.democ, 
               data = prz,
               na.action = na.exclude,
               family = binomial(link = "probit")) 


# ------------------------------------------
# Exercise 3: Emergence model
# ------------------------------------------

# lag the independent variables

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

# subset to non-democracies and estimate

prz_democ0 <- filter(prz, l.democ==0) 

emergence <- glm(democ ~ l.g + l.gdpw + l.oil, 
                 data = prz_democ0,
                 na.action = na.exclude,
                 family = binomial(link = "probit"))


# ------------------------------------------
# Exercise 4: Survival model
# ------------------------------------------

prz_democ1 <- filter(prz, l.democ==1)

survive <- glm(democ ~ l.g + l.gdpw + l.oil, 
               data = prz_democ1,
               na.action = na.exclude,
               family = binomial(link = "probit"))


# ------------------------------------------
# Exercise 5: Joint estimation
# ------------------------------------------

# create interaction terms

prz$l.democgdpw <- prz$l.democ * prz$l.gdpw
prz$l.democg <- prz$l.democ * prz$l.g
prz$l.democoil <- prz$l.democ * prz$l.oil

joint <- glm(democ ~ l.g + l.gdpw + l.oil + l.democ + l.democg + l.democgdpw + l.democoil, 
             data = prz,
             na.action = na.exclude,
             family = binomial(link = "probit"))


###############################################
# Exercise 6: ROC Curves
###############################################

# ------------------------------------------
# Static model
# ------------------------------------------

prob_static <- predict(static, type="response")
prz$prob_static <- unlist(prob_static)

roc_static <- roc(prz$democ, prz$prob_static)


# ------------------------------------------
# Dynamic model
# ------------------------------------------

prob_dynamic <- predict(dynamic, type="response")
prz$prob_dynamic <- unlist(prob_dynamic)

roc_dynamic <- roc(prz$democ, prz$prob_dynamic)


# ------------------------------------------
# Emergence model
# ------------------------------------------

prob_emergence <- predict(emergence, type="response")
prz_democ0$prob_emergence <- unlist(prob_emergence)

roc_emergence <- roc(prz_democ0$democ, prz_democ0$prob_emergence)


# ------------------------------------------
# Survival model
# ------------------------------------------

prob_survive <- predict(survive, type="response")
prz_democ1$prob_survive <- unlist(prob_survive)

roc_survive <- roc(prz_democ1$democ, prz_democ1$prob_survive)


# ------------------------------------------
# Joint model
# ------------------------------------------

prob_joint <- predict(joint, type="response")
prz$prob_joint <- unlist(prob_joint)

roc_joint <- roc(prz$democ, prz$prob_joint)


###############################################
# modelsummary Table
###############################################

# ------------------------------------------
# Store models in a list
# ------------------------------------------

models <- list(
  "Probit (static)"    = static,
  "Probit (lagged)"    = dynamic,
  "Emergence"          = emergence,
  "Survival"           = survive,
  "Full Interaction"   = joint
)

# ------------------------------------------
# Write the coefficient map
# ------------------------------------------

cm <- c('g'              = 'Growth',
        'gdpw'           = 'per capita GDP',
        'oil'            = 'Oil Exporter (Yes)',
        'l.g'            = 'Growth (lagged)',
        'l.gdpw'         = 'per capita GDP (lagged)',
        'l.oil'          = 'Oil Exports (lagged)',
        'l.democ'        = 'Democracy (lagged)',       
        'l.democg'       = 'Democracy x Growth (lagged)',  
        'l.democgdpw'    = 'Democracy x per capita GDP (lagged)',  
        'l.democoil'     = 'Democracy x Oil Exporter (lagged)',  
        '(Intercept)'    = 'Intercept')

# ------------------------------------------
# Custom row for ROC curves
# ------------------------------------------

rows <- tibble(
  '~term' = 'ROC Curve',
  `~(1)` = auc(roc_static),
  `~(2)` = auc(roc_dynamic),
  `~(3)` = auc(roc_emergence),
  `~(4)` = auc(roc_survive),
  `~(5)` = auc(roc_joint)
)

# place the custom row into position in the final table
attr(rows, 'position') <- c(23)

# ------------------------------------------
# Generate table
# ------------------------------------------

modelsummary(models,
             title = 'Regression Models, data are taken from \\citet{prz:2000}',
             escape = FALSE,
             stars = TRUE,
             coef_map = cm,
             gof_omit = 'AIC|BIC|Log.Lik|F|RMSE',
             notes = "\\vspace{0.3\\baselineskip}",
             notes_append = TRUE,
             add_rows = rows)|>
  group_tt(j = list("Dependent Variable: Democracy" = 2:6))|>
  theme_latex(resize_width= 1.0, resize_direction="both")|> 
  theme_latex(outer = "label={tblr:test}")|> 
  theme_latex(placement= "H")