########################################
# PO33Q - Determinants of Democracy
# Dr Flo Linke
# Lecture, Week 7
########################################

# -----------------------
# Load Required Packages
# -----------------------

library(tidyverse)
library(pROC)   # for ROC curves: https://cran.r-project.org/web/packages/pROC/pROC.pdf
library(survey) # for RegTermTest: # http://r-survey.r-forge.r-project.org/survey/html/regTermTest.html
library(modelsummary)
library(tinytable)

# -----------------------
# set working directory
# -----------------------

setwd("")

# -----------------------
# Load data, Slide 13
# -----------------------

world <- read.csv("world.csv")


# -----------------------------------------
# lagging the dependent variable, Slide 14
# -----------------------------------------

world <- world %>%
  group_by(countrycode) %>%
  mutate(l.democracy = lag(democracy)) %>%
  ungroup()


# --------------------------------------------------
# lagging the independent variables, Slides 17 & 18
# --------------------------------------------------

world <- world %>%
  group_by(countrycode) %>%
  mutate(l.gdppc = lag(gdppc)) %>%
  ungroup()


world <- world %>%
  group_by(countrycode) %>%
  mutate(l.life = lag(life)) %>%
  ungroup()


world <- world %>%
  group_by(countrycode) %>%
  mutate(l.enrol_gross = lag(enrol_gross)) %>%
  ungroup()


# ----------------------------------------------------
# Subset data for conditional probabilities, Slide 19
# ----------------------------------------------------

# select observations in which l.democracy=0
#-------------------------------------------

world_democ0 <- filter(world, l.democracy==0)


# select observations in which l.democracy=1
#-------------------------------------------

world_democ1 <- filter(world, l.democracy==1)


###############################################
# Estimate Markov Transition Models
###############################################

# ------------------------------------------
# estimate emergence model, Slides 21 & 22
# ------------------------------------------

emergence <- glm(democracy ~ l.gdppc, 
                 data = world_democ0, 
                 na.action = na.exclude,
                 family = binomial(link = "probit"))


summary(emergence)



# ----------------------------------------------
# estimate full emergence model, Slides 23 & 24
# ----------------------------------------------

emergence_full <- glm(democracy ~ l.gdppc + l.life + l.enrol_gross, 
                      data = world_democ0, 
                      na.action = na.exclude,
                      family = binomial(link = "probit"))

summary(emergence_full)




# ------------------------------------------
# estimate survival model, Slides 27 & 28
# ------------------------------------------

survival <- glm(democracy ~ l.gdppc, 
                  data = world_democ1, 
                  na.action = na.exclude,
                  family = binomial(link = "probit"))

summary(survival)



# ------------------------------------------
# estimate full survival model, Slides 29 & 30
# ------------------------------------------

survival_full <- glm(democracy ~ l.gdppc + l.life + l.enrol_gross, 
                       data = world_democ1, 
                       na.action = na.exclude,
                       family = binomial(link = "probit"))


summary(survival_full)




############################
# ROC Curves
############################

# ------------------------------------------
# Emergence, Slides 47 & 48
# ------------------------------------------

prob_em <- predict(emergence, type="response")
world_democ0$prob_em <- unlist(prob_em)

roc1 <- roc(world_democ0$democracy, world_democ0$prob_em)

auc(roc1)
plot(roc1, print.auc=TRUE)


# plot with ggplot2, Slides 49 & 50
#----------------------------------

auc_value1 <- auc(roc1)

ggroc(roc1) +
  geom_path(
    aes(x = specificity, y = sensitivity),
    color = "#e57726", linewidth = 1) +
  geom_ribbon(aes(x = specificity,ymin = 0,ymax = sensitivity),
              fill = "#e57726", alpha = 0.25) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "#8a1e00") +
  annotate("text",x = 0.25,y = 0.25,
           label = paste("AUC =", round(auc_value1, 3)),
           size = 6,family = "sans") +
  xlab("Specificity")+
  ylab("Sensitivity")+
  theme_classic() +
  coord_fixed()



# ------------------------------------------
# Full Emergence, Slide 51
# ------------------------------------------

prob_em_full <- predict(emergence_full, type="response")
world_democ0$prob_em_full <- unlist(prob_em_full)

roc2 <- roc(world_democ0$democracy, world_democ0$prob_em_full)

auc(roc2)
plot(roc2, print.auc=TRUE) 


# plot with ggplot2, Slide 52
#----------------------------------

auc_value2 <- auc(roc2)

ggroc(roc2) +
  geom_path(
    aes(x = specificity, y = sensitivity),
    color = "#e57726", linewidth = 1) +
  geom_ribbon(aes(x = specificity,ymin = 0,ymax = sensitivity),
              fill = "#e57726", alpha = 0.25) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "#8a1e00", linewidth=1.1) +
  annotate("text",x = 0.25,y = 0.25,
           label = paste("AUC =", round(auc_value2, 3)),
           size = 6,family = "sans") +
  xlab("Specificity")+
  ylab("Sensitivity")+
  theme_classic() +
  coord_fixed()



# ------------------------------------------
# Survival, Slide 53 
# ------------------------------------------

prob_sur <- predict(survival, type="response")
world_democ1$prob_sur <- unlist(prob_sur)

roc3 <- roc(world_democ1$democracy, world_democ1$prob_sur)

auc(roc3)
plot(roc3, print.auc=TRUE)

# plot with ggplot2, Slide 54
#----------------------------------

auc_value3 <- auc(roc3)

ggroc(roc3) +
  geom_path(
    aes(x = specificity, y = sensitivity),
    color = "#e57726", linewidth = 1) +
  geom_ribbon(aes(x = specificity,ymin = 0,ymax = sensitivity),
              fill = "#e57726", alpha = 0.25) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "#8a1e00", linewidth=1.1) +
  annotate("text",x = 0.25,y = 0.25,
           label = paste("AUC =", round(auc_value3, 3)),
           size = 6,family = "sans") +
  xlab("Specificity")+
  ylab("Sensitivity")+
  theme_classic() +
  coord_fixed()



# ------------------------------------------
# Full Survival, Slide 55
# ------------------------------------------

prob_sur_full <- predict(survival_full, type="response")
world_democ1$prob_sur_full <- unlist(prob_sur_full)

roc4 <- roc(world_democ1$democracy, world_democ1$prob_sur_full)

auc(roc4)
plot(roc4, print.auc=TRUE)


# plot with ggplot2, Slide 56
#----------------------------------

auc_value4 <- auc(roc4)

ggroc(roc4) +
  geom_path(
    aes(x = specificity, y = sensitivity),
    color = "#e57726", linewidth = 1) +
  geom_ribbon(aes(x = specificity,ymin = 0,ymax = sensitivity),
              fill = "#e57726", alpha = 0.25) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "#8a1e00", linewidth=1.1) +
  annotate("text",x = 0.25,y = 0.25,
           label = paste("AUC =", round(auc_value4, 3)),
           size = 6,family = "sans") +
  xlab("Specificity")+
  ylab("Sensitivity")+
  theme_classic() +
  coord_fixed()


###########################################
# modelsummary Table, Slide 57
###########################################


# store models in a list
#------------------------

models <- list(
  "Classical"    = emergence,
  "New"    = emergence_full,
  "Classical"    = survival,
  "New"    = survival_full
)

# write the coefficient map
#----------------------------------

cm <- c('l.gdppc'    = 'per capita GDP (lagged)',
        'l.life'    = 'Life Expectancy (lagged)',
        'l.enrol_gross'    = 'Gross Primary Enrollment (lagged)',
        '(Intercept)' = 'Intercept')

# create a mini dataset with the information on the ROC curves
#--------------------------------------------------------------

rows <- tibble(
  '~term' = 'ROC Curve',
  `~(1)` = auc(roc1),
  `~(2)` = auc(roc2),
  `~(3)` = auc(roc3),
  `~(4)` = auc(roc4)
)

# place the custom row into position in the final table
#----------------------------------------------------------

attr(rows, 'position') <- c(9)


# modelsummary code
#----------------------------------

modelsummary(models,
             stars = TRUE,
             coef_map = cm,
             gof_omit = 'AIC|BIC|Log.Lik|F|RMSE',
             add_rows = rows)|>                                 # this is the custom row for ROC
  group_tt(j = list("Emergence" = 2:3, "Survival" = 4:5))|>     # this creates the group header
  group_tt(j = list("Dependent Variable: Democracy" = 2:5))     # this creates the top line



###########################################
# JOINT ESTIMATION WORKSHEET
###########################################

# This is the "proper" code with the independent variable lagged, as well

world$l.gdppc_l.democracy <- world$l.democracy * world$l.gdppc


joint <- glm(democracy ~ l.gdppc + l.democracy + l.gdppc_l.democracy, 
                data = world, 
                na.action = na.exclude,
                family = binomial(link = "probit"))


summary(joint)


# ------------------------------------------
# WALD TEST FOR JOINT ESTMATION
# ------------------------------------------


regTermTest(joint, ~l.gdppc+l.gdppc_l.democracy, method="Wald")



