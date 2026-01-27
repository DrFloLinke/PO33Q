########################################
# PO33Q - Determinants of Democracy
# Dr Florian Linke
# Lecture, Week 7
########################################

# -----------------------
# Load Required Pages
# -----------------------

library(tidyverse)
library(pROC)

# -----------------------
# set working directory
# -----------------------

setwd("~/Dropbox/PO33Q/files/Lectures/Week-7")

# -----------------------
# Load data
# -----------------------

world <- read.csv("world.csv")


# --------------------------------
# lagging the dependent variable
# --------------------------------

world <- world %>%
  group_by(countrykey) %>%
  mutate(l.democracy = lag(democracy)) %>%
  ungroup()


# ----------------------------------
# lagging the independent variables
# ----------------------------------

world <- world %>%
  group_by(countrykey) %>%
  mutate(l.gdp_pc = lag(gdp_pc)) %>%
  ungroup()


world <- world %>%
  group_by(countrykey) %>%
  mutate(l.lifeexp = lag(lifeexp)) %>%
  ungroup()


world <- world %>%
  group_by(countrykey) %>%
  mutate(l.enrl_gross = lag(enrl_gross)) %>%
  ungroup()


# ------------------------------------------
# Subset data for conditional probabilities
# ------------------------------------------

# select observations in which l.democracy=0

world_democ0 <- filter(world, l.democracy==0)


# select observations in which l.democracy=1

world_democ1 <- filter(world, l.democracy==1)


# ------------------------------------------
# Estimate Markov Transition Models
# ------------------------------------------

# estimate emergence model

emergence <- glm(democracy ~ l.gdp_pc, 
                 data = world_democ0, 
                 na.action = na.exclude,
                 family = binomial(link = "probit"))


summary(emergence)


# estimate full emergence model

emergence_full <- glm(democracy ~ l.gdp_pc + l.lifeexp + l.enrl_gross, 
                      data = world_democ0, 
                      na.action = na.exclude,
                      family = binomial(link = "probit"))

summary(emergence_full)



# estimate survival model

survival <- glm(democracy ~ l.gdp_pc, 
                  data = world_democ1, 
                  na.action = na.exclude,
                  family = binomial(link = "probit"))

summary(survival)


# estimate full survival model

survival_full <- glm(democracy ~ l.gdp_pc + l.lifeexp + l.enrl_gross, 
                       data = world_democ1, 
                       na.action = na.exclude,
                       family = binomial(link = "probit"))


summary(survival_full)

############################
# ROC Curves
############################

# ------------------------------------------
# Emergence
# ------------------------------------------

prob_em <- predict(emergence, type="response")
world_democ0$prob_em <- unlist(prob_em)

roc <- roc(world_democ0$democracy, world_democ0$prob_em)

auc(roc)
plot(roc, print.auc=TRUE)


# plot with ggplot2
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
           size = 6,family = "Fira Math") +
  xlab("Specificity")+
  ylab("Sensitivity")+
  theme_classic() +
  coord_fixed()

# ------------------------------------------
# Full Emergence
# ------------------------------------------

prob_em_full <- predict(emergence_full, type="response")
world_democ0$prob_em_full <- unlist(prob_em_full)

roc <- roc(world_democ0$democracy, world_democ0$prob_em_full)

auc(roc)
plot(roc, print.auc=TRUE) 

# ------------------------------------------
# Survival
# ------------------------------------------

prob_sur <- predict(survival, type="response")
world_democ1$prob_sur <- unlist(prob_sur)

roc <- roc(world_democ1$democracy, world_democ1$prob_sur)

auc(roc)
plot(roc, print.auc=TRUE)

# ------------------------------------------
# Full Survival
# ------------------------------------------

prob_sur_full <- predict(survival_full, type="response")
world_democ1$prob_sur_full <- unlist(prob_sur_full)

roc <- roc(world_democ1$democracy, world_democ1$prob_sur_full)

auc(roc)
plot(roc, print.auc=TRUE)




###########################################
# JOINT ESTIMATION WORKSHEET
###########################################

world$gdp_pc_l.democracy <- world$l.democracy * world$gdp_pc


joint <- glm(democracy ~ gdp_pc + l.democracy + gdp_pc_l.democracy, 
                data = world, 
                na.action = na.exclude,
                family = binomial(link = "probit"))


summary(joint)


# ------------------------------------------
# WALD TEST FOR JOINT ESTMATION
# ------------------------------------------


# http://r-survey.r-forge.r-project.org/survey/html/regTermTest.html

library(survey)
regTermTest(joint, ~gdp_pc+gdp_pc_l.democracy, method="Wald")



