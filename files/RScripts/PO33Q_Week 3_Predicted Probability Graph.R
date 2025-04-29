#############################################
# PO33Q -  Week 3, Predicted Probability Graph
#############################################

# set working directory
setwd()

# load packages
library(tidyverse)

###########
# DATA PREP
###########

# read in data
world <- read.csv("world.csv")

# filter to one year
world2000 <- filter(world, year==2000)

# estimate probit model
probit1 <- glm(democracy ~ life, 
               family = binomial(link = "probit"), 
               data = world2000)

# look at results of probit model
summary(probit1)

###########
# THE GRAPH
###########

## Preparing the Plot

summary(world2000$life) # look for range of life

xlife <- seq(0, 81.08, 1) # create a sequence of x-values with the range of life

ylife <- predict(probit1, list(life = xlife),type="response")


## If you want it to look all right

plot(xlife, ylife, 
     xlab = "Life Expectancy at Birth", 
     ylab = "Pr(Democracy)", 
     type = "l")

## If you want it to look jazzy, use ggplot. More on this can be found in Chapter 7 on https://drfloreiche.github.io/

# ggplot can only deal with a data frame, so we pass the x values and predictions into one called "predictions"

predictions <- data.frame(xlife,ylife) 

## then we call the ggplot function

ggplot(data=predictions, aes(x=xlife, y=ylife)) +
  geom_line() +
  labs(x= "Life Expectancy at Birth", y="Pr(Democracy)") +
  theme_classic()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))
