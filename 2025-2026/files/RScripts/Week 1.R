########################################
# PO33Q - Determinants of Democracy
# Dr Flo Linke
# Seminar, Week 1
########################################

# -----------------------
# set working directory
# -----------------------

setwd("")

# -----------------------
# Basic operations
# -----------------------

5+3

result <- 5+3
result

# -----------------------
# Load Required Packages
# -----------------------

library(readxl)
library(tidyverse)

# -----------------------
# Load and inspect data
# -----------------------

example <- read_excel("example.xlsx", sheet="Sheet1")

str(example)

# -----------------------
# Create income category
# -----------------------

example <- example %>% 
  mutate(incomecat =
           ordered(
             cut(gdp, breaks=c(0, 20000, Inf), 
                 labels=c("low","high"))))

# -----------------------
# Load world dataset
# -----------------------

world <- read.csv("world.csv")