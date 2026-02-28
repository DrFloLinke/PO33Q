########################################
# PO33Q - Determinants of Democracy
# Dr Flo Linke
# Seminar, Week 1
########################################


setwd("")

5+3
result <- 5+3
result

library(readxl)

example <- read_excel("example.xlsx", sheet="Sheet1")

str(example)


library(tidyverse)


example <- example %>% 
mutate(incomecat=
           ordered(
             cut(gdp, breaks=c(0, 20000, Inf), 
                 labels=c("low","high"))))



world <- read.csv("world.csv")
