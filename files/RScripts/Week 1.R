setwd("~/OneDrive - University of Warwick/Warwick/Modules/PO33Q/R/Worksheets/WEEK 1")

5+3
result <- 5+3
result

library(readxl)

example <- read_excel("example.xlsx", sheet="Sheet1")

str(example)


library(dplyr)


example <- example %>% 
mutate(incomecat=
           ordered(
             cut(gdp, breaks=c(0, 20000, Inf), 
                 labels=c("low","high"))))


library(haven)

world <- read_dta("world.dta")
