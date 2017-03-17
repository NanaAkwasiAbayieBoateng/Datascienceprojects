#install.packages("magrittr")  # to use piping %>%
#install.packages("ggplot2")   # for ploting
#install.packages("MASS")      # to calculate the pseudo-inverse of a matrix
#install.packages("caret")     # to center our data by subtracting its mean
#install.packages("reshape2")
#install.packages("minqa")
#install.packages('caret', repos='http://cran.rstudio.com/')
#install.packages("RcppEigen")
#install.packages("RcppEigen") 
#install.packages('caret', repos='http://cran.rstudio.com/')
#install.packages("car")
#install.packages(c('lme4', 'pbkrtest', 'BradleyTerry2', 'car', 'caret'))
#install.packages('caret', dependencies = TRUE)


library(magrittr)  # to use piping %>%
library(ggplot2)   # for ploting
library(MASS)      # to calculate the pseudo-inverse of a matrix
library(caret)     # to center our data by subtracting its mean
library(reshape2)

sum(choose(900,seq(490,900,1)))*(0.5^899)

sum(choose(10,seq(8,10,1)))*(0.5^9)

z=(490-450)/15

2*pnorm(z,lower.tail = F)

z=(8-5)/sqrt(10*.5*.5)
2*pnorm(z,lower.tail = F)

2*pt(z,lower.tail = F,df=9)

z=(0.8-.5)/sqrt((0.5*.5)/10)
2*pnorm(z,lower.tail = F)

2*pt(z,lower.tail = F,df=9)
sum(choose(10,seq(8,10,1)))*(0.5^9)


load("data1.RData")