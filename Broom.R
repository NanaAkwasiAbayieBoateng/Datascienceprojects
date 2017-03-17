#==================================================================
# Install packages not already installed in a list
#==================================================================


list=c("tidyverse","stringr","forcats","ggmap","rvest","tm","SnowballC","dplyr","calibrate"
       ,"stringi","ggplot2","maps","httr","rsdmx","devtools","oec","OECD","plyr","dplyr","ggplot2","caret",
       "magrittr","babynames","acs","choroplethr","choroplethrMaps","broom","glmnet")


list.of.packages <- list

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

sapply(list, require, character.only = TRUE)

#install.packages("tidyverse")
library(broom)
tidyverse_update()

fit <- lm(mpg ~ wt + qsec, mtcars)
summary(fit)

tidy(fit)


head(augment(fit))


glance(fit)

td <- tidy(fit, conf.int = TRUE)
ggplot(td, aes(estimate, term, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept  =0)


mtcars %>% group_by(am) %>% do(tidy(lm(mpg ~ wt, .)))



#we could create a LASSO regression with the glmnet package:
#install.packages("glmnet")
  library(glmnet)

#update 


set.seed(03-19-2015)

# generate data with 5 real variables and 45 null, on 100 observations
nobs <- 100
nvar <- 50
real <- 5
x <- matrix(rnorm(nobs * nvar), nobs)
beta <- c(rnorm(real, 0, 1), rep(0, nvar - real))
y <- c(t(beta) %*% t(x)) + rnorm(nvar, sd = 3)

glmnet_fit <- cv.glmnet(x,y)

tidied_cv <- tidy(glmnet_fit)
glance_cv <- glance(glmnet_fit)

ggplot(tidied_cv, aes(lambda, estimate)) + geom_line(color = "red") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
  scale_x_log10() +
  geom_vline(xintercept = glance_cv$lambda.min) +
  geom_vline(xintercept = glance_cv$lambda.1se, lty = 2)


library(survival)

surv_fit <- survfit(coxph(Surv(time, status) ~ age + sex, lung))

td <- tidy(surv_fit)
ggplot(td, aes(time, estimate)) + geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)