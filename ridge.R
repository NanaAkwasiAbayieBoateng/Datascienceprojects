# STAT 849, Theory and Applied Regression and ANOVA I
#  Fifteenth discussion, 12-14-2012
#  TA: Guilherme Ludwig
#
# Do yourself a favor and buy this book (Amazon has it on sale every now and then):
# http://www-stat.stanford.edu/~tibs/ElemStatLearn/
#
# The following dataset is from Hastie, Tibshirani and Friedman (2009), from a study 
# by Stamey et al. (1989) of prostate cancer, measuring the correlation between the level 
# of a prostate-specific antigen and some covariates. The covariates are
#
# * lcavol  : log-cancer volume
# * lweight : log-prostate weight
# * age     : age of patient
# * lbhp    : log-amount of benign hyperplasia
# * svi     : seminal vesicle invasion
# * lcp     : log-capsular penetration
# * gleason : Gleason Score, check http://en.wikipedia.org/wiki/Gleason_Grading_System
# * pgg45   : percent of Gleason scores 4 or 5
#
# And lpsa is the response variable, log-psa.

url <- "http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.data"
str(pcancer <-read.table(url, header=TRUE))

# There's a training sub-dataset that we will focus on. Later, we will try to predict
# the values of the remaining observations.

train <- pcancer[which(pcancer$train),1:9]
calibrate <- pcancer[-which(pcancer$train),1:9]

# The data looks like this

plot(train)

# Of course, given that this is a biological dataset, the covariates are strongly related

round(cor(train),3)

# We fit a linear model and now focus on fixing multicollinearity

model.ls <- lm(lpsa ~ ., data=train)
rss.ls <- sum(model.ls$resid^2)/model.ls$df.residual

# #######################
# # STEPWISE REGRESSION # 
# #######################
#
# This method was shown weeks ago, but I'll reproduce it here for 
# comparison. 

model.backward <- step(model.ls, direction="backward")
rss.backward <- sum(model.backward$resid^2)/model.backward$df.residual

# So backward selection using AIC drops "gleason" from the model. The final AIC
# is -39.103.
#
# Note that for scope, the formula ~. means "as current model". Forward selection
# is poorly explained in R... 

scope <- list(upper=~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, lower=~.)
model.forward <- step(lm(lpsa ~ 1, data=train), scope, direction="forward")
rss.forward <- sum(model.forward$resid^2)/model.forward$df.residual

# So we conclude that forward selection using AIC keeps lcavol, lweight, svi and lbph.
# The AIC is -37.825.
#
# Note that the paths are arbitrary and such model selection procedures are called 
# "myopic" sometimes, unlike Lasso Regression which has the "Oracle" property. For example,
# we may compute r^2 for all possible subsets.

r2 <- list()
AICs <- list()
for(i in 1:8){
  indexes <- combn(8,i)
  currentr2 <- NULL
  currentAIC <- NULL
  for(j in 1:dim(indexes)[2]){
    temp.model <- lm(lpsa ~ ., data=train[,c(indexes[,j], 9)])
    currentr2[j] <- summary(temp.model)$r.squared
    currentAIC[j] <- AIC(temp.model)
  }
  r2[[i]] <- currentr2
  AICs[[i]] <- currentAIC
}

# let me find the corresponding r^2 and AIC entries for the paths chosen by
# backward and forward elimination... this code is a bit clumsy but it gets
# what we need.

compare <- function(set){
  s <- length(set)
  temp <- combn(8,s)
  check <- NULL
  for(i in 1:dim(temp)[2]){
    check[i] <- all(temp[,i]==set)
  }
  return(which(check))
}

backward <- compare(c(1:6,8))
forward <- c(compare(1), compare(1:2), compare(c(1,2,5)), compare(c(1,2,4,5)))

r2.b <- c(r2[[7]][backward], r2[[8]])
r2.f <- c(r2[[1]][forward[1]], r2[[2]][forward[2]], r2[[3]][forward[3]], r2[[4]][forward[4]])
AICs.b <- c(AICs[[7]][backward], AICs[[8]])
AICs.f <- c(AICs[[1]][forward[1]], AICs[[2]][forward[2]], AICs[[3]][forward[3]], AICs[[4]][forward[4]])

# We now can take a look at how backward/forward performs!

x11(width=10, height=5)
layout(matrix(1:2, ncol=2))
plot(0, xlim=c(0,9), ylim=c(0,0.8), type="n", ylab=expression(r^2), main="Fitting criteria")
for(i in 1:8){
  points(rep(i, length(r2[[i]])), r2[[i]], pch=21, bg="Grey")
}
points(7:8, r2.b, bg="Red", col="Red", pch=21, type="o")
points(1:4, r2.f, bg="Blue", col="Blue", pch=21, type="o")
plot(0, xlim=c(0,9), ylim=c(153,217), type="n", ylab="AIC", main="AIC")
for(i in 1:8){
  points(rep(i, length(AICs[[i]])), AICs[[i]], pch=21, bg="Grey")
}
points(7:8, AICs.b, bg="Red", col="Red", pch=21, type="o")
points(1:4, AICs.f, bg="Blue", col="Blue", pch=21, type="o")

# ####################
# # RIDGE REGRESSION # 
# ####################
#
# This method was shown in the previous discussion, but I'll reproduce it here for 
# comparison. 

library(car)
model.ridge <- lm.ridge(lpsa ~ ., data=train, lambda = seq(0,10,0.1))

plot(seq(0,10,0.1), model.ridge$GCV, main="GCV of Ridge Regression", type="l", 
     xlab=expression(lambda), ylab="GCV")

# The optimal lambda is given by

lambda.ridge <- seq(0,10,0.1)[which.min(model.ridge$GCV)]

# We can plot the coefficients and see how they vary as a function of lambda

colors <- rainbow(8)

matplot(seq(0,10,0.1), coef(model.ridge)[,-1], xlim=c(0,11), type="l",xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors, lty=1, lwd=2, main="Ridge coefficients")
abline(v=lambda.ridge, lty=2)
abline(h=0, lty=2)
text(rep(10, 9), coef(model.ridge)[length(seq(0,10,0.1)),-1], colnames(train)[-9], pos=4, col=colors)

beta.ridge <- coef(model.ridge)[which.min(model.ridge$GCV),]
resid.ridge <- train$lpsa - beta.ridge[1] - as.matrix(train[,1:8])%*%beta.ridge[2:9]

# To find df
d <- svd(as.matrix(train[,1:8]))$d
df <- 67 - sum(d^2/(lambda.ridge+d^2))

rss.ridge <- sum(resid.ridge^2)/df

# ####################
# # LASSO REGRESSION # 
# ####################
#
# In Lasso Regression, the coefficients are penalized by the L1 norm. The 
# optimal value for lambda is chosen by cross-validation.
#
# If necessary,
# install.packages("lars")

library(lars)
y <- as.numeric(train[,9])
x <- as.matrix(train[,1:8])
model.lasso <- lars(x, y, type="lasso")
lambda.lasso <- c(model.lasso$lambda,0)
beta <- coef(model.lasso)

# If you want fancier colors, try
#
# library(colorspace)
# colors <- rainbow_hcl(8, c = 65, l = 65)
#
# I'm not kidding, presenting your data clearly very important.

colors <- rainbow(8)

matplot(lambda.lasso, beta, xlim=c(8,-2), type="o", pch=20, xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors)
text(rep(-0, 9), beta[9,], colnames(x), pos=4, col=colors)
abline(v=lambda.lasso[4], lty=2)
abline(h=0, lty=2)

# It may help visualization if you plot using the scaled X

beta.scale <- attr(model.lasso$beta, "scaled:scale")
beta.rescaled <- beta
for(j in 1:9){
  beta.rescaled[j,] <- beta.rescaled[j,]*beta.scale
}

matplot(lambda.lasso, beta.rescaled, xlim=c(8,-2), type="o", pch=20, xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=colors)
text(rep(-0, 9), beta.rescaled[9,], colnames(x), pos=4, col=colors)
abline(v=lambda.lasso[4], lty=2)
abline(h=0, lty=2)

# I'll keep the lambda=1.7305 betas

beta.lasso <- beta[4,]
resid.lasso <- train$lpsa - predict(model.lasso, as.matrix(train[,1:8]), s=4, type="fit")$fit
rss.lasso <- sum(resid.lasso^2)/(67-4)

# #########################
# # PARTIAL LEAST SQUARES #
# #########################
#
# Partial Least Squares is pretty much like principal components regression, but
# we use information from Y to select weights for the principal components of X.

library(pls)
model.pls <- plsr(lpsa ~ ., 8, data = train, method = "oscorespls", validation = "CV")
summary(model.pls)

# I'm eyeballing CV here, but fitting 4 components should be enough. So I'll update
# the model

model.pls <- plsr(lpsa ~ .,4, data = train, method = "oscorespls")
summary(model.pls)

beta.pls <- drop(coef(model.pls))
resid.pls <- drop(model.pls$resid)[,4]
rss.pls <- sum(resid.pls^2)/(67-4)

# #########################
# # COMPARISON OF FITTING #
# #########################
#
# This is as straightforward as it gets:

rss.ls
rss.backward
rss.forward
rss.ridge
rss.lasso
rss.pls

# ############################
# # COMPARISON OF PREDICTION #
# ############################
#
# We can also compare with the predicition dataset we saved from before. In this case

y.new <- calibrate$lpsa

pss.ls <- sum((y.new - predict(model.ls, calibrate[,1:8]))^2)
pss.backward <- sum((y.new - predict(model.backward, calibrate[,1:8]))^2)
pss.forward <- sum((y.new - predict(model.forward, calibrate[,1:8]))^2)
pss.ridge <- sum((y.new - beta.ridge[1] - as.matrix(calibrate[,1:8])%*%beta.ridge[2:9])^2)
pss.lasso <- sum((y.new - predict(model.lasso, as.matrix(calibrate[,1:8]), s=4, type="fit")$fit)^2)
pss.pls <- sum((y.new - drop(predict(model.pls, calibrate[,1:8], 4)))^2)

pss.ls
pss.backward
pss.forward
pss.ridge
pss.lasso
pss.pls

# In this case Forward AIC-stepwise Regression did the best job at predicting, followed by Lasso, then 
# Ridge regression.
#
# ##################
# # FINAL PROJECT! #
# ##################
#
# Remember:
#
# * Provide means of model comparison (e.g. Prediction Error), and be careful to rescale the response to 
#   its original unit.
#
# * Provide confidence intervals for the parameters, use bootstrap when unsure of normality.
#
# * If you make a claim, provide graphics and tables to justify it. 
#
# * Don't show unnecessary code.
#
# * Label your graphics properly, make axes comparable, use color when possible.
#
# * Reference and explain graphics in text, don't throw graphics and expect the reader to know what
#   they're for.
#
# * Try to use graphics instead of tables when dimensions of the tables are too large. On the other hand
#   don't use graphics to summarize simple things (e.g. no pie-charts of sex showing male 48% female 52%),
#   use tables or explain it in text.
#
# * Only show as much digits as you need to compare things, depending on scale.
#
# * Write down your model and assumptions. Explain the covariates. Justify your choice of methods.
#
# * Anticipate the reader questions when writing. The presentation will give you some idea of what 
#   those questions can be.




# RIDGE REGRESSION

# Model is E(Y) = 0 + 1 X1 + 1 X2 + e   with e~N(0,1)
# Three variables are measured: x1,x2,x3.  
#  x1 and x1 are U(0,1); x3=10 * X1 + unif(0,1).  
#   This causes corr(X1,X3)=sqrt(100/101)=0.995.
# We will fit OLS and ridge regressions to these data, 
#  use the data to select the "best" constant to add, 
#  and then evaluate the two regressions on a new test set.

# Ridge regression function, ridge.lm(), is on MASS package

library(MASS)

# Generating the data

set.seed(558562316)
N <- 20      # Sample size

x1 <- runif(n=N)
x2 <- runif(n=N)
x3 <- runif(n=N)
x3c <- 10*x1 + x3 # New variable
ep <- rnorm(n=N)
y <- x1 + x2 + ep 

# OLS fit of 3-variable model using independent x3
ols <- lm(y~ x1 + x2 + x3)
summary(ols)


# OLS fit of 3-variable model using correlated x3.
olsc <- lm(y~ x1 + x2 + x3c)
summary(olsc)


# Ridge regression using independent variables
ridge <- lm.ridge (y ~ x1+x2+x3, lambda = seq(0, .1, .001))
summary(ridge)
plot(ridge)

# Ridge regression using correlated variables
ridgec <- lm.ridge (y ~ x1+x2+x3c, lambda = seq(0, .1, .001))
plot(ridgec)
select(ridgec)

# Selection of constant is at endpoint.  Extend endpoint and try again
ridgec <- lm.ridge (y ~ x1+x2+x3c, lambda = seq(0, 1, .1))
plot(ridgec)
select(ridgec)

# Selection of constant is at endpoint.  Extend endpoint and try again
ridgec <- lm.ridge (y ~ x1+x2+x3c, lambda = seq(0, 10, .01))
plot(ridgec)
select(ridgec)

# Final model uses lambda=4.
ridge.final <- lm.ridge (y ~ x1+x2+x3c, lambda = 4)
ridge.final
summary(ridge.final)
# Create test data and compute predicted values for OLS and ridge.
#  There's no predict() method for "ridgelm" objects

test <- expand.grid(x1 = seq(.05,.95,.1), x2 = seq(.05,.95,.1), x3=seq(.05,.95,.1))
mu <- test$x1 + test$x2
test$x3c <- 10*test$x1 + test$x3
pred.ols <- predict(ols,newdata=test)   # y ~ X1 + X2 + X3
pred.olsc <- predict(olsc,newdata=test) # y ~ X1 + X2 + X3c
pred.ridge <- coef(ridge.final)[1] + coef(ridge.final)[2]*test[,1] + 
  coef(ridge.final)[3]*test[,2] + coef(ridge.final)[4]*test[,4]

MSPE.ols <- sum((pred.ols - mu)^2)/length(mu)
MSPE.olsc <- sum((pred.olsc - mu)^2)/length(mu)
MSPE.ridge <- sum((pred.ridge - mu)^2)/length(mu)

MSPE.ols
MSPE.olsc
MSPE.ridge





# First I'll bring in the necessary libraries.  
# (Note: It may be that not all of these are 
# needed.) 
library(class)
library(MASS)
library(Hmisc)
library(classPP)
library(klaR)
library(e1071) 
library(kknn)
library(rpart)
library(boost)
library(mvtnorm)          
library(multinomRob )   
library(lars)
library(stats)
library(leaps)

#
# I like to set the seed when using randomly-generated
# data --- it's useful to be able to obtain the same 
# samples when trouble shooting, and it's good to be 
# able to have it so that others can reproduce the
# results. 
set.seed(632)

#
# I'll generate the training data.
x1 <- runif(50, 0, 1)
x2 <- x1 + rnorm(50, 0, 0.25)
x3 <- (x1 + x2)/2 + runif(50, 0, 0.1)
x4 <- runif(50, 0, 1)
x5 <- (2*x4 + rnorm(50, 0, 0.25))/2 + runif(50, 0, 0.1)
x6 <- runif(50, 0, 1)
y <- (3 + x1 + x2 + 0.5*x3 + 0.75*x4 + 0.5*x5 + 0.5*x6 + rnorm(50, 0, 1))

#
# Since I'm going to use some methods for which
# standardized predictors are generally preferred,
# to keep things simpler, I'll use standardized 
# predictors with all of the methods.
x <- scale( cbind(x1,x2,x3,x4,x5,x6) )
trdata <- data.frame( cbind(x,y) )
names(trdata) <- c("sx1", "sx2", "sx3", "sx4", "sx5", "sx6", "y")
attach(trdata)
cor(trdata)

#
# I'll use OLS to fit a linear model
# using all of the (standardized) predictors.
ols1 <- lm(y ~ sx1 + sx2 + sx3 + sx4 + sx5 + sx6)
summary(ols1)

#
# I'll use a backwards elimination strategy
# and fit a model with sx6 omitted.
ols2 <- lm(y ~ sx1 + sx2 + sx3 + sx4 + sx5)
summary(ols2)
#
# I'll now drop sx4.
ols3 <- lm(y ~ sx1 + sx2 + sx3 + sx5)
summary(ols3)
#
# Next, I'll drop sx2.
ols4 <- lm(y ~ sx1 + sx3 + sx5)
summary(ols4)
#
# Now, I'll drop sx3.
ols5 <- lm(y ~ sx1 + sx5)
summary(ols5)

#
# At this point I'll stop with the backwards
# elimination, and check to see what a stepwise
# procedure gives me.
ols6 <- step(ols1, direction="both")
summary(ols6)
# So, the stepwise regression based on AIC led
# to the same model as backwards elimination.
#
# If one wanted to pursue OLS models more, 
# one could use
#   bestss <- regsubsets(y ~ sx1 + sx2 + sx3 + sx4 + sx5 + sx6, data=trdata)
#   summary.regsubsets(bestss)
# to see that backwards elimination did not 
# even arrive at the best two variable model.
# But to deal sensibly with the output of a
# best subsets regression routine, one needs a
# way to compare the goodness of models having
# different numbers of variables.  (I'll say
# more about this later in class.)

#
# Perform lasso using lars funstion,
# noting that the syntax is different
# --- one needs to give it the design 
# matrix.
las <- lars(x, y, type="lasso")
las
plot(las, plottype="coefficients")

#
plot(las, plottype="Cp")
# The C_p seems to be minimized at about 5.6.

#
# By using cross-validation with the lasso,
# a good (hopefully near-optimal) value for
# the "fraction" can be determined.
cvlas <- cv.lars(x, y, type="lasso")
cvlas
frac <- cvlas$fraction[which.min(cvlas$cv)]
frac
las.coef <- predict.lars(las, type="coefficients", mode="fraction", s=frac)
las.coef
# I got the same results when using the
# unstandardized predictors.
#
# As a check, let's see if setting the value of
# s (the fraction, in the mode being used) to 1
# yields the coefficient values from the OLS fit.
las.coef <- predict.lars(las, type="coefficients", mode="fraction", s=1)
las.coef
ols1
# They match!

#
# Now, I'll try ridge regression ---
# I'll give it a sequence of values 
# for lambda, since it's not clear at
# this point what value to use.
lmridge <- lm.ridge(y ~ sx1 + sx2 + sx3 + sx4 + sx5 + sx6, lambda = seq(0, 10, 1))
lmridge$kHKB
lmridge$kLW
lmridge$GCV
# Two estimates of (a good) lambda are
# about 2.7 and 4.0, and generalized
# cross-validation suggests using a 
# value between 6 and 8.  <I'll skip
# showing some of the work I did to
# narrow in on the interval (6.72, 6.84).>
lmridge <- lm.ridge(y ~ sx1 + sx2 + sx3 + sx4 + sx5 + sx6, lambda = seq(6.72, 6.84, 0.01))
lmridge$GCV

#
# After some searching, I determined that 
# the value of lambda which minimizes the
# generalized cross-validation estimate of
# the error is about 6.8.  Two estimation
# methods produced estimated lambdas of 
# about 2.7 and 4.0.  So I will fit three
# ridge regression models, using lambdas 
# values of 2.7, 4.0, and 6.8.  As a check,
# I will fit a fourth model using 0 for lambda,
# which should be the same as OLS.
ridge1 <- lm.ridge(y ~ sx1 + sx2 + sx3 + sx4 + sx5 + sx6, lambda = 2.7)
ridge2 <- lm.ridge(y ~ sx1 + sx2 + sx3 + sx4 + sx5 + sx6, lambda = 4.0)
ridge3 <- lm.ridge(y ~ sx1 + sx2 + sx3 + sx4 + sx5 + sx6, lambda = 6.8)
ridge4 <- lm.ridge(y ~ sx1 + sx2 + sx3 + sx4 + sx5 + sx6, lambda = 0)
# Let's look to see if the fitted coefficients 
# match those from the OLS fit.
ols1
ridge4$coef
# They are rather close, but not exactly
# the same.

#
# Now I will do principal components regression.
#
# The first step is to use the standardized training 
# data to determine the principal components.
tr.pca <- princomp(~ sx1 + sx2 + sx3 + sx4 + sx5 + sx6)
# (Note: Normally, one might put cor=T, but here
# the variables have already been standardized.)
summary(tr.pca)
plot(tr.pca)
loadings(tr.pca)

# 
# The next step is to compute the p.c. variable
# values from the training data.
pcomp.tr <- predict(tr.pca)
pcomp.tr

#
# Now I will "pick off" the p.c. variable vectors.
# (Note: One wouldn't have to do it this way.)
pc1 <- pcomp.tr[,1]
pc2 <- pcomp.tr[,2]
pc3 <- pcomp.tr[,3]
pc4 <- pcomp.tr[,4]
pc5 <- pcomp.tr[,5]
pc6 <- pcomp.tr[,6]
#
# And now I will do the regressions.
# I'll start by using all of the p.c's,
# which should give me a fit equivalent 
# to the OLS fit based on all of the var's.
pcr1 <- lm(y ~ pc1 + pc2 + pc3 + pc4 + pc5 + pc6)
summary(pcr1)
#
# I'll use backwards elimination, and first omit pc3.
pcr2 <- lm(y ~ pc1 + pc2 + pc4 + pc5 + pc6)
summary(pcr2)
#
# I'll now omit pc5.
pcr3 <- lm(y ~ pc1 + pc2 + pc4 + pc6)
summary(pcr3)
#
# And finally I'll omit pc6.
pcr4 <- lm(y ~ pc1 + pc2 + pc4)
summary(pcr4)

#
# I'll next see what a stepwise procedure gives me.
pcr5 <- step(pcr1)
summary(pcr5)
# So, the stepwise regression based on AIC led
# to the same model as backwards elimination.
# So, I'll go with th emodel based on the 1st,
# 2nd, and 4th p.c's, and use the one based on
# all of the p.c's as a check (below).

#
# I will generate a large data set that can be
# used to estimate the generalization errors.
gx1 <- runif(5000, 0, 1)
gx2 <- gx1 + rnorm(5000, 0, 0.25)
gx3 <- (gx1 + gx2)/2 + runif(5000, 0, 0.1)
gx4 <- runif(5000, 0, 1)
gx5 <- (2*gx4 + rnorm(5000, 0, 0.25))/2 + runif(5000, 0, 0.1)
gx6 <- runif(5000, 0, 1)
gy <- (3 + gx1 + gx2 + 0.5*gx3 + 0.75*gx4 + 0.5*gx5 + 0.5*gx6 + rnorm(5000, 0, 1))

#
# To use this data with the previously
# fit models to get sensible predictions,
# it has to be standardized in the same
# way --- the sample means and sample 
# variances from the training data need 
# to be used!
sgx1 <- (gx1 - mean(x1))/sqrt(var(x1))
sgx2 <- (gx2 - mean(x2))/sqrt(var(x2))
sgx3 <- (gx3 - mean(x3))/sqrt(var(x3))
sgx4 <- (gx4 - mean(x4))/sqrt(var(x4))
sgx5 <- (gx5 - mean(x5))/sqrt(var(x5))
sgx6 <- (gx6 - mean(x6))/sqrt(var(x6))
gx <- cbind(sgx1,sgx2,sgx3,sgx4,sgx5,sgx6)
gendata <- data.frame( cbind(gx, gy) )
names(gendata) <- c("sx1", "sx2", "sx3", "sx4", "sx5", "sx6", "y")
detach(trdata)
attach(gendata)

#
# I will plug the new data values into the 
# p.c. formulas determined above.
pcomp.gen <- predict(tr.pca, newdata=gendata)
cor(pcomp.gen)

#
# Now I will estimate the generalization errors.
ols1.mspe <- mean( (gy - predict(ols1, newdata=gendata))^2 )  
ols5.mspe <- mean( (gy - predict(ols5, newdata=gendata))^2 )  
predlas <- predict.lars(las, newx=gx, type="fit", mode="fraction", s=frac)
las1.mspe <- mean( (gy - predlas$fit)^2 )  
predlas <- predict.lars(las, newx=gx, type="fit", mode="fraction", s=0.56)
las2.mspe <- mean( (gy - predlas$fit)^2 )  
predlas <- predict.lars(las, newx=gx, type="fit", mode="fraction", s=1)
las3.mspe <- mean( (gy - predlas$fit)^2 )  
# I can't find a function which obtains predicted values
# using an lm.ridge object, so I will compute them as
# shown below.  (Note: The ym component's value is the
# sample mean of the training sample response values,
# which (see the top of p. 60 of HTF) is the proper
# estimate of the intercept if the predictors were centered.) 
ridge1.mspe <- mean( (gy - (gx %*% ridge1$coef + ridge1$ym))^2 )
ridge2.mspe <- mean( (gy - (gx %*% ridge2$coef + ridge2$ym))^2 )
ridge3.mspe <- mean( (gy - (gx %*% ridge3$coef + ridge3$ym))^2 )
ridge4.mspe <- mean( (gy - (gx %*% ridge4$coef + ridge4$ym))^2 )

#
genpcdata <- data.frame(pcomp.gen)
names(genpcdata) <- c("pc1", "pc2", "pc3", "pc4", "pc5", "pc6")
detach(gendata)
attach(genpcdata)
pcr1.mspe <- mean( (gy - predict(pcr1, newdata=genpcdata))^2 )
pcr4.mspe <- mean( (gy - predict(pcr4, newdata=genpcdata))^2 )

#
# Here are the estimated mean squared prediction errors.
# (I'll start by giving the ones which are equiv. to the
# OLS fit based on all of the predictors.)
ridge4.mspe
### ridge (lambda for OLS equiv.)
las3.mspe
### lasso (fraction for OLS equiv.)
pcr1.mspe
### prin. comp. reg. (using all p.c's --- equiv. to ols1)
ols1.mspe
### OLS (all variables)
ols5.mspe
### OLS (2 var's from backwards elimination (also stepwise))
pcr4.mspe
### prin. comp. reg. (using 3 p.c's)
las1.mspe
### lasso (fraction by c-v)
las2.mspe
### lasso (fraction by C_p)
ridge1.mspe
### ridge (lambda by HKB)
ridge2.mspe
### ridge (lambda by L-W)
ridge3.mspe
### ridge (lambda by gen. c-v)
# The last value is the smallest of them all.
# While the prediction error was only descreased
# by about 8%, it should be kept in mind that 
# there is a relatively large irreducible error
# (due to the variance of the error term).
# If the mean irreducible error, which is equal
# to 1, is subtracted from all of the estimated 
# errors, then this last result indicates that 
# the reducible error was decreased by about 50%.




########Ridge Regression in RR

# load the package
library(glmnet)
# load data
data(longley)
x <- as.matrix(longley[,1:6])
y <- as.matrix(longley[,7])
# fit model
fit <- glmnet(x, y, family="gaussian", alpha=0, lambda=0.001)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, x, type="link")
# summarize accuracy
rmse <- mean((y - predictions)^2)
print(rmse)




###########Least Absolute Shrinkage and Selection Operator (LASSO) in RR


# load the package
library(lars)
# load data
data(longley)
x <- as.matrix(longley[,1:6])
y <- as.matrix(longley[,7])
# fit model
fit <- lars(x, y, type="lasso")
# summarize the fit
summary(fit)
# select a step with a minimum error
best_step <- fit$df[which.min(fit$RSS)]
# make predictions
predictions <- predict(fit, x, s=best_step, type="fit")$fit
# summarize accuracy
rmse <- mean((y - predictions)^2)
print(rmse)




#  Elastic Net in RR


# load the package
library(glmnet)
# load data
data(longley)
x <- as.matrix(longley[,1:6])
y <- as.matrix(longley[,7])
# fit model
fit <- glmnet(x, y, family="gaussian", alpha=0.5, lambda=0.001)
# summarize the fit
summary(fit)









R version 2.9.2 (2009-08-24)
#################
##### ridge regression
#################

> library(MASS)
> pr.ridge = lm.ridge(lpsa ~ ., data = train, lambda = seq(0, 100, length=20))
> plot(pr.ridge)
> pr.ridge.sum = summary(pr.ridge)
> attributes(pr.ridge)
$names
[1] "coef"   "scales" "Inter"  "lambda" "ym"     "xm"     "GCV"    "kHKB"  
[9] "kLW"   

$class
[1] "ridgelm"

> select(pr.ridge)
modified HKB estimator is 3.355691 
modified L-W estimator is 3.050708 
smallest value of GCV  at 5.263158 
> which.min(pr.ridge$GCV)
5.263158 
2 

## the best fit can depend on the range and number of values for lambda

> pr.ridge = lm.ridge(lpsa ~ ., data = train, lambda = seq(0, 10, length=50))
> select(pr.ridge)
modified HKB estimator is 3.355691 
modified L-W estimator is 3.050708 
smallest value of GCV  at 4.897959 
> pr.ridge = lm.ridge(lpsa ~ ., data = train, lambda = seq(0, 100, length=50))
> select(pr.ridge)
modified HKB estimator is 3.355691 
modified L-W estimator is 3.050708 
smallest value of GCV  at 4.081633 
> pr.ridge = lm.ridge(lpsa ~ ., data = train, lambda = seq(0, 60, length=100))
> select(pr.ridge)
modified HKB estimator is 3.355691 
modified L-W estimator is 3.050708 
smallest value of GCV  at 4.848485 
> which.min(pr.ridge$GCV)  
4.8484848 
9 
> pr.ridge = lm.ridge(lpsa ~ ., data = train, lambda = seq(4, 6, length=100))
> select(pr.ridge)
modified HKB estimator is 3.355691 
modified L-W estimator is 3.050708 
smallest value of GCV  at 4.929293   ## this is close to the book's choice of 5
> which.min(pr.ridge$GCV)
4.929293 
47 
> names(train)
[1] "lcavol"  "lweight" "age"     "lbph"    "svi"     "lcp"     "gleason"
[8] "pgg45"   "lpsa"  

# now do predictions
# lm.ridge centers and scales the predictors
# so we do the same 
# and add back in the mean (beta-hat-0)

mse = function(x,y) { mean((x-y)^2)}
msese = function(x,y) { sqrt(var((x-y)^2)) }

ytpredg = scale(test[,1:8],center = F, scale = pr.ridge$scales)%*% pr.ridge$coef[,47] + mean(test$lpsa)  # predict the new yhats

mse(ytpredg, test$lpsa)
 # agrees with Table 3.3
msese(ytpredg,test$lpsa)
  # doesn't agree

### However, I don't get the same coefficients as in Table 3.3
### whether I use my best lambda (4.929) or their choice of 5.

 pr.ridge$coef[,47]

#########################
### predictions for ordinary least squares
#########################
mse(predict(pr.lm,newdata=test[,1:8]),test$lpsa)

 msese(predict(pr.lm,newdata=test[,1:8]),test$lpsa)

summary(pr.best)  ## I fitted this earlier


mse(predict(pr.best,newdata=test[,1:8]),test$lpsa)

 msese(predict(pr.best,newdata=test[,1:8]),test$lpsa)
   ## these agree with Table 3.3

############################
### Lasso
############################

library(lars)  ## in the package "Lars"

pr.lars = lars(as.matrix(train[,1:8]),train$lpsa)
plot(pr.lars)  ## the book states that they chose s=0.36 by 10-fold CV

predict.lars(pr.lars,newx=test[,1:8],type="fit", s=0.36,mode="fraction")



mse(.Last.value$fit,test$lpsa)
  ## bigger than book, but close


######################
#####  Principal components regression
######################

pr.pca = prcomp(train[,1:8])
 round(pr.pca$sdev,3)
  # they are not dropping off
screeplot(pr.pca)    ## this is called a scree plot,plots the eigenvalues
 matplot(1:8,pr.pca$rot[,1:7],type="l")  ## this shows that the derived variables
## are hard to interpret

## the book used 10-fold CV on the training data to choose 7 derived variables

pr.pcalm = lm(train$lpsa ~ pr.pca$x[,1:7])
pr.pcalm$coef  ## these are the theta-hats (p.79)


pr.pca$rot[,1:7] %*% pr.pcalm$coef[-1]  ## these are the beta-hats
## but they don't agree with Table 3.3
## there may be a problem with the way the
## x's were standardized

## below I follow some code from Faraway's book "Linear models with R"
 mm = apply(train[,1:8],2,mean)  # find the means of the training sample
 mm

tx = as.matrix(sweep(test[,1:8],2,mm)) # center the test sample by these means

 rmspr = numeric(8)  # a little loop to find the best number of derived vars

 for(i in 1:8){
  nx = tx %*% pr.pca$rot[,1:i]
  model3 = lm(train$lpsa ~ pr.pca$x[,1:i])
   pv = cbind(1,nx) %*% model3$coef
   rmspr[i] = mse(pv,test$lpsa)}

  ### note that it cheats, by using the test data
  which.min(rmspr)

 min(rmspr)
  ## this agrees with Table 3.3

### trying again using scaling in prcomp

pr.pca2 = prcomp(train[,1:8], scale=TRUE)  # this is recommended in the doc'n
 pr.pca2$sdev

pr.pca$sdev

tx2 = as.matrix(sweep(test[,1:8],2,mm))
mse(cbind(1,tx %*%pr.pca2$rot[,1:7]) %*% lm(train$lpsa ~ pr.pca2$x[,1:7])$coef, test$lpsa)

dim(pr.pca2$rot[,1:7])

 pr.pcalm2 = lm(train$lpsa ~ pr.pca2$x[,1:7])
pr.pcalm2$coef[-1]

pr.pca2$x[, 1:7]PC7 

pr.pca2$rot[,1:7]%*%.Last.value  ## closer
# make predictions
predictions <- predict(fit, x, type="link")
# summarize accuracy
rmse <- mean((y - predictions)^2)
print(rmse)