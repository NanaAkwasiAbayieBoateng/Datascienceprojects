rm(list=ls())
require(ISLR)
require(dplyr)
#install.packages(c("LiblineaR"))


#################################################
### load multiple packages at the same time###

lib=c("ISLR","dplyr","purrr","feather","tree","randomForest","caret","ggplot2","scales",
      "e1071","gbm","LiblineaR","MASS")

#install.packages(list)
#lapply(lib, library, character.only = TRUE)
sapply(lib, require, character.only = TRUE)
## install.packages("pacman")
pacman::p_load(dplyr, psych, tm) 
pacman::p_load(ISLR, dplyr)

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install 
#them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("ggplot2", "plyr", "reshape2", "RColorBrewer", "scales", "grid")
ipak(packages)



library(devtools)
library(lubripack)
#install_github("espanta/lubripack")
lubripack("plyr", "psych", "tm")


#####################################################
# saves data to to the smallest size and reads it faster
# back into r tahn eg csv
####  feather package ##############################


write_feather(mtcars,"/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/mtcars.feather")

mtcars2 <- read_feather("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/mtcars.feather")



#####################################################
####  Tree-Based Methods ##############################

library (tree)
library (ISLR)
attach (Carseats )
?Carseats
dim(Carseats)
head(Carseats )
tail(Carseats )

# convert continouos Sales variable to binary variable
High=ifelse (Sales <=8," No"," Yes ")
Carseats =data.frame(Carseats ,High)
tree.carseats=tree(High~.-Sales,Carseats)
summary (tree.carseats )

#The argument pretty=0 instructs R to include the category 
#names for any qualitative predictors,rather than simply displaying a letter for each category
plot(tree.carseats )
text(tree.carseats ,pretty =0)
text(tree.carseats )

#the argument type="class" instructs R to return
#the actual class prediction
set.seed (2)
train=sample (1: nrow(Carseats ), 200)
Carseats.test=Carseats [-train ,]
High.test=High[-train ]
tree.carseats=tree(High~.-Sales,Carseats,subset =train)
tree.pred=predict(tree.carseats, Carseats.test, type="class")

#  correct predictions rate
# (86+57) /200=0.715
table(tree.pred ,High.test)

summary(tree.carseats)
# cross validation 
#FUN=prune.misclass in order to indicate that we want the
#classification error rate to guide the cross-validation and pruning process,
#rather than the default for the cv.tree() function, which is deviance
# k is the value of the cost-complexity parameter used
#dev corresponds to the cross-validation error rate in this instance
set.seed (3)
cv.carseats =cv.tree(tree.carseats ,FUN=prune.misclass )
names(cv.carseats )

par(mfrow =c(1,2))
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
plot(cv.carseats$k ,cv.carseats$dev ,type="b")

## pruning down trees to 9
prune.carseats =prune.misclass (tree.carseats ,best =9)
plot(prune.carseats )
text(prune.carseats ,pretty =0)

##  correct predictions rate for test data
tree.pred=predict(prune.carseats,Carseats.test,type="class")
tab=table(tree.pred ,High.test)
(tab[1]+tab[4])/sum(tab)

# using 15 trees
prune.carseats=prune.misclass (tree.carseats ,best =15)
plot(prune.carseats )
text(prune.carseats ,pretty =0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
tab=table(tree.pred ,High.test)
(tab[1]+tab[4])/sum(tab)

### Boston Data 
library (MASS)
set.seed (1)
head(Boston)
dim(Boston)
names(Boston)
train = sample (1: nrow(Boston ), nrow(Boston )/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary (tree.boston )

cv.boston =cv.tree(tree.boston )
plot(cv.boston$size ,cv.boston$dev ,type='b')

prune.boston =prune.tree(tree.boston ,best =5)
plot(prune.boston )
text(prune.boston ,pretty =0)


yhat=predict (tree.boston ,newdata =Boston[-train ,])
boston.test=Boston[-train ,"medv"]
#boston.test=Boston[-train ,]
plot(yhat ,boston.test)
abline (0,1)
length(yhat)
length(boston.test)
#the test set MSE associated with the regression tree
mean((yhat -boston.test)^2)

#Bagging and Random Forests
#The argument mtry=13 indicates that all 13 predictors should be considered
#for each split of the tree
# Bagging uses m=p all the avaialable predictors 
# Bagging
set.seed (1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=T)

bag.boston
yhat.bag = predict (bag.boston ,newdata =Boston [-train ,])
plot(yhat.bag , boston.test,pch=19)
abline (0,1)
mean(( yhat.bag -boston.test)^2)

(fit=lm( boston.test~yhat.bag))

plot(fit)
layout(matrix(c(1,2,3,4),2,2))
fitted(fit)

plot(fitted(fit),boston.test,pch=19)
abline(fit)
abline(fit[[1]][[1]],fit[[1]][[2]])
abline(fit$coefficients[1],fit$coefficients[2])


# Random Forest is just like Bagging but uses
# m<p like m=p/3, sqrt(3)
# Random Forest
set.seed (1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6, importance=TRUE)

yhat.rf = predict (rf.boston ,newdata =Boston[-train ,])
 mean(( yhat.rf -boston.test)^2)
 
# Using the importance() function, we can view the importance of each
 #importance() variable.

 VarImp= importance (rf.boston )

(VI_F=importance(rf.boston))

 ##plotting the variable importance

varImpPlot(rf.boston,pch=19)

varImp(rf.boston)
       
varImpPlot(rf.boston,type=2,pch=19)

varImpPlot(rf.boston,type=1,pch=19)

print(varImp(rf.boston, scale = FALSE))

plot(varImp(rf.boston, scale = FALSE), main="Variable Importance")


# Importance plot of each variable
#las=2 displays all names
par(mfrow=c(1, 1))
importanceOrder=order(-varImp(rf.boston)$Overall)
names=rownames(varImp(rf.boston))[importanceOrder]

barplot(sort(varImp(rf.boston)$Overall,decreasing=T),width=1,space=0.01,las=2,
        ,names.arg=names,main="Importance  of each variable",xlab="Variables"
        )
box(lty = '1373', col = 'black')



dat=sort(varImp(rf.boston)$Overall,decreasing = T)
names(dat)=names

barplot(dat,las=2,ylab="Overall",xlab="")
box(lty = '1373', col = 'black')


sortdat=as.data.frame(dat)

end_point = 0.5 + nrow(sortdat) + nrow(sortdat)-1 #this is the line which does the trick (together with barplot "space = 1" parameter)

barplot(sortdat[,1], col="grey50", 
        main="",
        ylab="Overall ", ylim=c(0,5+max(sortdat[,1])),
        xlab = "",
        space=1)
#rotate 60 degrees, srt=60
text(seq(1.5,end_point,by=2), par("usr")[3]-0.25, 
     srt = 60, adj= 1, xpd = TRUE,
     labels = paste(rownames(sortdat)), cex=0.65)

box(lty = '1373', col = 'black')


qplot(sortdat)

str(sortdat)

ggplot(varImp(rf.boston))

qplot(data=varImp(rf.boston ), geom=c("point", "smooth"))

qplot(data=varImp(rf.boston ), geom=c("line", "smooth"))

qplot(data=varImp(rf.boston ), geom="line")

p <- ggplot(varImp(rf.boston ),p + geom_line())

set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
Boston.train <- Boston[train, -14]
Boston.test <- Boston[-train, -14]
Y.train <- Boston[train, 14]
Y.test <- Boston[-train, 14]
rf.boston1 <- randomForest(Boston.train, y = Y.train, xtest = Boston.test, ytest = Y.test, mtry = ncol(Boston) - 1, ntree = 500)
rf.boston2 <- randomForest(Boston.train, y = Y.train, xtest = Boston.test, ytest = Y.test, mtry = (ncol(Boston) - 1) / 2, ntree = 500)
rf.boston3 <- randomForest(Boston.train, y = Y.train, xtest = Boston.test, ytest = Y.test, mtry = sqrt(ncol(Boston) - 1), ntree = 500)
plot(1:500, rf.boston1$test$mse, col = "green", type = "l", xlab = "Number of Trees", ylab = "Test MSE", ylim = c(10, 19))
lines(1:500, rf.boston2$test$mse, col = "red", type = "l")
lines(1:500, rf.boston3$test$mse, col = "blue", type = "l")
legend("topright", c("m = p", "m = p/2", "m = sqrt(p)"), col = c("green", "red", "blue"), cex = 1, lty = 1)


#Boosting
#option distribution="gaussian" since this is a regression problem; if it were a binary
#classification problem, we would use distribution="bernoulli".
#The argument n.trees=5000 indicates that we want 5000 trees, and the option
#interaction.depth=4 limits the depth of each tree.
set.seed (1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian"
                 ,n.trees=5000,interaction.depth = 4)

summary(boost.boston ,las=2)
box(lty = '1373', col = 'black')

dat=summary(boost.boston )[,2]
names(dat)=summary(boost.boston )[,1]

barplot(dat,las=2,ylab="Relative Influence",xlab="")
box(lty = '1373', col = 'black')

# partial dependence plots
par(mfrow =c(1,2))
plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")


yhat.boost=predict (boost.boston ,newdata =Boston[-train ,],
                    n.trees =5000)
mean(( yhat.boost-boston.test)^2)

# the shrinkage parameter λ
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian"
                 ,n.trees=5000,interaction.depth = 4,shrinkage = 0.2,verbose = F)

yhat.boost=predict (boost.boston ,newdata =Boston [-train ,],
                      n.trees =5000)
mean(( yhat.boost-boston.test)^2)



library(gbm)
set.seed(1)
Hitters <- na.omit(Hitters)
Hitters$Salary <- log(Hitters$Salary)
train <- 1:200
Hitters.train <- Hitters[train, ]
Hitters.test <- Hitters[-train, ]
pows <- seq(-10, -0.2, by = 0.1)
lambdas <- 10^pows
train.err <- rep(NA, length(lambdas))
for (i in 1:length(lambdas)) {
  boost.hitters <- gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
  pred.train <- predict(boost.hitters, Hitters.train, n.trees = 1000)
  train.err[i] <- mean((pred.train - Hitters.train$Salary)^2)
}
plot(lambdas, train.err, type = "b", xlab = "Shrinkage values", ylab = "Training MSE")

###################################################################
### # Chapter 9 Lab: Support Vector Machines

# SVM with Multiple Classes
# Support Vector Classifier

set.seed(1)
x=matrix (rnorm (20*2) , ncol =2)
y=c(rep (-1,10) , rep (1 ,10) )
x[y==1 ,]= x[y==1,] + 1
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(2,1))
plot(x,col=(3-y))
plot(x)
#A cost argument allows us to specify the cost of
#a violation to the margin. When the cost argument is small, then the margins
#will be wide and many support vectors will be on the margin or will
#violate the margin. When the cost argument is large, then the margins will
#be narrow and there will be few support vectors on the margin or violating
#the margin.
#Note that in order
#for the svm() function to perform classification (as opposed to SVM-based
#regression), we must encode the response as a factor variable
#The argument scale=FALSE tells the svm() function not to scale each feature
#to have mean zero or standard deviation one; depending on the application,
#one might prefer to use scale=TRUE.

#linearly inseparable example
svmfit=svm(y~., data=dat, kernel="linear", cost=10,scale=F)
plot(svmfit, dat)
svmfit$index
summary (svmfit )
svmfit=svm(y~., data=dat, kernel="radial", cost=10,scale=T)
plot(svmfit, dat)

#using a smaller cost argument
svmfit=svm(y~., data=dat, kernel="linear", cost=0.1,scale=FALSE)
plot(svmfit, dat)
svmfit$index


# using cross validation to pick the best cost argument
set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)


# predicting the  class labels with the best cost
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
ypred=predict(bestmod,testdat)
table(predict=ypred, truth=testdat$y)

svmfit=svm(y~., data=dat, kernel="linear", cost=.01,scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred, truth=testdat$y)

# two classes are linearly separable example
set.seed(1)
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)
svmfit=svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit,dat)