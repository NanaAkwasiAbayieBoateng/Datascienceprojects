a <- 1:10
save(a, file="C:/Users/Gucci148/Documents/dumData.Rdata")
rm(a)
load("C:/Users/Gucci148/Documents/dumData.Rdata")
print(a)

install.packages("xlsx")

library(xlsx)
 table(iris$Species)
 head(iris)
 tail(iris)
 
 install.packages("readr")
 library(readr)
 
 pie(table(iris$Species))
 
 boxplot(Sepal.Length ~ Species, data=iris, xlab="Species", ylab="Sepal.Length")
 
 
 
 #############################################
 ## Decision Trees and Random Forest#########
 
 set.seed(1234)
 ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
 trainData <- iris[ind==1,]
 testData <- iris[ind==2,]
 
 iris[sample(iris,100,replace=T),]
 
 
 
 #install.packages("pls")
 set.seed (2)
 library(pls)
 pcr.fit=pcr(Salary∼., data=Hitters ,scale=TRUE ,validation ="CV")
 
 data(Hitters)
 
 
 
 #############################################
 ## layout matrix #########
 
 x <- pmin(3, pmax(-3, stats::rnorm(50)))
 y <- pmin(3, pmax(-3, stats::rnorm(50)))
 xhist <- hist(x, breaks = seq(-3,3,0.5), plot = FALSE)
 yhist <- hist(y, breaks = seq(-3,3,0.5), plot = FALSE)
 top <- max(c(xhist$counts, yhist$counts))
 xrange <- c(-3, 3)
 yrange <- c(-3, 3)
 nf <- layout(matrix(c(2,0,1,3),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)
 layout.show(nf)
 
 
 
 
 par(mar = c(3,3,1,1))
 plot(x, y, xlim = xrange, ylim = yrange, xlab = "", ylab = "")
 par(mar = c(0,3,1,1))
 barplot(xhist$counts, axes = FALSE, ylim = c(0, top), space = 0)
 par(mar = c(3,0,1,1))
 barplot(yhist$counts, axes = FALSE, xlim = c(0, top), space = 0, horiz = TRUE)
 par(mar = c(1,2,2,2))
 barplot(yhist$counts, axes = FALSE, xlim = c(0, top), space = 0, horiz = TRUE)
 
 par(def.par)  #- reset to default
 
 
 
 
 #############################################
 ## read .dat file from a website ######### 
 read.table("http://www.nilu.no/projects/ccc/onlinedata/ozone/CZ03_2009.dat", 
            header=TRUE, skip=3)
 
 
 paste("A", 1, "%")       #A bunch of individual character strings.
 paste(1:4, letters[1:4]) #2 or more strings pasted element for element.
 paste(1:10)
 
 paste0(1:4, letters[1:4])
 paste(1:4, letters[1:4],sep="")
 paste(1:4, letters[1:4],sep=":")
 
 
 
 
 
 #===========================================================    
 #fit many regressions
 #
 #===========================================================  
 
 library(dplyr)
 library(broom)
 
 df.h = data.frame( 
   hour     = factor(rep(1:24, each = 21)),
   price    = runif(504, min = -10, max = 125),
   wind     = runif(504, min = 0, max = 2500),
   temp     = runif(504, min = - 10, max = 25)  
 )
 
 dfHour = df.h %>% group_by(hour) %>%
   do(fitHour = lm(price ~ wind + temp, data = .))
 
 # get the coefficients by group in a tidy data_frame
 dfHourCoef = tidy(dfHour, fitHour)
 dfHourCoef
 
 
 # get the predictions by group in a tidy data_frame
 dfHourPred = augment(dfHour, fitHour)
 dfHourPred
 
 
 # get the summary statistics by group in a tidy data_frame
 dfHourSumm = glance(dfHour, fitHour)
 dfHourSumm