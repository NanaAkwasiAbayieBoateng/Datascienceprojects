

# load needed libraries
library(readr)
#install.packages("ggplot2")
library(caret)
library(ggplot2)
?createDataPartition

# import data
#iris <- read.csv("../input/Iris.csv")


# create a validation dataset

# use 80% of the original data for training
train_index <- createDataPartition(iris$Species, p=0.8, list=FALSE)
train <- iris[train_index,]

# use the remaining 20% of the data for validation
validation <- iris[-train_index,]

# 10-fold cross validation
control <- trainControl(method="cv", number=10)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)


# use accuracy to evaluate the models
metric <- "Accuracy"


# evaluate 5 different algorithms
# 1) Linear Discriminant Analysis
set.seed(33)
fit.lda <- train(Species~., data=train, method="lda", metric=metric, trControl=control)



# use accuracy to evaluate the models
metric <- "Accuracy"


# evaluate 5 different algorithms
# 1) Linear Discriminant Analysis
set.seed(33)
fit.lda <- train(Species~., data=train, method="lda", metric=metric, trControl=control)

# 2) Classification and Regression Trees
set.seed(33)
fit.cart <- train(Species~., data=train, method="rpart", metric=metric, trControl=control)

# importance of variables Regression Trees
varImp(fit.cart )

varImp(fit.cart )$importance[[1]]

sort(varImp(fit.cart )$importance)

dim(as.matrix(varImp(fit.cart )$importance))[,1]

as.matrix(varImp(fit.cart )$importance)[,order$Overall]

p=data.frame(varImp(fit.cart )$importance)
row.names(p)

p[rev(order(p)),]

with(p,rev(order(p)))

ImpMeasure<-data.frame(varImp(fit.cart)$importance)
ImpMeasure$Vars<-row.names(ImpMeasure)
ImpMeasure[order(-ImpMeasure$Overall),][1:3,]

rownames(varImp(fit.cart)$importance)[1:3]



library(dplyr)

col_index <- varImp(fit.cart)$importance %>% 
  mutate(names=row.names(.)) %>%
  arrange(-Overall)
imp_names <- col_index$names[1:3]

str(col_index)
col_index[1:3,]



#plot of  importance of variables Regression Trees

plot(varImp(fit.cart ), top = 4)




roc_imp2 <- varImp(fit.cart, scale = FALSE)
roc_imp2


ggplot(varImp(fit.cart ))

qplot(data=varImp(fit.cart ), geom=c("point", "smooth"))

qplot(data=varImp(fit.cart ), geom=c("line", "smooth"))

qplot(data=varImp(fit.cart ), geom="line")

p <- ggplot(varImp(fit.cart ),p + geom_line())


# 3) k-Nearest Neighbors
set.seed(33)
fit.knn <- train(Species~., data=train, method="knn", metric=metric, trControl=control)
# 4) Support Vector Machines with a linear kernel
set.seed(33)
fit.svm <- train(Species~., data=train, method="svmRadial", metric=metric, trControl=control)



# 5) Random Forest
set.seed(33)
fit.rf <- train(Species~., data=train, method="rf", metric=metric, trControl=control)


#summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)


# compare accuracy of models
dotplot(results)



# estimate skill of LDA on the validation dataset
predictions.lda <- predict(fit.lda, validation)
confusionMatrix(predictions.lda, validation$Species)


# estimate skill of Random Forest on the validation dataset
predictions.rf <- predict(fit.rf, validation)
confusionMatrix(predictions.rf, validation$Species)




# estimate skill of knn on the validation dataset
predictions.knn <- predict(fit.knn, validation)
confusionMatrix(predictions.knn, validation$Species)



# estimate skill of svm on the validation dataset
predictions.svm <- predict(fit.svm, validation)
confusionMatrix(predictions.svm, validation$Species)


# estimate skill of Classification and Regression Trees on the validation dataset
predictions.cart <- predict(fit.cart, validation)
confusionMatrix(predictions.cart, validation$Species)



library(nycflights13)
library(dplyr)
dim(flights)
head(flights)

filter(flights, month == 1, day == 1)

#This is equivalent to the more verbose code in base R:
flights[flights$month == 1 & flights$day == 1, ]
subset(flights,flights$month == 1 & flights$day == 1)
filter(flights, month == 1 | month == 2)


#To select rows by position, use slice():
slice(flights, 1:10)
flights[1:10,]


#arrange() works similarly to filter() except that instead 
#of filtering or selecting rows, it reorders them. It takes a data frame, 
#and a set of column names (or more complicated expressions) to order by
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))

flights[,order(flights$arr_delay)]


###equivalent
flights[order(flights$year, flights$month, flights$day), ]
flights[order(flights$arr_delay, decreasing = TRUE), ] or flights[order(-flights$arr_delay), ]





dd <- data.frame(b = factor(c("Hi", "Med", "Hi", "Low"), 
                            levels = c("Low", "Med", "Hi"), ordered = TRUE),
                 x = c("A", "D", "A", "C"), y = c(8, 3, 9, 9),
                 z = c(1, 1, 1, 2))

dd[ order(-dd[,4], dd[,1]), ]

dd[order(dd$b),]

arrange(dd,b)



# Select columns by name
select(flights, year, month, day)


# Select all columns between year and day (inclusive)
select(flights, year:day)



# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))




#You can rename variables with select() by using named arguments:
select(flights, tail_num = tailnum)



#But because select() drops all the variables not explicitly mentioned,
#it's not that useful. Instead, use rename():
  rename(flights, tail_num = tailnum)
  
  
# Use distinct()to find unique values in a table:
 # Extract distinct (unique) rows
   distinct(flights, tailnum) 
   
   
   distinct(flights, origin, dest)
   
   
   
   
#Add new columns with mutate()  
   
mutate(flights,
          gain = arr_delay - dep_delay,
          speed = distance / air_time * 60)  


d=mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60)
)
d[1:10,21]


#If you only want to keep the new variables, use transmute():
  transmute(flights,
            gain = arr_delay - dep_delay,
            gain_per_hour = gain / (air_time / 60)
            
            
            
            
#Summarise values with summarise()
# The last verb is summarise(). It collapses a data frame to a single 
#row (this is exactly equivalent to plyr::summarise()):
  summarise(flights, delay = mean(dep_delay, na.rm = TRUE)) 
  
  
#Randomly sample rows with sample_n() and sample_frac()
#You can use sample_n() and sample_frac() to take a random sample of rows: 
#use sample_n() for a fixed number and sample_frac() for a fixed fraction.
sample_n(flights, 10)
flights[sample(flights$year,10),]  


#group
by_tailnum <- group_by(flights, tailnum)

unique(flights$tailnum)



by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)

library(ggplot2)
# Interestingly, the average delay is only slightly related to the
# average distance flown by a plane.
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()




destinations <- group_by(flights, dest)
summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n())


daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))



(per_month <- summarise(per_day, flights = sum(flights)))


(per_year  <- summarise(per_month, flights = sum(flights)))

##############################
#####chaining

a1 <- group_by(flights, year, month, day)
a2 <- select(a1 ,arr_delay, dep_delay)
a3 <- summarise(a2,
                arr = mean(arr_delay, na.rm = TRUE),
                dep = mean(dep_delay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)



filter(
  summarise(
    select(
      group_by(flights, year, month, day),
      arr_delay, dep_delay
    ),
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ),
  arr > 30 | dep > 30
)



mtcars %>% tbl_df()
mtcars %>% add_rownames()


scramble <- function(x) x[sample(nrow(x)), sample(ncol(x))]
# By default, ordering of rows and columns ignored
all_equal(mtcars, scramble(mtcars))
scramble(dd)

dd[sample(nrow(dd)), sample(ncol(dd))]

x=1:10
y=4:10
x%>%sum
sum(x)


install.packages(("magrittr"))
library(babynames) # data package
library(dplyr)     # provides data manipulating functions.
library(magrittr)  # ceci n'est pas un pipe
library(ggplot2)   # for graphics

x <- rnorm(1e2)
x[between(x, -1, 1)]



one <- mtcars[1:4, ]
two <- mtcars[11:14, ]
# You can either supply data frames as arguments
bind_rows(one, two)
# Or a single argument containing a list of data frames
bind_rows(list(one, two))
bind_rows(split(mtcars, mtcars$cyl))
df <- data.frame(
  x = sample(10, 100, rep = TRUE),
  y = sample(10, 100, rep = TRUE)
)
nrow(df)
nrow(distinct(df))
nrow(distinct(df, x, y))
distinct(df, x)
distinct(df, y)
# Can choose to keep all other variables as well
distinct(df, x, .keep_all = TRUE)
distinct(df, y, .keep_all = TRUE)
# You can also use distinct on computed variables
distinct(df, diff = abs(x - y))


by_cyl <- group_by(mtcars, cyl)
do(by_cyl, head(., 2))

install.packages("installr")
library(installr)
install.packages("reinstallr")
library(reinstallr)
updateR()
