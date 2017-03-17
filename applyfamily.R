library(spuRs)
data(ufc)
#install.packages("CorrBin")
library(lattice)
library(CorrBin)

ufc
#input data
data(egde)
edge <- egde 

tapply(ufc$height.m, ufc$species, mean)

##########################################
##    by function
#########################################

by(data, INDICES, FUN, ..., simplify = TRUE)

by(ufc[,5], ufc["species"], function(x) mean(ufc[,5]))


p=split(edge,edge$Trt)

sum(p[[1]]$ClusterSize*p[[1]]$Freq)/sum(p[[1]]$Freq)

by(edge, edge["Trt"], function(x)sum(x$ClusterSize*x$Freq)/sum(x$Freq))

by(airquality[,1:3], Month, function(x) mean(airquality[,1:3],na.rm=TRUE) )


require(stats)
by(warpbreaks[, 1:2], warpbreaks[,"tension"], summary)
by(warpbreaks[, 1],   warpbreaks[, -1],       summary)
c=by(warpbreaks, warpbreaks[,"tension"],
   function(x) lm(breaks ~ wool, data = x))

## now suppose we want to extract the coefficients by group
tmp <- with(warpbreaks,
            by(warpbreaks, tension,
               function(x) lm(breaks ~ wool, data = x)))
sapply(tmp, coef)

sapply(c, coef)

ufc[c("plot", "species")]









##########################################
##    elt function
#########################################
??elt
x<-list(a=matrix(1:4,2,2),b=matrix(1:6,3,2))

lapply(x,function(elt) elt[,1])

lapply(x,function(elt) elt[,2])

x<-matrix(rnorm(200),20,20)
colMeans(x)=apply(x,2,mean)

colSums(x)=apply(x,2,sum)
rowSums(x)=apply(x,1,sum)
rowMeans(x)=apply(x,1,mean)


apply(x,1,quantile,probs=c(0.25,0.75))

a<-array(rnorm(2*2*2),c(2,2,10))

apply(a,c(1,2),mean)

rowMeans(a,dim=2)

x<-c(rnorm(10),runif(10),rnorm(10,1))
f<-gl(3,10)
f
tapply(x,f,mean)

split(x,f)



##########################################
##   split  function
#########################################
library(datasets)
head(airquality)

s<-split(airquality,airquality$Month)

split(x,list(f1,f2))


split(x,list(f1,f2),drop=TRUE)


list(rep(1,4),rep(2,3),rep(3,2),rep(4,1))


##########################################
##    tapply  function
#########################################

tapply(c(airquality$Ozone),airquality$Month,mean,na.rm=TRUE)
tapply(airquality$Solar.R,airquality$Month,mean,na.rm=TRUE)
tapply(c(airquality$Wind),airquality$Month,mean,na.rm=TRUE)
tapply(c(airquality$Ozone),airquality$Month,mean,na.rm=TRUE)

##########################################
##    lapply  function
#########################################

lapply(s,function(x) colMeans(x[,c("Ozone","Solar.R","Wind")]))

lapply(ufc[4:5], mean)

x<-1:10
y=2:11
lapply(x,runif)
lapply(c(y,x),t.test)
lapply(x,runif,min=0,max=10)

##########################################
##    sapply  function
#########################################

sapply(s,function(x) colMeans(x[,c("Ozone","Solar.R","Wind")]))

sapply(s,function(x) colMeans(x[,c("Ozone","Solar.R","Wind")],na.rm=TRUE))


sapply(ufc[4:5], mean)[[1]]



##########################################
##   interaction  function
#########################################

x<-rnorm(10)
f1<-gl(2,5)
f2<-gl(5,2)
f1
f2
interaction(f1,f2)



##########################################
##   mapply function
#########################################
rep(1:4,c(4,3,2,1))
mapply(rep.int,1:4,4:1)
mapply(rep, times = 1:4, MoreArgs = list(x = 42))
mapply(rep.int,42,1:4)
mapply(function(x, y) seq_len(x) + y,
       c(a =  1, b = 2, c = 3),  # names from first
       c(A = 10, B = 0, C = -10))

word <- function(C, k) paste(rep.int(C, k), collapse = "")
utils::str(mapply(word, LETTERS[1:6], 6:1, SIMPLIFY = FALSE))

rep(1:4, 2)
rep(1:4, each = 2)       # not the same.
rep(1:4, c(2,2,2,2))     # same as second
rep.int(1:4, 3)
rep_len(1:4, length.out=12)
rep(1:4, each = 2)       # not the same.
rep(1:4, c(2,2,2,2))     # same as second.
rep(1:4, c(2,1,2,1))
?mapply

x=1:10
y=11:20
mapply(wilcox.test,matrix(c(x,y),2))
mapply(t.test,c(x,y))
t.test(x)
?rep


pt2<-unlist(split(p1$egf,p1$specID)[10])
pt3<-unlist(split(p1$egf,p1$specID)[20])
pt5<-unlist(split(p1$egf,p1$specID)[31])
pt6<-unlist(split(p1$egf,p1$specID)[32])
pt7<-unlist(split(p1$egf,p1$specID)[33])
pt8<-unlist(split(p1$egf,p1$specID)[34])
pt10<-unlist(split(p1$egf,p1$specID)[1])
pt11<-unlist(split(p1$egf,p1$specID)[2])
pt13<-unlist(split(p1$egf,p1$specID)[3])
pt14<-unlist(split(p1$egf,p1$specID)[4])
pt15<-unlist(split(p1$egf,p1$specID)[5])
pt16<-unlist(split(p1$egf,p1$specID)[6])
pt17<-unlist(split(p1$egf,p1$specID)[7])
pt18<-unlist(split(p1$egf,p1$specID)[8])#8
pt19<-unlist(split(p1$egf,p1$specID)[9])
pt20<-unlist(split(p1$egf,p1$specID)[11])
pt21<-unlist(split(p1$egf,p1$specID)[12])
pt22<-unlist(split(p1$egf,p1$specID)[13])
pt23<-unlist(split(p1$egf,p1$specID)[14])
pt24<-unlist(split(p1$egf,p1$specID)[15])
pt25<-unlist(split(p1$egf,p1$specID)[16])
pt26<-unlist(split(p1$egf,p1$specID)[17])
pt28<-unlist(split(p1$egf,p1$specID)[18])
pt29<-unlist(split(p1$egf,p1$specID)[19])
pt30<-unlist(split(p1$egf,p1$specID)[21])
pt31<-unlist(split(p1$egf,p1$specID)[22])
pt32<-unlist(split(p1$egf,p1$specID)[23])
pt33<-unlist(split(p1$egf,p1$specID)[24])
pt34<-unlist(split(p1$egf,p1$specID)[25])
pt35<-unlist(split(p1$egf,p1$specID)[26])
pt36<-unlist(split(p1$egf,p1$specID)[27])
pt37<-unlist(split(p1$egf,p1$specID)[28])
pt38<-unlist(split(p1$egf,p1$specID)[29])
pt39<-unlist(split(p1$egf,p1$specID)[30])


######wilcoxin signed rank test of egr group#######

t=matrix(c(pt2,pt3,pt5,pt6,pt7,pt8,pt10,pt11,pt13,pt14,pt15,pt16,pt17,pt18,
           pt19,pt20,pt21,pt22,pt23,pt24,pt25,pt26,pt28,pt29,pt30,pt31,pt32,
           pt33,pt34,pt35,pt36,pt37,pt39,pt39),34,6,byrow=TRUE)


egf.wilcox=apply(t,1,wilcox.test,na.rm=TRUE)
egf.wilcox

c=data.frame(pt2,pt3,pt5,pt6,pt7,pt8,pt10,pt11,pt13,pt14,pt15,pt16,pt17,pt18,
             pt19,pt20,pt21,pt22,pt23,pt24,pt25,pt26,pt28,pt29,pt30,pt31,pt32,pt33,pt34,pt35,pt36,pt37,pt39,pt39)

egf.wilcox=apply(c,2,wilcox.test,na.rm=TRUE)
egf.wilcox


###### by function########
attach(iris)
head(iris)

sapply(iris[,-5],mean)
apply(iris[,-5],2,mean)
tapply(iris$Sepal.Length,iris$Species,mean)
by(iris[, 1:4], Species, colMeans)


# ######## eapply  ########
# a new environment
e<-new.env()
# two environment variables, a and b
e$a<-1:10
e$b<-11:20
# mean of the variables
eapply(e, mean)



###################################################################################
####################### purrr package ##############################################
# install.packages("devtools")
#devtools::install_github("hadley/purrr")
library(purrr)
df <- data.frame(
  a = 1L,
  b = 1.5,
  y = Sys.time(),
  z = ordered(1)
)

df[1:4] %>% sapply(class) %>% str()

str(df)


df[1:4] %>% map_chr(class)
#> Error: Result 3 is not a length 1 atomic vector
df[1:4] %>% map_chr(~ paste(class(.), collapse = "/"))


# apply a function over a list
x <- list(1, 3, 5)
y <- list(2, 4, 6)
map2(x, y, c)


map2_dbl(x, y, `+`)

mapply("+",x,y)
mapply(sum,x,y)

xx <- c(1, 3, 5)
yy <- c(2, 4, 6)
sapply(list(xx,yy),sum)
sapply(list(x,y),sum)



listA <- list(matrix(rnorm(20), nrow=10),
              matrix(rnorm(20), nrow=10))
listB <- list(matrix(rnorm(20), nrow=10),
              matrix(rnorm(20), nrow=10))
mapply(function(X,Y) {
  sapply(1:10, function(row) cor(X[row,], Y[row,]))
}, X=listA, Y=listB)


# apply a multiple functions over a list
spread <- list(sd = sd, iqr = IQR, mad = mad,sum=sum)
x <- rnorm(100)

invoke_map_dbl(spread, x = x)



#############Type-stable flatten
#Another situation when type-stability is important is flattening a nested list 
#into a simpler data structure. Base R has unlist(), but it’s dangerous because 
#it always succeeds. As an alternative, purrr provides flatten_lgl(), flatten_int(),
#flatten_dbl(), and flatten_chr():
  
  x <- list(1L, 2:3, 4L)
x %>% str()

x %>% flatten() %>% str()

x %>% flatten_int() %>% str()


############Type-stable try()
#Another function in base R that is not type-stable is try(). try() 
#ensures that an expression always succeeds, either returning the original 
#value or the error message:
  
str(try(log(10)))

str(try(log("a"), silent = TRUE))

#safely() is a type-stable version of try. It always returns a list of two 
#elements, the result and the error, and one will always be NULL.

safely(log)(10)

safely(log)("a")

safe_log <- safely(log)
x <- list(10, "a", 5)
log_x <- x %>% map(safe_log)

str(log_x)


log_x %>% transpose() %>% str()


log_x %>% transpose() %>% str()


results <- x %>% map(safe_log) %>% transpose()

(ok <- results$error %>% map_lgl(is_null))

(bad_inputs <- x %>% discard(ok))

(successes <- results$result %>% keep(ok) %>% flatten_dbl())



#For loops vs functionals
#Imagine you have a data frame and you want to compute the mean of each column. Y
#ou might write code like this:
  
  df <- data.frame(
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    d = rnorm(10)
  )

results <- numeric(length(df))
for (i in seq_along(df)) {
  results[i] <- mean(df[[i]])
}
results
apply(df,2,mean)



#map() returns a list.
#map_lgl() returns a logical vector.
#map_int() returns a integer vector.
#map_dbl() returns a double vector.
#map_chr() returns a character vector.
#map_df() returns a data frame.
#walk() returns nothing. Walk is a little different to the others 
#because it’s called exclusively for its side effects, so it’s described in 
#more detail later in walk.

x <- list(1, 2, 3)
str(x)

map_int(x, length)

map_dbl(x, mean)

map_dbl(x, median)

map_dbl(x, mean, trim = 0.5)


z <- list(x = 1:3, y = 4:5)
map_int(z, length)
sapply(z,length)


models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))



models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))


########### similar  work ####################

hsb2 <- read.csv("http://www.ats.ucla.edu/stat/data/hsb2.csv")
names(hsb2)
varlist <- names(hsb2)[8:11]

models <- lapply(varlist, function(x) {
  lm(substitute(read ~ i, list(i = as.name(x))), data = hsb2)
})

lapply(models, summary)

par(mfrow = c(2, 2))
invisible(lapply(models, plot))



models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)

summary(models)

models[[1]]

summary(models[[1]])$r.squared
#equivalently
summary(models[[1]])[[8]]





############## rapply#############################
##Recursively Apply a Function to a List


X <- list(list(a = pi, b = list(c = 1:1)), d = "a test")
rapply(X, function(x) x, how = "replace")
rapply(X, sqrt, classes = "numeric", how = "replace")
rapply(X, nchar, classes = "character",
       deflt = as.integer(NA), how = "list")
rapply(X, nchar, classes = "character",
       deflt = as.integer(NA), how = "unlist")
rapply(X, nchar, classes = "character", how = "unlist")
rapply(X, log, classes = "numeric", how = "replace", base = 2)
