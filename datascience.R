point <- c("A","B","D")
connectTo <- list(list("B","C"),list("A","C"),list())
test <- data.frame(point, I(connectTo))
test$connectTo[[1]][[1]][1]

tmp <- apply(test,1,function(x){pt <- x$point[1];return(Reduce(rbind,lapply(x$connectTo,function(y){print(y);return(matrix(ncol=2,c(pt,y)))})))})
final_matrix = Reduce(rbind, tmp)




#============================================================
# 
#randomly select some rows of a dataframe
#  
#===========================================================

x=data.frame(A=c(1,2,3,10),B=c(4,5,6,11),C=c(7,8,9,12))

x[sample(x,5),]

x[sample(nrow(x),3),]


#============================================================
# 
#randomly select some columns of a dataframe
#  
#===========================================================

x[,sample(ncol(x),2)]


sapply(1:4,function(row) x[row,]=sample(x[row,]))



####packages   ##
??randomizeMatrix
library(picante)
randomizeMatrix(x, null.model = "frequency", iterations = 1000)



#============================================================
# 
#sample the colums 
#  
#===========================================================

sample(x[,1],4)
xx=matrix(0,nrow=nrow(x),ncol=ncol(x))
for (i in 1:3)
  xx[,i]=sample(x[,i],nrow(x),replace=F)
colnames(xx)=c("A","B","C")
xx


#============================================================
# 
#sample the colums with replacement
#  
#===========================================================

nsample=function(x) sample(x,replace=T)

apply(x,2,function(x) nsample(x))

apply(x, 2,sample,replace=T)

mapply(sample, x,replace=T)


#============================================================
# 
#sample the colums without replacement
#  
#===========================================================

apply(x,2,function(x) sample(x))

mapply(sample, x,replace=F)


a <-array(rnorm(2 * 2 * 10), c(2, 2, 10))

apply(a, c(1, 2), mean)

rowMeans(a, dims = 2) ## Faster

mapply(rep, 1:4, 4:1)

mapply(sample, x,byrow)


apply(x, 1,sample,replace=F,size=2)






#============================================================
# 
#Google Analytics
#  
#===========================================================

library(googleVis)
data(Fruits)
M = gvisMotionChart(data=Fruits, idvar="Fruit", timevar="Date", chartid="ILoveFruit")
plot(M)
cat(unlist(M$html), file="output/ILoveFruits.html")
print(M)


M <- gvisMotionChart(Fruits, "Fruit", "Year")
cat(M$html$chart, file="tmp.html")





#============================================================
# 
#Compile R for data science Hadley Wickham
#  
#===========================================================

#devtools::install_github("hadley/r4ds")
#devtools::install_github("wch/webshot")



git clone https://github.com/hadley/ggplot2-book.git
cd ggplot2-book
make clean
make


library(devtools)
if (packageVersion("devtools") < "1.9.1") {
  message("Please upgrade devtools")
}
devtools::install_deps()



#============================================================
# 
#replace NA with mean of coulumn
#  
#===========================================================

d=data.frame(x=c(NA,2,3,5,NA,3,5),y=c(NA,9,3,5,NA,3,50),z=c(NA,20,3,5,NA,3,5))
x=c(NA,2,3,5,NA,3,5)

replace=function(x){
  x[which(is.na(x))]=mean(x,na.rm=TRUE)
  
  return(x)
}
replace(x)

apply(d,2,replace)

#################################
## install latest version of dplyr

#if (packageVersion("devtools") < 1.6) {
#  install.packages("devtools")
#}
#devtools::install_github("hadley/lazyeval")
#devtools::install_github("hadley/dplyr")

#install.packages(c("nycflights13", "Lahman"))
#install.packages(c("RSQLite","RPostgreSQL"))
rm(list=ls())
library(dplyr) # for functions
library(RSQLite)
library(RPostgreSQL)
library(nycflights13) # for data
flights

# Caches data in local SQLite db
flights_db1 <- tbl(nycflights13_sqlite(), "flights")

# Caches data in local postgres db
flights_db2 <- tbl(nycflights13_postgres(), "flights")
# Select columns by name

select(flights, year, month, day)

select(flights, year:day)
require(MASS)
require(dplyr)
mtcars %>%
  dplyr::select(mpg)

flights%>%dplyr::select(year,day,month)



#==============
# LOAD PACKAGES
#==============

library(tidyverse)
library(stringr)
library(forcats)
library(ggmap)
library(rvest)


#==========================
#SCRAPE DATA FROM WIKIPEDIA
#==========================

# SCRAPE
html.world_ports <- read_html("https://en.wikipedia.org/wiki/List_of_busiest_container_ports")

df.world_ports <- html_table(html_nodes(html.world_ports, "table")[[2]], fill = TRUE)


# INSPECT
glimpse(df.world_ports)



#==========================
# RENAME VARIABLES: 
# - transform to lower case
#==========================

#-----------
# LOWER CASE
#-----------
colnames(df.world_ports) <- colnames(df.world_ports) %>% tolower()

# INSPECT
colnames(df.world_ports)



#===================================================
# GEOCODE
# - here, we're picking up lat/long from Google Maps
#   using ggmaps::geocode()
#===================================================


#--------------------------
# get data from google maps
#--------------------------
geocodes.world_ports <- geocode(df.world_ports$port)


#--------------------------------------------------------
# COMBINE:
# - bind the new lat/long data to df.world_ports data frame
#--------------------------------------------------------
df.world_ports <- cbind(df.world_ports, geocodes.world_ports)

head(df.world_ports)







#--------------------------------------------------------
# 
# - install many packages
#--------------------------------------------------------
my_packages=c("manipulate","dbscan","tm","downloader","wordcloud","rgdal","httr",
              "rvest","ggmap","calibrate","maps","maptools","stringi","caret","nnet",
              "NeuralNetTools","tidyr","ggthemes","gridExtra","shiny","shinydashboard",
              "plyr","RCurl","ROAuth","twitteR","chron","RColorBrewer","lattice","lubridate"
              ,"sp","fields","ROCR","caTools","rpart","e1071","randomForest","plotmo","XML",
              "xlsx","readr","scales","reshape2","gtrendsR","bookdown")

which(    installed.packages() %in% my_packages )
length(installed.packages())

length(my_packages)

(my_packages %in% installed.packages()[, "Package"])

install_load= function(packages){
  to_install <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  if (length(to_install)){
    install.packages(to_install, repos='http://cran.us.r-project.org',dependencies = TRUE)
  }
  lapply(packages,library, character.only = TRUE)
  
  install_load(my_packages)
}


install_load(my_packages)

library(bookdown)

#bookdown_site(http://topepo.github.io/caret/index.html)

install.packages("h2o")
library(h2o)


#============================================================
# 
#install and initialize the H2O package for R.
#  
#===========================================================

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1497/R", getOption("repos"))))
library(h2o)
localH2O = h2o.init()

# Finally, let's run a demo to see H2O at work.
demo(h2o.glm)







#============================================================
# 
#number of packages in R.
#  
#===========================================================


nrow(available.packages(repos='https://cloud.r-project.org/'))



#============================================================
# 
#number of installed packages in R.
#  
#===========================================================

nrow(installed.packages())

dim(installed.packages())


#============================================================
# 
#To check if you already installed the package or not, type in
#  
#===========================================================


any(grepl("<name of your package>",
          installed.packages()))


"haven" %in% installed.packages()

any(grepl("<haven>",
          installed.packages()))