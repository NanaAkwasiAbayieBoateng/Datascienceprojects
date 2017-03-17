
table(rpois(100, 5))[1:11]
tabulate(rpois(100, 5))
str(table(rpois(100, 5)))
as.matrix(table(rpois(100, 5)))[,1]*2
ftable(rpois(100, 5))

library(readr)

data=read_csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv")
x<-as.numeric(data$VAL)
x=na.omit(x)
table(x) 
hist(x,breaks=24,xlab='class',main='Histogram of property value',col='darkblue')


str(na.omit(c(NA,1,2)))

DF <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA))
na.omit(DF)
length(is.na(DF))


setwd("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience")


#The code below checks if the data is already downloaded, if not it downloads it.
# save as q1.csv
if(!file.exists('q1.csv')){
  url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
  
  download.file(url,destfile = "q1.csv")
}
#Then let's read the data into R
data<-read.csv("q1.csv")
#We can perform calculations on the using the data. For example, let's calculate how many properties are worth $1,000,000 or more. 

#From the code book, we can see that the variable 'VAL' is property value and 24 represents properies that worth 1000000+. So let's use the table command to see how many properties are worth what.
x<-data$VAL
table(x) 
hist(x,breaks=24,xlab='class',main='Histogram of property value',col='darkblue')

## update packages 
download.packages(pkgs=lib,destdir="/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience")


#Rading XML packages

#Check if XML package is present, else download it.
if(!require(XML)){
  install.packages('XML')}
#Load XML package and download data. I have replaced https by httP to make it downloadable.
library(XML)

fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"

fileURL2 <- sub('https', 'http', fileURL)
doc <- xmlTreeParse(fileURL2, useInternal = TRUE)
#Now, let's get the rootnode and explore it.
rootNode<-xmlRoot(doc)
xmlName(rootNode)

names(rootNode)

zipcode<-xpathSApply(rootNode,"//zipcode",xmlValue)

sum(zipcode=='21218')

zipcode<-as.numeric(zipcode)

x<-unique(zipcode)
x

zipcode<-zipcode[zipcode>0]

hist(as.numeric(zipcode),breaks=32,xlab='zipcode',
     ylab='Number of restautants',
     main='Histogram of number of restaurants in Baltimore, MD, USA',
     col='skyblue',border='red')



#Reading rows and columns of our choice.¶
#Let's download xlsx binary data by setting the download mode to binary.
#The Excel spreadsheet for this example is from Natural Gas Aquisition Program which can be downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx
#Check if the data is already downloaded, else download it.

if(!file.exists('q3.xlsx')){
  url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx "
  
  download.file(url,destfile = "q3.xlsx", mode='wb')
}


#Check if the package xlsx is installed, else install it. Then, load xlsx package.
if(!require(xlsx)){
  install.packages('xlsx')
}

library(xlsx)
#We can read column and row numbers of our interest and do any calculations as shown below.
rowIndex<-18:23
colIndex<- 7:15

dat<-read.xlsx('q3.xlsx',sheetIndex=1,rowIndex = rowIndex,colIndex = colIndex)

sum(dat$Zip*dat$Ext,na.rm=T)