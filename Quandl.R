#install.packages("devtools")
library(devtools)
#install_github("quandl/quandl-r")
#install.packages("Quandl")
library(Quandl)
mydata = Quandl("FRED/GDP") # basic call
mydata = Quandl("FRED/GDP", type="raw") #change format
mydata = Quandl("FRED/GDP", type="ts")  # ts format
mydata = Quandl("FRED/GDP", type="xts") #xts format
mydata = Quandl("FRED/GDP", type="zoo")  # zoo format
#Slice and Dice the Data
#To set start and end dates:
mydata = Quandl("FRED/GDP", start_date="2001-12-31", end_date="2005-12-31")


#Request specific columns:
  
  mydata = Quandl(c("FRED/GDP.1", "WIKI/AAPL.4"))
  
  
  

#Preprocess the Data
#To change the sampling frequency:
  
mydata = Quandl("FRED/GDP", collapse="annual")
#To perform elementary calculations on the data:
  
mydata = Quandl("FRED/GDP", transform="rdiff")
#Datatables
#Make a Basic Data Call
#This example retrieves all rows for ZACKS/FC where ticker='AAPL'.

mydata = Quandl.datatable("ZACKS/FC", ticker="AAPL")
#Set Pagination
#Datatables can return large volumes of data due to their format. To request more than one page of data, please turn on pagination.

#To turn on pagination:
  
  mydata = Quandl.datatable("ZACKS/FC", ticker="AAPL", paginate=T)
#Slice and Dice the Data
#To request specific columns:
  
  mydata = Quandl.datatable("ZACKS/FC", ticker=c("AAPL", "MSFT"), qopts.columns=c("ticker", "per_end_date"))
#This returns the ticker and per_end_date columns for the tickers AAPL and MSFT from the ZACKS/FC datatable.

#To filter based on column:
  
mydata = Quandl.datatable("ZACKS/FC", ticker=c("AAPL", "MSFT"), per_end_date.gt="2015-01-01",qopts.columns=c("m_ticker", "per_end_date", "tot_revnu"))
#This returns the m_ticker, per_end-date, and tot_revnu columns for the tickers AAPL and MSFT for every per_end_date greater than 2015-01-01 from the ZACKS/FC datatable.

#Usage Limits
#The Quandl R package is free. If you would like to make more than 50 calls a day, however, you will need to create a free Quandl account and set your API key:
  
  Quandl.api_key("YOUR_API_KEY_HERE")
#Please Note:
#If a valid API key is not used, some datatables will default to returning sample data. If you are not receiving all the expected data, please double check your API key.
#When using the paginate=T option, and there is a very large result set, you will receive a message indicating that there is more data to be downloaded. Please use more granular filters to select a smaller subset of the data.
  Quandl.api_key("vStD_6ejDcBt56ELqU-Z")
 
  
#  **Task 1:** Pulling data from an API 
#Venture Capital Investments By Stage - Number of Deals - Quarterly  
  mydata = Quandl("NVCA/VENTURE_3_09D") 
 
  
  #install.packages("KernelKnn")
  #install.packages("sqldf")
  #################################################
  ### load multiple packages at the same time###
  
  lib=c("ISLR","dplyr","purrr","feather","tree","randomForest","caret","ggplot2","scales",
        "e1071","gbm","LiblineaR","MASS","quantmod","fImport","KernelKnn","Quandl","plyr"
        ,"data.table", "pastecs","Hmisc","GGally","ggcorrplot","tibble","stringr","sqldf")
  
  #install.packages("pastecs")
  
  sapply(lib, require, character.only = TRUE)
  
  require(quantmod)
  require(dplyr)
  library(fImport)
  library(Quandl)
  
  
  api_key = "zr_GzVVB7WAsAFtRZCBi"
df1=  Quandl("NVCA/VENTURE_3_09D", authtoken= api_key ) 
df2=  Quandl("NVCA/VENTURE_3_09C", authtoken= api_key ) 



# PHLX Semiconductor (SOX)\n",
df3 = Quandl('NASDAQOMX/SOX', collapse='quarterly', authtoken=api_key)



# Lending Club stock

start = as.Date("2015-1-1")
end = as.Date("2016-11-30")

# Load LC data get current quote

df4=getSymbols("LC",src="yahoo",from=start,to=end) 

df4=LC

getQuote("LC")

tail(df4)


# current quote
getQuote('LC', from=start,to=end)




# OnDeck Capital stock
getQuote('ONDK', 'yahoo', from=start,to=end)


df5=getSymbols("ONDK",src="yahoo",from=start,to=end) 

df5=ONDK

head(df5)


#**Task 2:** Dropping unnecessary columns

#a)** From df1, drop the 'Total' column

########################################
# MASS package causes a conflict with dplyr

select <- dplyr::select  #fixes clash with MASS

df1=df1[,-6]

select(df1,-Total)  #doesn't because of conflict with MASS alternatively

df1%>%dplyr::select(-Total)  # Alternatively wirte dplyr::select each time works


drop.cols <- c('Sepal.Length', 'Sepal.Width')
iris %>% dplyr::select(-one_of(drop.cols))




#b)** From df2, drop the 'Total' column

df2=df2[,-6]

select(df2,-(Total))#alternatively

df2%>%dplyr::select(-Total) 
names(df2)

#c)** From df3, drop the following columns: 'Index Value', 'High', 'Low', 'Dividend Market Value'\n",



df3=df3[,-c(2,3,4,6)]


drop.cols =c("Index Value", "High", "Low","Dividend Market Value") #alternatively
df3=as.data.frame(df3)
df3 %>% select(-one_of(drop.cols))
df3%>%dplyr::select(-one_of(drop.cols))
names(df3)
#**d)** From df4 and df5, drop the following columns: 'Open', 'High', 'Low', 'Close'"

df4=df4[,-c(1,2,3,4)]

drop.cols =c("LC.Open","LC.High","LC.Low","LC.Close") #alternatively
df4=as.data.frame(df4)
df4 %>% select(-one_of(drop.cols))
df4%>%dplyr::select(-one_of(drop.cols))
names(df4)



df5=df5[,-c(1,2,3,4)]


drop.cols =c('ONDK.Open', 'ONDK.High', 'ONDK.Low',' ONDK.Close') #alternatively
df5=as.data.frame(df5)
df5 %>% select(-one_of(drop.cols))


#**Task 3:** Renaming columns

#**a)** Use list comprehension to replace white spaces with underscores and convert the string to lowercase\n",
#setnames(DF, "oldName", "newName")


colnames(df1)=gsub('([[:punct:]])|\\s+','_',colnames(df1))
colnames(df2)=gsub('([[:punct:]])|\\s+','_',colnames(df2))
colnames(df3)=gsub('([[:punct:]])|\\s+','_',colnames(df3))
colnames(df4)=gsub('([[:punct:]])|\\s+','_',colnames(df4))
colnames(df5)=gsub('([[:punct:]])|\\s+','_',colnames(df5))

#install.packages("stringr")
library(stringr)
## change Block case to lower case
colnames(df1) <- tolower(colnames(df1))
colnames(df2) <- tolower(colnames(df2))
colnames(df3) <- tolower(colnames(df3))
colnames(df4) <- tolower(colnames(df4))
colnames(df5) <- tolower(colnames(df5))



#**b)** For df1 and df2, add column name suffixes '_cnt' and '_amt' respectively 
colnames(df1) <- paste(colnames(df1),"_cnt", sep = "_")
colnames(df2) <- paste(colnames(df2),"_amt", sep = "_")

#**c)** For df4 and df5, add column name prefixes 'lc_' and 'od_' respectively 

#colnames(df4) <- paste( "lc_",colnames(df4), sep = "_")
colnames(df5) <- paste( "od_",colnames(df5), sep = "_")




#**Task 4:** Calculating new columns with Pandas methods and lambda functions\n",

#**a)** Convert the 'total_market_value' column to the percent change (from one observation to the next) and name it 'tmv_pct'\n",



df3%>%mutate(tmv_pct=(c(NA,diff(total_market_value))/total_market_value))

df3=mutate(df3,tmv_pct=(c(NA,diff(total_market_value))/total_market_value)*100)



#combine vectors of different length
nm <- list(1:8,3:8,1:5)
sapply(nm, '[', seq(max(sapply(nm,length))))
sapply(nm, '[', seq(max(lengths(nm))))

#**b)** Apply a lambda function to create a binary (0 or 1) column; if value is greater than 0 then 1 else 0 and name it 'tmv_pct_flag'\n",


df3=mutate(df3,tmv_pct_flag=ifelse(df3$tmv_pct>0,1,0))



#**c)** Use vectorized computation (native to pandas) to calculate the log returns using the 'adj_close' column and make sure to add column name prefixes again"



head(df5)
head(df4)
head(df3)
head(df2)
head(df1)

#**Task 5:** Combining dataframes

#**a)** Use the Pandas merge or concatonate operation to combine the three (df1, df2, and df3) dataframes on their common date axis; name the new dataframe 'df'.\n",

#**b)** Do the same for df4 and df5; name the new dataframe 'df2'.


colnames(df3)[1] <- "Date"

#Alternatively setnames(DF, "oldName", "newName") using data.table

setnames(df3, "trade_date", "date")

df3 <- plyr::rename(df3,c('trade_date '=' Date'))     #plyr
df1 <- plyr::rename(df3,c('date__cnt '=' Date'))     #plyr
df2 <- plyr::rename(df3,c('date__amt '=' Date'))

dplyr::rename(df3, Trade Date = Date)     #dplyr

df=inner_join(df1, df2, by='Date') %>%
  inner_join(., df3, by='Date') 


df=join_all(list(df1,df2,df3), by='Date', type='inner')


df22=merge(df4,df5)



#"**Task 6:** Generating descriptive statistics\n",

#"**a)** Run some descriptive statistics on your data"

summary(df)
summary(df22)

apply(df,2,summary)

apply(df22,2,summary)

#library(Hmisc)

describe(df) 

describe(df22) 

apply(df22,2,describe)

#install.packages("pastecs")
#library(pastecs)
# paastecs display summary in neat table
stat.desc(df) 

stat.desc(df22) 


#**Task 7:** Generating histograms\n",

#**a)** Create histograms for each variable in 'df'"
colnames(df)
ggplot(df, aes(x = seed)) + 
  geom_histogram( 
                 breaks=seq(10, 170, by = 10), 
                 col="black", 
                 fill="black", 
                 alpha = .2) + 
  ggtitle("Distribution of seed entire dataset")+ labs(x="Seed", y="Frequency")



ggplot(df, aes(x = early_stage)) + 
  geom_histogram(breaks=seq(180, 700, by = 20),fill="black",col="black",alpha=0.2) + 
  ggtitle("Distribution of early_stage entire dataset")


ggplot(df, aes(x = log(total_market_value))) + 
  geom_histogram(fill="black",col="black",alpha=0.2) + 
  ggtitle("Distribution of total_market_value entire dataset")

qplot(log(df$total_market_value),
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for total_market_value", 
      xlab = "total_market_value",  
      fill=I("black"), 
      col=I("black"), 
      alpha=I(.2))


#par(mfrow=c(2,2))
#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
ggplot(df22, aes(x = lc_volume)) + 
  geom_histogram(fill="black",col="black",alpha=0.2) + 
  ggtitle("Distribution of lc_volume entire dataset")


ggplot(df22, aes(x = ondk_volume )) + 
  geom_histogram(fill="black",col="black",alpha=0.2) + 
  ggtitle("Distribution of ondk_volume  entire dataset")


ggplot(df22, aes(x = ondk_adjusted )) + 
  geom_histogram(fill="black",col="black",alpha=0.2) + 
  ggtitle("Distribution of ondk_adjusted  entire dataset")

ggplot(df22, aes(x = lc_adjusted )) + 
  geom_histogram(fill="black",col="black",alpha=0.2) + 
  ggtitle("Distribution of lc_adjusted  entire dataset")

head(df22)

#**Task 8:** Examining bivariate relationships\n",

#**a)** Calculate and visualize the correlation coefficients for each combination of variables"

pairs(df[,-1])
cor(df[,-1])
cor(df22)
class(df22)


panel.cor <- function(x, y, digits = 2, cex.cor, ...)
  
{
  
  usr <- par("usr"); on.exit(par(usr))
  
  par(usr = c(0, 1, 0, 1))
  
  # correlation coefficient
  
  r <- cor(x, y)
  
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  
  txt <- paste("r= ", txt, sep = "")
  
  text(0.5, 0.6, txt)
  
  
  
  # p-value calculation
  
  p <- cor.test(x, y)$p.value
  
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  
  txt2 <- paste("p= ", txt2, sep = "")
  
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  
  text(0.5, 0.4, txt2)
  
}



pairs(df, upper.panel = panel.cor)
#nstall.packages("GGally")
library(GGally)



panel.plot <- function(x, y) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  ct <- cor.test(x,y)
  sig <- symnum(ct$p.value, corr = FALSE, na = FALSE,
                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                symbols = c("***", "**", "*", ".", " "))
  r <- ct$estimate
  rt <- format(r, digits=2)[1]
  cex <- 0.5/strwidth(rt)
  
  text(.5, .5, rt, cex=cex * abs(r))
  text(.8, .8, sig, cex=cex, col='blue')
}
panel.smooth <- function (x, y) {
  points(x, y)
  abline(lm(y~x), col="red")
  lines(stats::lowess(y~x), col="blue")
}
pairs(df, lower.panel=panel.smooth, upper.panel=panel.plot)





# Correlation plot
ggcorr(df, palette = "RdBu", label = TRUE)


# Compute a correlation matrix
corrm <- round(cor(df22), 1)
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(mtcars)
# Install
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggcorrplot")
require(ggcorrplot)
# Visualize the correlation matrix
# --------------------------------
# method = "square" (default)
ggcorrplot(corrm)
# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(corrm, hc.order = TRUE, outline.col = "white")
# Types of correlogram layout
# --------------------------------
# Get the lower triangle
ggcorrplot(corrm, hc.order = TRUE, type = "lower",
           outline.col = "white")

# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(corrm, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))


# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
ggcorrplot(corrm, hc.order = TRUE, type = "lower",
           lab = TRUE)

library(Hmisc)
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
res2<-rcorr(as.matrix(df[,-1]))
flattenCorrMatrix(res2$r, res2$P)


# Get some colors
col<- colorRampPalette(c("blue", "white", "red","green"))(20)
x=cor(df22)
heatmap(x , col = col, symm = TRUE)




###### plotly
#install_github("hadley/tibble")
#install_github("plotly")

library(plotly)
#install.packages("plotly")
#install.packages("tibble") ; 
library(tibble)
#install.packages("plotly")

#print a lot of rows with tibble/dplyr
options(dplyr.print_max = 1e9)

as_data_frame(iris) # tibbles shows the class of columns
data_frame(x = 1:5, y = 1, z = x ^ 2 + y)
as.data.frame(iris)
library(nycflights13)
flights

# tibble package helps ggpair to work
ggpairs(
  df[,-1], 
  upper = list(continuous = wrap(ggally_cor, size = 10)), 
  lower = list(continuous = 'smooth')
)

ggpairs(df22)
ggpairs(df[,-1])








#**Task 9:** Interpreting and explaining results\n",

#"**a)** Explain any insights that you had based on the analysis thus far.\n",

#"**b)** What might some next steps be?"




#**Task 10:** Working with and visualizing time series data\n",

#**a)** Plot 'df2' log returns as a line plot\n",

#**b)** Plot 'df2' log returns as a line plot"



#**Task 11:** Identifying outliers\n",

#**a)** Create a column called 'lc_2_std' that holds a boolean value that identifies whether or not an observation is within 2 standard deviaitions. \n",

#**b)** Create a column called 'od_log_returns' that holds a boolean value that identifies whether or not an observation is within 2 standard deviaitions. "



#**Task 12:** Filtering dataframes\n",

#**a)** Plot overlapping histograms with 25 bins for 'lc_log_returns' and 'od_log_returns'\n",

#**b)** Plot overlapping histograms with 25 bins for 'lc_log_returns' and 'od_log_returns', but only for obervations that were within 2 standard deviations "





#**Task 13:** Windowing functions\n",

#**a)** Calculate a moving average on 'lc_adj_close' where the window is 10\n",

#**b)** Calculate a moving average on 'od_adj_close' where the window is 10\n",

#**c)** Plot these new fields"



#**Task 11:** Simple OLS forecasting in statsmodels\n",

#**a)** Fit a simple OLS regression using the 90 most recent data points from 'lc_adj_close_ma' and examine the results\n",

#**b)** Make a forecast for the next 30 data points and append it in a new column named 'lc_forecast'\n",

#**c)** Ensure that 'df2' has an appropriate date index to accomodate the new predictions\n",

#**d)** Plot 'lc_adj_close', 'lc_adj_close_ma', and 'lc_forecast'"




#**Task 12:** Supervised machine learning\n",

#**a)** Using the 'Housing' dataset (defined below), train a model to predict a home's 'price'. You can predict the exact price or the price range that the house will fall in. \n",

#**b)** Interpret the model. Is it a good model? What is it telling you?\n",

#**Note** This task is meant to be very open ended with the intent of evaluating your problem solving abilities and machine learning knowledge."




# Boston housing Data
#Descriptive statistics
# R will produce a list of available datasets
data()


#to list the data sets in all *available* packages.
data(package = .packages(all.available = TRUE))

data=Boston

apply(Boston,2,summary)
cor(Boston)
plot(density(Boston$medv),main='Density of Median Housing Values',
                              xlab='med. housing value',
                              ylab='density')
apply(Boston, 2, function(x) length(unique(x)))


X = scale(Boston[, -ncol(Boston)])
y = Boston[, ncol(Boston)]

# random split of data in train and test

spl_train = sample(1:length(y), round(length(y) * 0.75))
spl_test = setdiff(1:length(y), spl_train)
str(spl_train)


# evaluation metric

mse = function (y_true, y_pred) {
  
  out = mean((y_true - y_pred)^2)
  
  out
}



library(KernelKnn)

preds_TEST = KernelKnn(X[spl_train, ], TEST_data = X[spl_test, ], y[spl_train], k = 5 , 
                       
                       method = 'euclidean', weights_function = NULL, regression = T)
str(preds_TEST)



#The KernelKnnCV function

#I’ll use the KernelKnnCV function to calculate the mean-squared-error 
#using 3-fold cross-validation for the previous mentioned parameter pairs,

fit_cv_pair1 = KernelKnnCV(X, y, k = 9, folds = 3, method = 'mahalanobis', 
                           
                           weights_function = 'triweight', regression = T, threads = 5)
str(fit_cv_pair1)

fit_cv_pair2 = KernelKnnCV(X, y, k = 3, folds = 3, method = 'canberra',
                           
                           weights_function = 'cosine', regression = T, threads = 5)


str(fit_cv_pair2)