#install.packages("Rblpapi")
library(Rblpapi)
#install.packages("RBloomberg")
install.packages("Rbbg", repos = "http://r.findata.org") 
require(Rblpapi)



bdh("SPY US Equity", c("PX_LAST", "VOLUME"), start.date=Sys.Date()-31)