# List of packages needed and load them together
rm(list=ls())
list=c("dplyr","tibble","Hmisc","readr","stringr","stringr","sqldf","Hmisc","RSQLite")
sapply(list, require, character.only = TRUE)

#install.packages("RSQLite")

# read downloaded 2014 data file into R

data2014=read_csv("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience/Medicare_Provider_Charge_Inpatient_DRGALL_FY2014.csv")

#data2014=read_csv("C:/Users/Gucci148/Documents/DataMiningscience/Medicare_Provider_Charge_Inpatient_DRGALL_FY2014.csv")
  
  
dim(data2014)

# Remove spaces in the names of the  columns

colnames(data2014)=gsub('([[:punct:]])|\\s+','_',colnames(data2014))

#Select rows which contain the KIDNEY & URINARY TRACT INFECTIONS W MCC

filter <- dplyr::filter

data2014 %>%
  dplyr::filter(str_detect(DRG_Definition, "KIDNEY & URINARY TRACT INFECTIONS W MCC")) %>%
  dplyr::filter(str_detect(Provider_City, "BALTIMORE"))


library(sqldf)
 #Alternatively,this is not case sensitive

df1=sqldf("select * from data2014 where DRG_Definition LIKE '%KIDNEY & URINARY TRACT INFECTIONS W MCC%'")

df2=sqldf("select * from df1 where Provider_City LIKE '%Baltimore%'")







####  Hospital with the Highest charge 

df2[which(df2$Average_Covered_Charges==max(df2$Average_Covered_Charges)),]$Provider_Name 

####  Hospital with the Lowest charge 

df2[which(df2$Average_Covered_Charges==min(df2$Average_Covered_Charges)),]$Provider_Name 


#average charge per inpatient stay for the city

mean(df2$Average_Covered_Charges)

data2014 %>%
  dplyr::filter(str_detect(DRG_Definition, "KIDNEY & URINARY TRACT INFECTIONS W MCC")) %>%
  dplyr::filter(str_detect(Provider_City, "BALTIMORE")) %>% mutate(mean(Average_Covered_Charges))






#Descriptive statistics for entire data set

describe(data2014)

#Descriptive statistics for city of Baltimore data set

(describe(df2))



# view first 40 variables of column 1

print(unique(data2014[,1]), n=564)


############################################################
## Average_Total_Payments in Baltimore

ggplot(df2, aes(x = Average_Total_Payments)) + 
  geom_histogram() + 
  ggtitle("Distribution of Average_Total_Payments in Baltimore")+
  ggtitle("Distribution of Average_Total_Payments in Baltimore")+ labs(x="Average_Total_Payments", y="Frequency")






