library(readxl)
dir="/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience"
setwd(dir)
file.list <- list.files(pattern='*.xlsx')
file.list <- list.files(pattern='*.csv')
df.list <- lapply(file.list, read_excel)
df.list <- sapply(file.list, read.csv, simplify=FALSE)


#Supposing the columns are the same for each file,
#you can bind them together in one dataframe with bind_rows from dplyr:
  
  library(dplyr)
df <- bind_rows(df.list, .id = "id")

#or with rbindlist from data.table:
  
library(data.table)
df <- rbindlist(df.list, idcol = "id")



# with the 'attr' function from base R
attr(df.list, "names") <- file.list
# with the 'names' function from base R
names(df.list) <- file.list
# with the 'setattr' function from the 'data.table' package
setattr(df.list, "names", file.list)




library(xlsx)
setwd("c:/temp/")
filenames <- list.files(pattern=".xls")
library(plyr)
df.list <- lapply(filenames, function(x) read.xlsx(file=x, sheetIndex=1,
    colIndex=1:4,as.data.frame=TRUE, header=FALSE, FILENAMEVAR=x))
final.df <- rbind.fill(df.list)



# install and load packages -----------------------------------------------
pkg <- c("XLConnect")

new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}

library(XLConnect)
# load excel workbook
excel <- loadWorkbook("filepath/ExcelData.xlsx") # change to match your path

# get sheet names
sheet_names <- getSheets(excel)
names(sheet_names) <- sheet_names

# put sheets into a list of data frames
sheet_list <- lapply(sheet_names, function(.sheet){readWorksheet(object=excel, .sheet)})

# limit sheet_list to sheets with at least 1 dimension 
sheet_list2 <- sheet_list[sapply(sheet_list, function(x) dim(x)[1]) > 0]


# code to read in each excel worksheet as individual dataframes
# for (i in 2:length(sheet_list2)){assign(paste0("df", i), as.data.frame(sheet_list2[i]))}

# define function to clean data in each data frame (updated based on your data)
cleaner <- function(df){
  # drop rows with missing values 
  df <- df[rowSums(is.na(df)) == 0,] 
  # remove serial comma from all variables 
  df[,-1] <- as.numeric(gsub(",", "", as.matrix(df[,-1])))
  # create numeric version of year variable for graphing 
  df$Year <- as.numeric(substr(df$year, 1, 4))
  # return cleaned df      
  return(df)
}

# clean sheets and create one data frame
# data <- do.call(rbind,lapply(seq_along(sheet_list2), function(x) cleaner(sheet_list2[[x]])))
data <- do.call(rbind,lapply(names(sheet_list2), function(x) cleaner(sheet_list2[[x]])))




install.packages("readxl") # CRAN version

devtools::install_github("hadley/readxl") # development version


library(readxl)

# read_excel reads both xls and xlsx files
read_excel("my-old-spreadsheet.xls")
read_excel("my-new-spreadsheet.xlsx")

# Specify sheet with a number or name
read_excel("my-spreadsheet.xls", sheet = "data")
read_excel("my-spreadsheet.xls", sheet = 2)

# If NAs are represented by something other than blank cells,
# set the na argument
read_excel("my-spreadsheet.xls", na = "NA")


install.packages("openxlsx")
library(openxlsx)
rawData<-read.xlsx("your.xlsx");



require("xlsx")
read.xlsx("filepath/filename.xlsx",1) 

library("xlsx")
FirstTable <- read.xlsx("MyExcelFile.xlsx", 1 , stringsAsFactors=F)
SecondTable <- read.xlsx("MyExcelFile.xlsx", 2 , stringsAsFactors=F)


library(XLConnect)
theData <- readWorksheet(loadWorkbook("C:/AB_DNA_Tag_Numbers.xlsx"),sheet=1)