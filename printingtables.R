install.packages("gridExtra")

library(formattable)
DF <- data.frame(Ticker=c("", "", "", "IBM", "AAPL", "MSFT"),
                 Name=c("Dow Jones", "S&P 500", "Technology", 
                        "IBM", "Apple", "Microsoft"),
                 Value=accounting(c(15988.08, 1880.33, NA, 
                                    130.00, 97.05, 50.99)),
                 Change=percent(c(-0.0239, -0.0216, 0.021, 
                                  -0.0219, -0.0248, -0.0399)))
DF
##   Ticker       Name     Value Change
## 1         Dow Jones 15,988.08 -2.39%
## 2           S&P 500  1,880.33 -2.16%
## 3        Technology        NA  2.10%
## 4    IBM        IBM    130.00 -2.19%
## 5   AAPL      Apple     97.05 -2.48%
## 6   MSFT  Microsoft     50.99 -3.99%
formattable(DF, list(
  Name=formatter(
    "span",
    style = x ~ ifelse(x == "Technology", 
                       style(font.weight = "bold"), NA)),
  Value = color_tile("white", "orange"),
  Change = formatter(
    "span",
    style = x ~ style(color = ifelse(x < 0 , "red", "green")),
    x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)))
)


setwd("/Users/nanaakwasiabayieboateng/Documents/memphisclassesbooks/DataMiningscience")

library(gridExtra)
pdf("data_output.pdf", height=11, width=8.5)
grid.table(mtcars)
dev.off()

grid.table(mtcars, main="Main Title Here.")
grid.arrange(tableGrob(mtcars), main="Main Title Here.")
getwd()

library(gridExtra); maxrow = 30;
npages = ceiling(nrow(iris)/maxrow); 
pdf("iris_pages.pdf", height=11, width=8.5); 
for (i in 1:npages) {idx = seq(1+((i-1)*maxrow), i*maxrow); 
grid.newpage(); grid.table(iris[idx, ])}; 
dev.off()






  library(xtable)

df = data.frame(matrix(rnorm(400), nrow=100))
xt = xtable(df)
print(xt, 
      tabular.environment = "longtable",
      floating = FALSE
)



options(digits = 4)
set.seed(123)
x = matrix(rnorm(40), 5)
dimnames(x) = list(NULL, head(LETTERS, ncol(x)))
knitr::kable(x, digits = 2, caption = "A table produced by printr.")


#install.packages("knitr")
library(stargazer)
stargazer(mtcars, type = 'text', out = 'out.txt')

stargazer(mtcars, type = 'html', out = 'out.html')

require(knitr)
library(printr)
options(digits = 4)
set.seed(123)
x = matrix(rnorm(40), 5)
x
knit_print(x)
library(printr)
loadNamespace('printr')

my_data <- head(iris)
names(my_data) <- c(letters[1:ncol(iris)])
library("knitr")
kable(my_data)

library("xtable")
print(xtable(my_data), type = "html",
      include.rownames=FALSE, html.table.attributes
      =list("border='0' cellpadding='5' "))