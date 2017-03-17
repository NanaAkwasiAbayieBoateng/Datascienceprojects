# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.


data = read.csv("../input/diabetes.csv")
library("ggplot2")
library("corrgram")
library("car")
library("lattice")
library("ROCR")

# Need to split the dataset into the training sample and the testing sample
# Will create 50/50 samples
sample_size = floor(0.5 * nrow(data))

# set the seed to make your partition reproductible
set.seed(1729)
train_set = sample(seq_len(nrow(data)), size = sample_size)

training = data[train_set, ]
testing = data[-train_set, ]

# Corrgram of the entire dataset
corrgram(data, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt,
         main="Corrgram of the data")

# Corrgram of the training dataset
corrgram(training, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt,
         main="Corrgram of the training data")

# Corrgram of the testing dataset
corrgram(testing, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt,
         main="Corrgram of the data")


# Run a scatterplot matrix on the entire dataset
scatterplotMatrix(~Pregnancies +
                    Glucose +
                    BloodPressure +
                    SkinThickness +
                    Insulin	+ 
                    BMI	+
                    DiabetesPedigreeFunction	+
                    Age	+
                    Outcome
                  ,data=data, main="Simple Scatterplot Matrix")

# Run a scatterplot matrix on the training dataset
scatterplotMatrix(~Pregnancies +
                    Glucose +
                    BloodPressure +
                    SkinThickness +
                    Insulin	+ 
                    BMI	+
                    DiabetesPedigreeFunction	+
                    Age	+
                    Outcome
                  ,data=training, main="Simple Scatterplot Matrix of the training dataset")

# Run a scatterplot matrix on the testing dataset
scatterplotMatrix(~Pregnancies +
                    Glucose +
                    BloodPressure +
                    SkinThickness +
                    Insulin	+ 
                    BMI	+
                    DiabetesPedigreeFunction	+
                    Age	+
                    Outcome
                  ,data=testing, main="Simple Scatterplot Matrix testing dataset")




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

pairs(data, upper.panel = panel.cor)

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

pairs(training, upper.panel = panel.cor)

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

pairs(testing, upper.panel = panel.cor)




# Look at the variable distributions
ggplot(data, aes(x = Pregnancies)) + 
  geom_bar() + 
  ggtitle("Distribution of Number of times pregnant for the entire dataset")

ggplot(training, aes(x = Pregnancies)) + 
  geom_bar() + 
  ggtitle("Distribution of Number of times pregnant for the training dataset")

ggplot(testing, aes(x = Pregnancies)) + 
  geom_bar() + 
  ggtitle("Distribution of Number of times pregnant for the testing dataset")

ggplot(data, aes(x = BloodPressure)) + 
  geom_bar() +
  ggtitle("DIstribution of Diastolic blood pressure (mm Hg) for the entire dataset")

ggplot(training, aes(x = BloodPressure)) + 
  geom_bar() +
  ggtitle("DIstribution of Diastolic blood pressure (mm Hg) for the training dataset")

ggplot(testing, aes(x = BloodPressure)) + 
  geom_bar() +
  ggtitle("DIstribution of Diastolic blood pressure (mm Hg) for the testing dataset")

ggplot(data, aes(x = SkinThickness)) + 
  geom_histogram(binwidth = 0.5) +
  ggtitle("Distribution of Triceps skin fold thickness (mm) for the entire dataset")

ggplot(training, aes(x = SkinThickness)) + 
  geom_histogram(binwidth = 0.5) +
  ggtitle("Distribution of Triceps skin fold thickness (mm) for the training dataset")

ggplot(testing, aes(x = SkinThickness)) + 
  geom_histogram(binwidth = 0.5) +
  ggtitle("Distribution of Triceps skin fold thickness (mm) for the testing dataset")

ggplot(data, aes(x = Insulin)) + 
  geom_histogram() +
  ggtitle("Distribution of the 2-Hour serum insulin (mu U/ml) for the entire dataset")

ggplot(training, aes(x = Insulin)) + 
  geom_histogram() +
  ggtitle("Distribution of the 2-Hour serum insulin (mu U/ml) for the training dataset")

ggplot(testing, aes(x = Insulin)) + 
  geom_histogram() +
  ggtitle("Distribution of the 2-Hour serum insulin (mu U/ml) for the testing dataset")

ggplot(data, aes(x = BMI)) + 
  geom_histogram(binwidth = 0.4) +
  ggtitle("DIstribution of Body mass index (BMI) for the entire dataset")

ggplot(training, aes(x = BMI)) + 
  geom_histogram(binwidth = 0.4) +
  ggtitle("DIstribution of Body mass index (BMI) for the training dataset")

ggplot(testing, aes(x = BMI)) + 
  geom_histogram(binwidth = 0.4) +
  ggtitle("DIstribution of Body mass index (BMI) for the testing dataset")

ggplot(data, aes(x = DiabetesPedigreeFunction)) + 
  geom_histogram(binwidth = 0.025) +
  ggtitle("Distribution of Diabetes pedigree function for the entire dataset")

ggplot(training, aes(x = DiabetesPedigreeFunction)) + 
  geom_histogram(binwidth = 0.025) +
  ggtitle("Distribution of Diabetes pedigree function for the training dataset")

ggplot(testing, aes(x = DiabetesPedigreeFunction)) + 
  geom_histogram(binwidth = 0.025) +
  ggtitle("Distribution of Diabetes pedigree function for the testing dataset")

ggplot(data, aes(x = Age)) + 
  geom_bar() + 
  ggtitle("Distribution of Age for the entire dataset")

ggplot(training, aes(x = Age)) + 
  geom_bar() + 
  ggtitle("Distribution of Age for the training dataset")

ggplot(testing, aes(x = Age)) + 
  geom_bar() + 
  ggtitle("Distribution of Age for the testing dataset")

ggplot(data, aes(x = Outcome)) + 
  geom_histogram() + 
  ggtitle("Distribution of Outcome for the entire dataset")

ggplot(training, aes(x = Outcome)) + 
  geom_histogram() + 
  ggtitle("Distribution of Outcome for the training dataset")

ggplot(testing, aes(x = Outcome)) + 
  geom_histogram() + 
  ggtitle("Distribution of Outcome for the testing dataset")


# Model Fitting
# Start off with this (alpha = 0.05)
model_algorithm = model <- glm(Outcome ~ Pregnancies +
                                 Glucose +
                                 BloodPressure +
                                 SkinThickness +
                                 Insulin	+ 
                                 BMI	+
                                 DiabetesPedigreeFunction	+
                                 Age ,
                               family=binomial(link='logit'),data=training)

print(summary(model_algorithm))
print(anova(model_algorithm, test="Chisq"))




# Settled on this
model_algorithm_final = model <- glm(Outcome ~ Glucose + BMI + DiabetesPedigreeFunction	,
                                     family=binomial(link='logit'),data=training)

print(summary(model_algorithm_final))
print(anova(model_algorithm_final, test="Chisq"))


# Apply the algorithm to the training sample
prediction_training = predict(model_algorithm_final,training, type = "response")
prediction_training = ifelse(prediction_training > 0.5, 1, 0)
error = mean(prediction_training != training$Outcome)
print(paste('Model Accuracy',1-error))

# Apply the algorithm to the testing sample
prediction_testing = predict(model_algorithm_final,testing, type = "response")
prediction_testing = ifelse(prediction_testing > 0.5, 1, 0)
error = mean(prediction_testing != testing$Outcome)
print(paste('Model Accuracy',1-error))

# Apply the algorithm to the entire dataset
prediction_data = predict(model_algorithm_final,data, type = "response")
prediction_data = ifelse(prediction_data > 0.5, 1, 0)
error = mean(prediction_data != data$Outcome)
print(paste('Model Accuracy',1-error))


# Get the ROC curve and the AUC
p = predict(model_algorithm_final, testing, type="response")
pr = prediction(p, testing$Outcome)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)


auc = performance(pr, measure = "auc")
auc = auc@y.values[[1]]
print(paste("Model Accuracy", auc))

