############################################################################################################
#                                                                                                          #
#           Regression model:  admission_chance  =  gre_score  toefl_score  sop  lor  cgpa   research      #
#                                                                                                          #
#   gre_score: The Graduate Record Examination score, out of 340                                           #
#   toefl_score: TOEFL Score, out of 120                                                                   #
#   sop: Statement of purpose and letter of ecommendation Strength, out of 5                               #
#   lor: Letter of Recommendation Strength  (1 to 5)                                                       #
#   cgpa: Undergraduate GPA, out of 10                                                                     #
#   research: Candidate’s research experience (0 or 1)                                                     #
#   admission_chance: The probability of being admitted, ranging from 0 to 1                               #
#                                                                                                          #
######################################## KADIMA GLOIRE KABEYA ##############################################

#Installing packages if necessary

#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("caret")
#install.packages("dplyr")
#install.packages("prais")
#install.packages("car")
#Importing packages
#library(tidyverse)

library(ggplot2)
library(caret)
library(dplyr)
library(lmtest)
library(prais)
library(car)


#importing data set and considering empty cells as missing values
data = read.csv("https://drive.google.com/uc?export=download&id=1hCmY68acDX601HDZA73uSluUHEdYjTND", na.strings=c("", "NA"))

#displaying all the data
View(data)

#Dimensions of the data frame and names of variables
dim(data)
colnames(data)

#Summary for each variable
summary(data)

#correlation matrix
cor(cbind(data$gre_score, data$toefl_score, data$sop, data$lor, data$cgpa, data$admission_chance))

#Some graphs
hist(data$gre_score, xlab = "GRE score", col = "#ffffff", main = "Gre score")
hist(data$toefl_score, xlab = "Toefl score", col = "#ffffff", main = "Toefl score")

plot(data$admission_chance, x = data$gre_score,
     xlab = "Gre score",
     ylab = "Admission chance",
     main = "Admission chance vs Gre score"
)

plot(data$admission_chance, x = data$toefl_score,
     xlab = "Toefl score",
     ylab = "Admission chance",
     main = "Admission chance vs Toefl score"
)

#Splitting the data set into training and test data set
##Fixed random index sample
set.seed(123)
trainIndex = createDataPartition(data$admission_chance, p = 0.80,
                                 list = FALSE, 
                                 times = 1)
##Selecting the training observations from the index sample
trainData = data[trainIndex,]
#View(trainData)
dim(trainData)

##The remaining observations are the test data
testData = data[-trainIndex,]
#View(testData)
dim(testData)

##We estimate or train a multiple Linear Regression model
mlr_model = lm(admission_chance ~ ., data = trainData)
summary(mlr_model)

##We reestimate or train the multiple Linear Regression model without non-significant variables SH, ST and GD
mlr_model = lm(admission_chance ~ ., data = data[, !(names(data) %in% c("sop"))])
summary(mlr_model)

#Testing the errors normality
shapiro.test(mlr_model$residuals)

#Testing the errors non-autocorrelation
dwtest(mlr_model)
acf(mlr_model$residuals)

#Testing the errors homoscedasticity
ncvTest(mlr_model)

## Predicting Exam_score on the test set
mlr_predictions = predict(mlr_model, testData)

# Evaluating the model performance
MAE_mlr <- mean(abs(mlr_predictions - testData$admission_chance))
RMSE_mlr <- sqrt(mean((mlr_predictions - testData$admission_chance)^2))

cat("Multiple Linear Regression MAE:", MAE_mlr, "\nMultiple Linear Regression RMSE:", RMSE_mlr, "\n")

# Visual exam of residuals
plot(mlr_model, which = 1)
plot(mlr_model, which = 2)
hist(residuals(mlr_model), main = "Histogram of Residuals", xlab = "Residuals", breaks = 20)

# Observed VS Predicted Exam score
ggplot(testData, aes(x = admission_chance, y = mlr_predictions)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicted vs. Observed admission chances", x = "Observed admission chances", y = "Predicted admission chances") +
  theme_minimal()

