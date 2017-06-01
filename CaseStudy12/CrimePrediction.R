##Let us call the library functions which are most needed for this data challenge
library(plyr)
library(ggplot2)
library(caTools)

rm(list = ls())
#Let's set the correct working directory
setwd("C:/Users/vj853t/Documents/DataScience/Case Study/Case Study 12")

#Let us assign the dataset to variables
var_name <- read.csv("~/DataScience/Case Study/Case Study 12/variable_names.csv", header = FALSE)
data <- read.csv("~/DataScience/Case Study/Case Study 12/data.csv", header = FALSE)

#Check the structure of each dataset
str(var_name)
str(data)

#1. Clean variable_names and set the names of data to these clean names

#Let us clean the first column by removing the first 4 letters in the entire phrase.
var_name$clean = substr(var_name$V1, 4, 1000)
#We will only consider the word before ':' in the entire string
var_name$clean = gsub(":.+",'', var_name$clean)
#Now assign the words in Clean column from var_name dataset as column names for dataset named data
names(data) <- var_name$clean
#Check to make sure that the column names are properly assigned
names(data)

#2. The police chief is only interested in the following variables so create a new data frame with
#just these variables included: 'ViolentCrimesPerPop', 'pctUrban', 'agePct16t24', 'PctUnemployed', 'medIncome'
model_data <- data[, names(data) %in% c('ViolentCrimesPerPop', 'pctUrban',
                                        'agePct16t24', 'PctUnemployed', 'medIncome')]

# 3. Check for correlations between medIncome and PctUnemployed. Plot these variables to confirm correlations
cor(model_data$medIncome, model_data$PctUnemployed)
cor(model_data$medIncome, model_data$ViolentCrimesPerPop)
cor(model_data$PctUnemployed, model_data$ViolentCrimesPerPop)
cor(model_data$PctUnemployed, model_data$agePct16t24)

plot(model_data$medIncome, model_data$PctUnemployed,
xlab = "Median Household Income",
ylab = " percentage of people 16 and over, in the labor force, and unemployed") 
+ title(main ="Median Income per percentage of people 16 and over, in the labor force, and unemployed")

plot(model_data$medIncome, model_data$ViolentCrimesPerPop, 
     xlab = "Median Household Income",
     ylab = " Total amount of crimes per 100K population") 
+ title(main ="Median Income per Total amount of crimes")

plot(model_data$PctUnemployed, model_data$ViolentCrimesPerPop, 
     xlab = "Percentage of Unemployed Population",
     ylab = "Total amount of crimes per population")
+ title(main ="Percentage of Unemployed Population per Total amount of crimes per 100K population")

plot(model_data$PctUnemployed, model_data$agePct16t24, 
     xlab = "Percentage of Unemployed Population",
     ylab = "percentage of population that is 16-24 in age")
+ title(main ="Percentage of Unemployed Population per percentage of population that is 16-24 in age")

#4. Split the data into training and testing sets using set.seed(123)
set.seed(123)
split = sample.split(model_data$ViolentCrimesPerPop, SplitRatio = 0.8)

data_train <- model_data[split,] 
data_test <- model_data[!split,]

#5. Build a linear regression model using pctUrban, agePct16t24 and whichever of the variables from 
# question 3 is best correlated with ViolentCrimesPerPop
model = lm(formula = ViolentCrimesPerPop ~ medIncome + PctUnemployed + pctUrban + agePct16t24, data = data_train)
model = lm(formula = ViolentCrimesPerPop ~ medIncome + PctUnemployed + pctUrban, data = data_train)
summary(model)

#6. Predict the ViolentCrimesPerPop for the testing set
test_predicted_data <- predict(model, newdata = data_test)

#7. Calculate the R2 of the testing set
ss_residual <- sum((test_predicted_data - data_test$ViolentCrimesPerPop)^2)
ss_total <- sum((data_test$ViolentCrimesPerPop - mean(data_test$ViolentCrimesPerPop))^2)
r_square <- 1 - ss_residual/ss_total
r_square
