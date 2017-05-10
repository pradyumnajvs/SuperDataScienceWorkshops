library(ggplot2)

rm(list = ls())
setwd("C:/Users/vj853t/Documents/DataScience/Case Study/Case Study 6")
#titanicdata <- read.table(titanic_data.csv, header = TRUE, sep =",")

titanic_data <- read.csv("~/DataScience/Case Study/Case Study 6/titanic_data.csv")

names(titanic_data)

colnames(titanic_data) = c('passanger_id', 'survived_status', 'passanger_class', 'name', 'sex', 'age', 'number-sibiling_spouse', 'parch_staus', 'ticket', 'fare', 'cabin', 'embarked')

str(titanic_data)

titanic_data$passanger_class[titanic_data$passanger_class == 1] = '1st Class'
titanic_data$passanger_class[titanic_data$passanger_class == 2] = '2nd Class'
titanic_data$passanger_class[titanic_data$passanger_class == 3] = '3rd Class'

class(titanic_data$passanger_class)

titanic_data$passanger_class <- as.factor(titanic_data$passanger_class)

#1. Number of Passenger in the ship

nrow(titanic_data)

#2. Who bought the most expensive tickets
View(titanic_data[order(titanic_data$fare),])

#3. How many men and women survived
survivors_sex = aggregate(titanic_data$survived_status, by = list(sex = titanic_data$sex), FUN = sum)

ggplot(survivors_sex, aes(x=sex, y=x)) + geom_bar(stat="identity") + 
  labs(x="Sex", y="Numberof Survivors") + ggtitle("Survivors by Sex")

#4. How many passengers are included in the data by class?

survivors_class = aggregate(titanic_data$passanger_class, by = list(class = titanic_data$passanger_class), FUN = length)

survivors_class

ggplot(survivors_class, aes(x=class, y=x)) + geom_bar(stat="identity") + 
  labs(x="Passenger Travel Class", y="Number of Survivors") + ggtitle("Survivors by Class")

#5. How many people had the title 'Mrs'?
title_mrs <- titanic_data[grepl('Mrs', titanic_data$name)]

#6. What is the correlation between Age and Fare? (please provide a scatter plot)
cor(titanic_data$age, titanic_data$fare, use='complete.obs')

#7. What is the distribution of fares? (please provide a histogram)