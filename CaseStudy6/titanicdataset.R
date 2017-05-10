#Let us call the library functions which are most needed for this data challenge
library(ggplot2)
library(plyr)

rm(list = ls())
#Let's set the correct working directory
setwd("C:/Users/vj853t/Documents/DataScience/Case Study/Case Study 6")

#Read the CSV file and assign it to a variable
titanic_data <- read.csv("~/DataScience/Case Study/Case Study 6/titanic_data.csv")

#Check the Column Names of the dataset
names(titanic_data)

#Assigning Column Names which are comfortable
colnames(titanic_data) = c('passanger_id', 'survived_status', 'passanger_class', 'name', 'sex', 'age', 'number-sibiling_spouse', 'parch_staus', 'ticket', 'fare', 'cabin', 'embarked')

#Check the structure of the dataset
str(titanic_data)

#Passenger Class is Int class with 1, 2 and 3 as options. We prefer to classify as class instead of different integers
titanic_data$passanger_class[titanic_data$passanger_class == 1] = '1st Class'
titanic_data$passanger_class[titanic_data$passanger_class == 2] = '2nd Class'
titanic_data$passanger_class[titanic_data$passanger_class == 3] = '3rd Class'

#Check the class of Passanger Class Column
class(titanic_data$passanger_class)

#Since Passanger Class Column is Character, we are converting it to factor class with 3 levels (1st, 2nd and 3rd class)
titanic_data$passanger_class <- as.factor(titanic_data$passanger_class)

#Let us now jump into the questions provided in the problem statement

#1. Number of Passengers in the ship
nrow(titanic_data)

#2. Who bought the most expensive tickets
expensive_tickets <- head(titanic_data[order(titanic_data$fare, decreasing= T),], n=10)

#3. How many men and women survived
survivors_sex = aggregate(titanic_data$survived_status, by = list(sex = titanic_data$sex), FUN = sum)

#Rename the columns to make it easy to work on
colnames(survivors_sex) <- c('Sex', 'Count')

#Check the structure of newly created variable
str(survivors_sex)

ggplot(survivors_sex, aes(Sex, Count)) + geom_bar(stat="identity") + 
  labs(x="Sex", y="Number of Survivors") + ggtitle("Number of Survivors by Sex")

#4. How many passengers are included in the data by class?
survivors_class = aggregate(titanic_data$passanger_class, by = list(class = titanic_data$passanger_class), FUN = length)
survivors_class

#Plot the passengers based on the class they travelled
ggplot(survivors_class, aes(x=class, y=x)) + geom_bar(stat="identity") + 
  labs(x="Passenger Travel Class", y="Number of Survivors") + ggtitle("Survivors by Class")

#5. How many people had the title 'Mrs'?
#Grepl is a command that can search for a specific substring in a string. We will use grepl to search which string in the entire column
#consists of the "Mrs" in their name
nrow(titanic_data[grepl('Mrs', titanic_data$name),])

#6. What is the correlation between Age and Fare? (please provide a scatter plot)
#Cor command can complare the correlation between 2 variables
cor(titanic_data$age, titanic_data$fare, use='complete.obs')

#7. What is the distribution of fares? (please provide a histogram)
#Hist command produces the histogram of the 
hist(titanic_data$fare)
