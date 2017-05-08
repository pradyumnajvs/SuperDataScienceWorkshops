#Let us call the library functions which are most needed for this data challenge
library(ggplot2)
library(plyr)

rm(list = ls())
#Let's set the correct cowkring directory
setwd("C:/Users/venkatajanga/Documents/DataScience/Case Study/Case Study 2")
#Reading the data set
crime_data <- read.csv("~/DataScience/Case Study/Case Study 2/CrimeData2.csv")

#Let us check the column names of the dataset
names(crime_data)

#Check the structure
str(crime_data)

##Let us now jump into the questions provided in the problem statement

#1. Overall trend in crimes for the whole period of time in the dataset. The granularity should be at the Day level.

# This converts the current data time format to YYYY-MM-DD HH:MM:SS format according to the EST time zone.
crime_data$DateTime <- as.POSIXct(crime_data$Dispatch_Date_Time, format = "%Y-%m-%d %H:%M:%S", tz="EST")

#Check the sample of the rows in the newly created column
head(crime_data$DateTime)

#Check the structure after the datetime conversion
str(crime_data)

#Need to create a new column to store data in dates format
crime_data$Date <- as.Date(crime_data$DateTime, tz="EST")

#Recheck the structure after the addition of new column
str(crime_data)

# Now let us group all the crimes based on the date and check the number of crimes per day. 
#This gives us the trend of count fo crimes per day
crimecount_bydate <- aggregate(crime_data$Date, list(date = crime_data$Date), FUN=length)

#Check the structure of newly created variable
str(crimecount_bydate)

#Rename the columns to make it easy to work on
colnames(crimecount_bydate) <- c('Date', 'CrimeCount')

#Plot the number of crimes across every day as mentioned in the question. We are granulating to the day level
ggplot(crimecount_bydate, aes(Date, CrimeCount, color = Date)) + geom_line() + ggtitle('Crime Count Distribution by Day')

#2. Which are the most and the least dangerous hours in Philadelphia?
#strptime function converts character vectors to class "POSIXlt"
crime_data$Hour <- strftime(crime_data$DateTime, format = '%H', tz='EST')
#As usual, let us check the strucutre after adding the new column
str(crime_data)

# Since we are checking the seasonality in crime, we need to group all the crimes that occur in each month. 
#This gives us the trend of crimes per month in a day
crimecount_byhour <- aggregate(crime_data$Hour, list(Hour = crime_data$Hour), FUN=length)

#Check the structure of newly created variable
str(crimecount_byhour)

#Rename the columns to make it easy to work on
colnames(crimecount_byhour) <- c('Hour', 'CrimeCount')

#It is preferrable to have the hour column as integer. So, let's convert it
crimecount_byhour$Hour <- as.integer(crimecount_byhour$Hour)

#Plot the number of crimes across every hour in a day as mentioned in the question. We are granulating to the hour level
ggplot(crimecount_byhour, aes(Hour, CrimeCount)) + geom_line(color = 'Red') + ggtitle('Crime Count Distribution by Hour')

#3. Is there any seasonality in the crime rate?
#strptime function converts character vectors to class "POSIXlt". However, unlike in the previous case, we are not formatting
#to the hour level, indtead we are granualting to the month level
crime_data$Month <- strftime(crime_data$DateTime, format = '%m', tz='EST')
str(crime_data)

# Since we are checking the most dangerous hours in the day, we need to group all the crimes based on the month. 
#This gives us the trend of count of crimes per hour in an year
crimecount_bymonth <- aggregate(crime_data$Month, list(Month = crime_data$Month), FUN=length)

#Check the structure of newly created variable
str(crimecount_bymonth)

#Rename the columns to make it easy to work on
colnames(crimecount_bymonth) <- c('Month', 'CrimeCount')

#Check the structure of newly modified variable
str(crimecount_bymonth)

#Plot the number of crimes across every month in an year as mentioned in the question. We are granulating to the month level
ggplot(crimecount_bymonth, aes(Month, CrimeCount)) + geom_bar(fill = 'Blue', stat = "identity") + ggtitle('Crime Count Distribution by Month')

#4. What are the top 10 crimes crime types?
# Since we are checking the most frequently committed crimes, we need to group all the crimes based on the type of crime 
#This gives us the trend of count of crimes per type in an year
top_crimetypes <- aggregate(crime_data$Text_General_Code, list(Type = crime_data$Text_General_Code), FUN = length)

#Rename the columns to make it easy to work on
colnames(top_crimetypes) <- c('CrimeType', 'Count')

#Check the structure of newly created variable
str(top_crimetypes)

#Sorting the crime types based on the descending order of the count of occurances
top_crimetypes <- head(arrange(top_crimetypes,desc(Count)), n=33)

#Plot the number of crimes per crime type in an year as mentioned in the question.
ggplot(top_crimetypes, aes(CrimeType, Count)) + geom_bar(fill = 'Blue', stat = "identity") + ggtitle('Crime Count Distribution by Crime Type')

#5. Which police HQ is in the most need of strengthening?
crimecount_bypoliceHQ <- aggregate(crime_data$Dc_Dist, list(Type = crime_data$Dc_Dist), FUN = length)
#Rename the columns to make it easy to work on
colnames(crimecount_bypoliceHQ) <- c('District', 'CountOfCrime')

#Check the structure of newly created variable
str(crimecount_bypoliceHQ)

#Sorting the disticts based on the descending order of the count of crime occurances
head(arrange(crimecount_bypoliceHQ, desc(CountOfCrime)), n=10)
