library(countrycode)
library(ggplot2)
library(plyr)

rm(list = ls())
setwd("C:/Users/vj853t/Documents/DataScience/Case Study/Case Study 4")
voting_data <- read.csv("~/DataScience/Case Study/Case Study 4/VotingData.csv")

str(voting_data)

#We are considering only the countries which are members and are present
voting_data <- voting_data[voting_data$vote <= 3, ]
str(voting_data)

#Adding a new row called Year
voting_data$Year <- 1945 + voting_data$session
head(voting_data)

#Adding country name
voting_data$country <- countrycode(voting_data$ccode, "cown", "country.name")
head(voting_data)

#1. Percent of positive votes across all years and countries
nrow(voting_data[voting_data$vote == 1, ])/nrow(voting_data)

#2. Trends in Positive, Negative and Abstain votes percent
#trends through the time (3 line charts with smoothing required?)
#total_votes <- setNames(aggregate(voting_data$Year, by = list(voting_data$Year), FUN = "length"), c("Year", "Total Votes"))
total_votes <- aggregate(voting_data$Year, by = list(voting_data$Year), FUN = "length")
colnames(total_votes) <- c("Year", "Total Votes")
head(total_votes)

#positive_votes <- setNames(aggregate(voting_data[voting_data$vote == 1, ]$Year, by = list(voting_data[voting_data$vote == 1, ]$Year), FUN = "length"), c("Year", "Total Votes"))
positive_votes <- aggregate(voting_data[voting_data$vote == 1, ]$Year, by = list(voting_data[voting_data$vote == 1, ]$Year), FUN = "length")
colnames(positive_votes) <- c("Year", "Positive Votes")
head(positive_votes)

#abstain_votes <- setNames(aggregate(voting_data[voting_data$vote == 2, ]$Year, by = list(voting_data[voting_data$vote == 2, ]$Year), FUN = "length"), c("Year", "Abstain Votes"))
abstain_votes <- aggregate(voting_data[voting_data$vote == 2, ]$Year, by = list(voting_data[voting_data$vote == 2, ]$Year), FUN = "length")
colnames(abstain_votes) <- c("Year", "Abstain Votes")
head(abstain_votes)

#negative_votes <- setNames(aggregate(voting_data[voting_data$vote == 3, ]$Year, by = list(voting_data[voting_data$vote == 3, ]$Year), FUN = "length"), c("Year", "Negative Votes"))
negative_votes <- aggregate(voting_data[voting_data$vote == 3, ]$Year, by = list(voting_data[voting_data$vote == 3, ]$Year), FUN = "length")
colnames(negative_votes) <- c("Year", "Negative Votes")
head(negative_votes)


votes_by_year <- Reduce(function(x,y) merge(x,y, all = TRUE), list(total_votes, positive_votes, abstain_votes, negative_votes))
head(votes_by_year)

votes_by_year$percent_positive <- round(votes_by_year$`Positive Votes`/votes_by_year$`Total Votes`, 2)
votes_by_year$percent_abstain <- round(votes_by_year$`Abstain Votes`/votes_by_year$`Total Votes`, 2)
votes_by_year$percent_negative <- round(votes_by_year$`Negative Votes`/votes_by_year$`Total Votes`, 2)


ggplot(votes_by_year, aes(x=Year, y=percent_positive)) + geom_line() + ggtitle('Trends in Percentage of Positive Votes') + xlab('Year') + ylab('Percentage Votes')
ggplot(votes_by_year, aes(x=Year, y=percent_abstain)) + geom_line() + ggtitle('Trends in Percentage of Abstain Votes') + xlab('Year') + ylab('Percentage Votes')
ggplot(votes_by_year, aes(x=Year, y=percent_negative)) + geom_line() + ggtitle('Trends in Percentage of Negative Votes') + xlab('Year') + ylab('Percentage Votes')

#3. Find top 5 countries which vote mostly positive.

total_votes_bycountry <- aggregate(voting_data$country, by = list(voting_data$country), FUN = "length")
colnames(total_votes_bycountry) <- c("Country", "Total_Votes")
head(total_votes_bycountry)

#positive_votes <- setNames(aggregate(voting_data[voting_data$vote == 1, ]$Year, by = list(voting_data[voting_data$vote == 1, ]$Year), FUN = "length"), c("Year", "Total Votes"))
positive_votes_bycountry <- aggregate(voting_data[voting_data$vote == 1,"country"], by = list(voting_data[voting_data$vote == 1, "country"]), FUN = "length")
colnames(positive_votes_bycountry) <- c("Country", "Positive_Votes")
head(positive_votes_bycountry)

abstain_votes_bycountry <- aggregate(voting_data[voting_data$vote == 2,"country"], by = list(voting_data[voting_data$vote == 2, "country"]), FUN = "length")
colnames(abstain_votes_bycountry) <- c("Country", "Abstain_Votes")
head(abstain_votes_bycountry)

negative_votes_bycountry <- aggregate(voting_data[voting_data$vote == 3,"country"], by = list(voting_data[voting_data$vote == 3, "country"]), FUN = "length")
colnames(negative_votes_bycountry) <- c("Country", "Negative_Votes")
head(negative_votes_bycountry)

votes_by_country <- Reduce(function(x,y) merge(x,y, all = TRUE), list(total_votes_bycountry, positive_votes_bycountry, abstain_votes_bycountry, negative_votes_bycountry))
head(votes_by_country)

votes_by_country$percent_positive <- round((votes_by_country$Positive_Votes/votes_by_country$Total_Votes),2)
votes_by_country$percent_abstain <- round((votes_by_country$Abstain_Votes/votes_by_country$Total_Votes),2)
votes_by_country$percent_negative <- round((votes_by_country$Negative_Votes/votes_by_country$Total_Votes),2)

top_positivevotes_countries <- head(arrange(votes_by_country,desc(percent_positive)), n=5)

#4. Find top 5 countries which vote mostly negative.

top_negativevotes_countries <- head(arrange(votes_by_country,desc(percent_negative)), n=10)

#5. Create stacked and grouped bar charts showing votes distribution from 1985 in 3 categories, Positive, Negative, Abstain.

vote_data <- voting_data
vote_data$vote[vote_data$vote == 1] = 'Positive'
vote_data$vote[vote_data$vote == 2] = 'Abstain'
vote_data$vote[vote_data$vote == 3] = 'Negative'

vote_data$vote <- as.factor(vote_data$vote)
str(vote_data)

ggplot(vote_data[vote_data$Year > 1985,], aes(Year, fill=vote)) + geom_bar()


