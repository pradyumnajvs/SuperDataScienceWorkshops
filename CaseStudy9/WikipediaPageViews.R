library(plyr)
library(ggplot2)

rm(list = ls())
setwd("C:/Users/vj853t/Documents/DataScience/Case Study/Case Study 9")
wiki_data <- read.csv("~/DataScience/Case Study/Case Study 9/wikipedia_data_full.csv")

#Let's check the structure of the dataset
str(wiki_data)

#Let us filter all the duplicate data
wiki_data <- unique(wiki_data)

#Let us remove NAs from the dataset
wiki_data <- wiki_data[(!is.na(wiki_data$views)),]

colnames(wiki_data)

wiki_data$source <- as.character(wiki_data$source)
wiki_data$page_name <- as.character(wiki_data$page_name)

#Let's check the structure of the dataset
str(wiki_data)

wiki_data$language <- gsub("_.+", "", wiki_data$source)

view_count <- aggregate(list(views = wiki_data$views), list(language = wiki_data$language), FUN = sum)

view_count = view_count[order(view_count$views, decreasing = TRUE),]

top10_views = head(arrange(view_count,desc(views)), n = 10)

qplot(language, data = top10_views, geom = 'bar', weight = views, fill = 'red')+
  ggtitle('Views by language')+
  xlab('Language')+
  ylab('Number of views')

pageview_count <- aggregate(list(views = wiki_data$views), list(pagename = wiki_data$page_name), FUN = sum)

pageview_count = pageview_count[order(pageview_count$views, decreasing = TRUE),]

qplot(page_name, data = pageview_count, geom = 'line', weight = views, fill = 'red')+
  ggtitle('Views by language')+
  xlab('Language')+
  ylab('Number of views')
