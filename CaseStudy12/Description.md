Disclaimer: SuperDataScience has no affiliation with data sources. The scenario is made up for educational purposes.
You are a Data Analyst working for the police department. You have been given a data set with a large number of variables and have been asked by the police chief to build a regression model to predict crime rates.
1. Clean variable_names and set the names of data to these clean names
2. The police chief is only interested in the following variables so create a new data frame with just these variables included: 'ViolentCrimesPerPop', 'pctUrban', 'agePct16t24', 'PctUnemployed', 'medIncome'
3. Check for correlations between medIncome and PctUnemployed. Plot these variables to confirm correlations
4. Split the data into training and testing sets using set.seed(123)
5. Build a linear regression model using pctUrban, agePct16t24 and whichever of the variables from question 3 is best correlated with ViolentCrimesPerPop
6. Predict the ViolentCrimesPerPop for the testing set
7. Calculate the R2 of the testing set
