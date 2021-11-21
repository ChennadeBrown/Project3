library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(knitr)
library(GGally)
library(dplyr)
library(caret)
library(tree)
library(rpart)
library(rattle)
employData <- read_csv("EmployeeAttrition.csv")

# Clean Data.
# Remove non predictor variables from the data set.
employData$EmployeeCount <- NULL
employData$EmployeeNumber <- NULL
employData$StandardHours <- NULL



# Store categorical and numeric variables in separate data frames for automation of plots.
numericVars <- select(employData, Age, Attrition, TotalWorkingYears, MonthlyIncome, YearsAtCompany, PercentSalaryHike, TrainingTimesLastYear, YearsInCurrentRole, DailyRate, DistanceFromHome, HourlyRate, MonthlyRate, NumCompaniesWorked, YearsSinceLastPromotion, YearsWithCurrManager)

# Create binary variable Attrition2 for scatterplots.
numericVars <- numericVars %>% mutate(Attrition2 = ifelse(Attrition == "No", "0", "1"))
numericVars$Attrition <- as.factor(numericVars$Attrition)

# Remove categorical Attrition.
numericVars$Attrition <- NULL
numericVars

catVars <- select(employData, Attrition, Department, JobRole, Gender, EducationField, BusinessTravel, EnvironmentSatisfaction, JobInvolvement, JobLevel, JobSatisfaction, MaritalStatus, Over18, OverTime, PerformanceRating, RelationshipSatisfaction, StockOptionLevel, WorkLifeBalance)

# Store the predictors and the response variables in vectors.
catPred = names(catVars)[1:17]

# Name the vectors.
catPred = set_names(catPred)

# Change categorical variables to factors for plotting.
factors <- c("Attrition" ,"Department", "JobRole", "Gender", "EducationField", "BusinessTravel", "EnvironmentSatisfaction", "JobInvolvement", "JobLevel", "JobSatisfaction", "MaritalStatus", "Over18", "OverTime", "PerformanceRating", "RelationshipSatisfaction", "StockOptionLevel", "WorkLifeBalance")
catData <- catVars
catData[,factors] <- lapply(catData[,factors] , factor)


shinyServer(function(input, output) {
  
})