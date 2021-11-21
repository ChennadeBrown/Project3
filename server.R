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
library(ggplot2)
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

catVars <- select(employData, Attrition, Department, JobRole, Gender, EducationField, BusinessTravel, EnvironmentSatisfaction, JobInvolvement, JobLevel, JobSatisfaction, MaritalStatus, Over18, OverTime, PerformanceRating, RelationshipSatisfaction, StockOptionLevel, WorkLifeBalance)

# Change categorical variables to factors for plots.
catVars$Attrition <- as.factor(catVars$Attrition)
catVars$Department <- as.factor(catVars$Department)
catVars$JobRole <- as.factor(catVars$JobRole)
catVars$Gender <- as.factor(catVars$Gender)
catVars$EducationField <- as.factor(catVars$EducationField)
catVars$BusinessTravel <- as.factor(catVars$BusinessTravel)
catVars$EnvironmentSatisfaction <- as.factor(catVars$EnvironmentSatisfaction)
catVars$JobInvolvement <- as.factor(catVars$JobInvolvement)
catVars$JobLevel <- as.factor(catVars$JobLevel)
catVars$JobSatisfaction <- as.factor(catVars$JobSatisfaction)
catVars$MaritalStatus <- as.factor(catVars$MaritalStatus)
catVars$Over18 <- as.factor(catVars$Over18)
catVars$OverTime <- as.factor(catVars$OverTime)
catVars$PerformanceRating <- as.factor(catVars$PerformanceRating)
catVars$RelationshipSatisfaction <- as.factor(catVars$RelationshipSatisfaction)
catVars$StockOptionLevel <- as.factor(catVars$StockOptionLevel)
catVars$WorkLifeBalance <- as.factor(catVars$WorkLifeBalance)
as.data.frame(catVars)



shinyServer(function(input, output) {
  output$attritionplots <- renderPlot({
    g <- ggplot(catVariable, aes(x = Attrition))
    
    if(input$plot == "bar"){
      g + geom_bar(aes_string(fill = input$catVariable)) +
        labs(title = "Bar Plot") +
        theme(axis.text.x = element_text(angle = 45))
    } else if(input$plot == "scatter"){
      ggplot(numericVariables, aes_string(x = input$numericVariables, y = input$numericVariables, color = "Attrition2")) +
        geom_jitter() + 
        geom_smooth(method = "lm") +
        labs(title = "Scatterplot to Show Relationship between Predictors & Attrition")
    }
  })
  
  output$summary <- DT::renderDataTable({
    sumTab <- numericVars %>% group_by(Attrition2) %>% summarise(
      Avg_Age = round(mean(Age),0),
      Avg_DailyRate = round(mean(DailyRate),0),
      Avg_MonthlyIncome = round(mean(MonthlyIncome),0), 
      Avg_HourlyRate = round(mean(HourlyRate),0),
      Avg_WorkingYrs = round(mean(TotalWorkingYears),0),
      Avg_YrsatComp = round(mean(YearsAtCompany),0),
      Avg_PctSalaryHike = round(mean(PercentSalaryHike),0), 
      Avg_TotalWkgYrs = round(mean(TotalWorkingYears), 0), 
      Avg_YrsinRole = round(mean(YearsInCurrentRole), 0),
      Avg_TrainingTimeLastYr = round(mean(TrainingTimesLastYear), 0),
      Avg_Distance = round(mean(DistanceFromHome), 0),
      Avg_Monthly = round(mean(MonthlyRate), 0),
      Avg_NoCoWorked = round(mean(NumCompaniesWorked), 0),
      Avg_YrsPromo = round(mean(YearsSinceLastPromotion),0)) 
    as.data.frame(sumTab)
  })
})

