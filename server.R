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

# Read in the data.
# Set file name for data.
dataFile <- "./HR-Employee-Attrition.csv"
dataFile

employData <- read_csv(dataFile)
employData

# Clean Data.
# Remove non predictor variables from the data set.
employData$EmployeeCount <- NULL
employData$EmployeeNumber <- NULL
employData$StandardHours <- NULL

# Convert Attrition to a factor.
#employData %>% 
#mutate(
#Attrition = as.factor(Attrition))

not_sel <- "Not Selected"


shinyServer(function(input, output) {
  
  #get numeric columns.
  getData <- reactive({
    newDataN <- select_if(employData, is.numeric)
    newDataN
  })
  
  # get character columns.  
  getData2 <- reactive({
    newDataF <- select_if(employData, is.character)
    newDataF
  })
  
  #Update choices with numeric variables.  
  observeEvent(getData(),{
    choices <- c(not_sel, names(getData()))
    updateSelectInput(inputId = "numVarOne", choices = (choices))
    updateSelectInput(inputId = "numVarTwo", choices = choices)
  })
  
  #Update choices with categorical variables.
  observeEvent(getData2(), {
    choices <- c(not_sel, names(getData2()))
    updateSelectInput(inputId = "factVars", choices = choices)
  })
})





