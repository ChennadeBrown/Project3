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

# Scatterplot & Barplot functions.
draw_plotOne <- function(getData, numVarOne, numVarTwo, factVars){
  if(numVarOne != not_sel &
     numVarTwo != not_sel &
     factVars != not_sel){
    ggplot(data = getData,
           aes_string(x = numVarOne, y = numVarTwo,
                      color = factVars)) +
      geom_point()}
  else if(numVarOne == not_sel & 
          numVarTwo == not_sel & 
          factVars != not_sel){
    ggplot(data = getData,
           aes_string(x = factVars)) +
      geom_bar()
  }}

shinyServer(function(input, output) {
  
  #get numeric columns.
  getData <- reactive({
    newDataN <- select_if(employData, is.numeric) %>% mutate(Attrition = as.factor(employData$Attrition), BusinessTravel = as.factor(employData$BusinessTravel), Department = as.factor(employData$Department), EducationField = as.factor(employData$EducationField), Gender = as.factor(employData$Gender), JobRole = as.factor(employData$JobRole), MaritalStatus = as.factor(employData$MaritalStatus), Over18 = as.factor(employData$Over18), OverTime = as.factor(employData$OverTime))
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
  
  #Reactive function for Run button to control output.
  numVarOne <- eventReactive(input$runButton, input$numVarOne)
  numVarTwo <- eventReactive(input$runButton, input$numVarTwo)
  factVars <- eventReactive(input$runButton, input$factVars)
  
  
  
  # Create the plot object.
  plotOne <- eventReactive(input$runButton,{
    draw_plotOne(getData(), numVarOne(), numVarTwo(), factVars())
  })
  
  # Render plot.
  output$plotOne <- renderPlot(plotOne())
  
  
  
  
  
  # Create table.
  # Create table.
  
  
})



