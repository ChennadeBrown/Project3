library(caret)
library(shiny)
library(DT)
library(knitr)
library(GGally)
library(dplyr)
library(caret)
library(tree)
library(rpart)
library(rattle)
library(tidyverse)
employData <- read_csv("EmployeeAttrition.csv")



shinyUI(fluidPage(
  titlePanel("Predicting Employee Attrition"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Categorical or Numeric Predictors for Plots:"),
    selectInput("catVariable", label = "Categorical Predictors", selected = "Attrition", choices = catData),
    selectInput("numVariables",label = "Numeric Predictors", selected = "Age", choices = numericVars),
  ),
    mainPanel(
    )
  )
)
)