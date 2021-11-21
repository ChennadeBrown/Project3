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
  headerPanel(title = "Predicting Employee Attrition"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Categorical Numeric or Summary Variables:"),
      # Create select boxes.
      selectInput("catVariable", label = "Categorical Predictors", selected = "Gender", choices = catVars),
      selectInput("numericVariables",label = "Numeric Predictors", selected = "Age", choices = numericVars),
      selectInput("tableVars", label= "Summary Variables", selected = "Avg_Age", choices = sumTab),
      # Radio buttons for user input.
      radioButtons("plot", "Select the Type of Summary", choices = list("Bar Plot" = "bar", "Scatterplot" = "scatter"), selected = "bar")
    ),
    mainPanel(
      # Show output and create tabs.
      tabsetPanel(type = "tab",
                  tabPanel("About"),
                  tabPanel("Data Exploration", plotOutput("attritionplots"), dataTableOutput("summary")),
                  tabPanel("Modeling",
                           tabsetPanel(
                             tabPanel("Modeling Info"),
                             tabPanel("Model Fitting"),
                             tabPanel("Prediction"))),
                  tabPanel("Data")
      )
    )
  ))
)