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
library(ggplot2)
library(data.table)

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





shinyUI(fluidPage(
  titlePanel("Employee Attrition"),
  sidebarLayout(
    sidebarPanel(
      # Create widgets.
      h4("Choose variables with the select boxes below."),
      selectInput("numVarOne", "Numeric Variable One", choices=c(not_sel)),
      selectInput("numVarTwo", "Numeric Variable Two", choices = c(not_sel)),
      selectInput("factVars", "Categorical Variable", choices = c(not_sel)),
      radioButtons("plot", "Select the Type of Plot/Summary", choices = list("Bar Plot" = "bar", "Scatterplot" = "scatter", "Table" = "table"), selected = "bar"),
      h5("Select three variables for scatter plots and one categorical variable for box plots and push play to produce plots."),
      actionButton("runButton", "Run Analysis", icon = icon("play")),
    ),
    mainPanel(
      # Show output and create tabs.
      tabsetPanel(type = "tab",
                  tabPanel("About"),
                  tabPanel("Data Exploration", plotOutput("plotOne"), dataTableOutput("summary")),
                  tabPanel("Modeling",
                           tabsetPanel(
                             tabPanel("Modeling Info"),
                             tabPanel("Model Fitting"),
                             tabPanel("Prediction"))),
                  tabPanel("Data"))
    )
  ))
)

#comment