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

# Data for summary tables.
# Get numeric variables and group variable (attrition) for table select input box.
tableData <- employData %>% select(Attrition,Gender, Age, DailyRate, DistanceFromHome, HourlyRate, MonthlyIncome, MonthlyRate, NumCompaniesWorked, PercentSalaryHike, TotalWorkingYears, YearsWithCurrManager, TrainingTimesLastYear, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion)
as.data.frame(tableData)


# Image for the about tab.
aboutImage <- "happyofficeworker.png"


# Save link to the data.
hrDataLink <- "https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset"



shinyUI(fluidPage(
  titlePanel("Employee Attrition"),
  sidebarLayout(
    sidebarPanel(
      # Create widgets.
      h5("Select three variables for scatter plots and one categorical variable for box plots and push play to produce plots."),
      selectInput("numVarOne", "Numeric Variable One", choices=c(not_sel)),
      selectInput("numVarTwo", "Numeric Variable Two", choices = c(not_sel)),
      selectInput("factVars", "Categorical Variable", choices = c(not_sel)),
      actionButton("runButton", "Run Analysis", icon = icon("play")),
      br(),
      radioButtons("stats", "Select the Type of Summary", choices = list("Group by Attrition" = "Attrition", "Group by Gender" = "Gender"), selected = "Attrition"),
      selectInput("sumVars", "Select Summary Variables", choices = unique(names(tableData)), selected = "Age", multiple = FALSE, selectize = TRUE)
    ),
    mainPanel(
      # Show output and create tabs.
      tabsetPanel(type = "tab",
                  tabPanel("About",
                           # Load Image
                           img(
                             src = aboutImage,
                             height = "300px",
                             width = "500px"
                           ),
                           h3("Purpose of the App"),
                           "This app helps identify the factors that lead to", "employee attrition by exploring questions such as comparing monthly",
                           "income by education and attrition.",
                           h3("Data Set"),
                           "The data set is a fictional data set created by",
                           "IBM Data Scientist.  The data set presents an",
                           "employee survey from IBM, indicating if there is","attrition or not.  The data set includes variables related to attrition ",
                           "and employee performance such as Age,",
                           "Department, Hourly Rate, etc.",
                           "The data set was downloaded from Kaggle",
                           a(href = hrDataLink, "here"),".",
                           h3("Tabs"),
                           tags$ul(
                             tags$li(
                               "About: Purpose of app and brief discussion of", "the data."),
                             tags$li(
                               "Data Exploration: Data visualizations and", "summaries of the data."),
                             tags$li(
                               "Modeling: Information on three supervised", "learning models, allowing the user to fit models, and make predictions", "based on user input."),
                             tags$li(
                               "Data: Allows the user to explore the data set", "and save the data file.")
                           )
                  ),
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

#Comment