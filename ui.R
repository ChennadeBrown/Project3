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

# Filter Rows for Data table.
mychoices <- unique(employData)


shinyUI(fluidPage(
  titlePanel("Employee Attrition"),
  sidebarLayout(
    sidebarPanel(
      # Create widgets.
      h4(strong("Data Exploration Tab")),
      h5("Select three variables for scatter plots and one categorical               variable for box plots and push play to produce plots."),
      selectInput("numVarOne", "Numeric Variable One", choices=c(not_sel)),
      selectInput("numVarTwo", "Numeric Variable Two", choices = c(not_sel)),
      selectInput("factVars", "Categorical Variable", choices = c(not_sel)),
      actionButton("runButton", "Run Analysis", icon = icon("play")),
      br(),
      radioButtons("stats", "Select Grouping for Summary Table", choices = list("Group by Attrition" = "Attrition", "Group by Gender" = "Gender"), selected = "Attrition"),
      selectInput("sumVars", "Select Variables for Summary Table", choices = unique(names(tableData)), selected = "Age", multiple = FALSE, selectize = TRUE),
      h4(strong("Data Tab")),
      h5("Select column(s) and age row to filter the data set."),
      selectizeInput("Columns",
                     "Filter Columns",
                     choices = NULL,
                     multiple = TRUE),
      selectizeInput("Rows",
                     "Filter by Age",
                     choices = NULL,
                     multiple = TRUE),
      downloadButton("downloadFile", "Download File")
    ),
    mainPanel(
      # Show output and create tabs.
      tabsetPanel(type = "tab",
                  tabPanel("About",
                           # Load Image
                           img(
                             src = aboutImage,
                             height = "300px",
                             width = "875px"
                           ),
                           h3("Purpose of the App"),
                           "This app helps identify the factors that lead",                            "to employee attrition by exploring questions",                             "such as comparing monthly",
                           "income by education and attrition.",
                           h3("Data Set"),
                           "The data set is a fictional data set created by",
                           "IBM Data Scientist.  The data set presents an",
                           "employee survey from IBM, indicating if there",                            "is attrition or not.  The data set includes",                              "variables related to attrition ",
                           "and employee performance such as Age,",
                           "Department, Hourly Rate, etc.",
                           "The data set was downloaded from Kaggle",
                           a(href = hrDataLink, "here"),".",
                           h3("Tabs"),
                           tags$ul(
                             tags$li(
                               "About: Purpose of app and brief discussion",                               "of the data."),
                             tags$li(
                               "Data Exploration: Data visualizations and",                             "summaries of the data."),
                             tags$li(
                               "Modeling: Information on three supervised",                                "learning models, allowing the user to fit",                                "models, and make predictions", "based on user",                              "input."),
                             tags$li(
                               "Data: Allows the user to explore the data set",                              "and save the data file.")
                           )
                  ),
                  # Create tabs for separate pages.
                  tabPanel("Data Exploration", plotOutput("plotOne"),                                  dataTableOutput("summary")),
                  tabPanel("Modeling",
                           tabsetPanel(
                             tabPanel("Modeling Info",
                                      "This section will include the use of",                                     "classification models to predict if",                                      "an employee is likely to quit which",                                      "could be valuable to HR departments",                                      "since it would allow them the ability",                                     "to resolve the situation and prevent",                                     "attrition.",
                                      h4("Logistic Regression"),
                                      "Generalized linear models can have",                                   "continuous and categorical predictors.",
                                      "A logistic regression model is a",                                       "type of generalized model which will",                                    "be used to predict Employee Attrition.",
                                  "A logistic regression model is used to",                                     "model the probability of a certain",                                   "class or event such as success/failure.",
                                      "A logistic regression model uses a",                                      "logistic function to model a binary",                                   "response variable.",
                                  "Benefits of logistic regression models",                                 "include:  ease of implementation, easy to",                                "update, greater accuracy, and can easily",                                "extend to multiple classes.",
                              "Cons of logistic regression include: the",                                  "possibility of overfitting with high",                                     "dimensional data sets which could lead to",                                "inaccurate results on the test set,",                                     "inability to solve non-linear problems,",                               "difficulty capturing complex relationships,",                               "and tedious data preparation.",
                                 "The logistic regression model formula is:",                                uiOutput("logEq"),
                                      br(),
                                      h4("Classification Tree"),
                             "A classification tree is a type of decision",                              "tree that uses a categorical binary response",                             "variable which has two levels", "(ex. yes or",                               "no or 1 or 0).",
                              "Classification trees can be used for",                                   "classification predictive modeling problems.",                              "Classification trees use tree based methods",                             "for prediction which involves splitting up",                                 "your predictor spaces, splitting the spaces",                               "into regions, and developing predictions",                                  "based off those regions.  The most",                                     "prevalent class for a region is usually used",                             "for prediction.  Benefits of classification",                               "trees include being very easy to read, no",                                 "need to scale predictors, and statistical",                              "assumptions are not necessary.  Drawbacks of",                           "using classification trees are that they don't",                         "really perform that well for predictive purposes,",                          "small changes in the data can have an impact on",                           "the tree, and trees usually need to be pruned.",
                                      br(),
                                      br(),
                                      h4("Random Forest Models"),
                            "Random Forest models are an extension of the",                             "tree based method bagging.  The random forest",                             "algorithm creates multiple trees from",                                   "bootstrapped samples, includes a random subset",                          "of predictors in each tree, and predicts based",                            "on the average of the results from those trees.",                        "Random forests can be used for classification and",                        "regression problems.  Advantages of using Random",                           "Forest models include better accuracy than other",                         "classification algorithms, lower risk of",                                  "overfitting, and random forests models perform",                           "well on non-linear data.  Disadvantages of the",                            "random forest model include slow training, bias",                          "when dealing with categorical variables, and the",                         "loss of interpretability when compared to dealing",                          "with a single decision tree."),
                             tabPanel("Model Fitting"),
                             tabPanel("Prediction"))),
                  tabPanel("Data",                              dataTableOutput("DataSet")
                  ))) 
    
  ))
)    

# comment