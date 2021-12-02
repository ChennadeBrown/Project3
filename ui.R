library(caret)
library(shiny)
library(DT)
library(knitr)
library(dplyr)
library(caret)
library(tree)
library(tidyverse)
library(ggplot2)
library(data.table)
library(party)

suppressWarnings(library(caret))

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

# Prepare data for modeling.

# Update levels of categorical variables to numeric.
employData2 <- employData %>% mutate(Attrition = if_else(Attrition == "No", "0", "1"))
employData2 <-employData2 %>% mutate(BusinessTravel = if_else(BusinessTravel == "Non-Travel", "1", if_else(BusinessTravel == "Travel_Frequently", "2", "3")))
employData2 <- employData2 %>% mutate(Department = if_else(Department == "Sales", "3", if_else(Department == "Research & Development", "2", "1")))
employData2 <- employData2 %>% mutate(EducationField = if_else(EducationField == "Human Resources", "1", if_else(EducationField == "Life Sciences", "2", if_else(EducationField == "Marketing", "3", if_else(EducationField == "Medical", "4", if_else(EducationField == "Other", "5", "6"))))))
employData2 <- employData2 %>% mutate(Gender = if_else(Gender == "Female", "1", "0"))
employData2 <- employData2 %>% mutate(JobRole = if_else(JobRole == "Healthcare Representative", "1", if_else(JobRole == "Human Resources", "2", if_else(JobRole == "Laboratory Technician", "3", if_else(JobRole == "Manager", "4", if_else(JobRole == "Manufacturing Director", "5", if_else(JobRole == "Research Director", "6", if_else(JobRole == "Research Scientist", "7", if_else(JobRole == "Sales Executive", "8", "9")))))))))
employData2 <- employData2 %>% mutate(MaritalStatus = if_else(MaritalStatus == "Divorced", "1", if_else(MaritalStatus == "Married", "2", "3")))
employData2 <- employData2 %>% mutate(OverTime = if_else(OverTime =="No", "1", "2"))

#Remove over18 column.
employData2$Over18 <- NULL


# Store columns to be changed to numeric in an object.
i <- c(3, 5, 8, 10, 14, 16, 20)

#Change character variables to numeric for modeling.
employData2[, c(3,5,8,10,14,16,20)] <- sapply(employData2[, c(3,5,8,10,14,16,20)], as.numeric)

# Change Attrition back to a factor.
employData2$Attrition <- as.factor(employData2$Attrition)


#Reorder columns.
employData2 <- employData2[,c(2, 1, 4, 3, 5:31)]








shinyUI(fluidPage(
  titlePanel("Employee Attrition"),
  sidebarLayout(
    sidebarPanel(
      # Create widgets.
      h4(strong("Data Exploration Tab")),
      selectInput("numVarOne", "Numeric Variable One", choices=c(not_sel)),
      selectInput("numVarTwo", "Numeric Variable Two", choices = c(not_sel)),
      selectInput("factVars", "Categorical Variable", choices = c(not_sel)),
      actionButton("runButton", "Run Analysis", icon = icon("play")),
      br(),
      radioButtons("stats", "Select Grouping for Summary Table", choices = list("Group by Attrition" = "Attrition", "Group by Gender" = "Gender"), selected = "Attrition"),
      selectInput("sumVars", "Select Variables for Summary Table", choices = unique(names(tableData)[3:16]), selected = "Age", multiple = FALSE, selectize = TRUE),
      h4(strong("Modeling Tab")),
      #Widgets to allow user to choose model settings..
      numericInput("setSeed",
                   "Set Random Seed",
                   value = 50,
                   min = 1,
                   max = 100,
                   step = 1),
      numericInput("dataProps",
                   "Choose Proportion of data to include in the training set.",
                   value = 0.70,
                   min = 0.10,
                   max = 0.90,
                   step = 0.10),
      # Choose number of folds for cross validation.
      numericInput(
        "cvFolds",
        "Choose No. of Folds",
        value = 5,
        min = 2,
        max = 8,
        step = 1
      ),
      h5("Choose Logistic Regression Model Parameters:"),
      #Select variables to use in the model.
      selectInput( "logVars",
                   "Choose Predictors:",
                   choices = colnames(employData2)[2:31],
                   selected = c("Age", "DailyRate", "Gender"),
                   multiple = TRUE,
                   selectize = TRUE),
      h5("Choose Classification Tree Parameters:"),
      selectInput(
        "classVars",
        "Choose Predictors:",
        choices = colnames(employData2)[2:31],
        selected = c("Age", "DailyRate", "Gender", "DistanceFromHome", "PercentSalaryHike", "HourlyRate", "JobInvolvement"),
        multiple = TRUE,
        selectize = TRUE),
      h5("Choose Random Forest Parameters"),
      selectInput( "forestVars",
                   "Choose Predictors",
                   choices = colnames(employData2)[2:31],
                   selected = c("Age", "DailyRate", "Gender"),
                   multiple = TRUE,
                   selectize = TRUE),
      # Choose tuning parameters.
      #selectizeInput("selMtry",
      #"Select maximum of 10 values for mtry",
      #choices = 1:length(colnames(employData2)[2:31]), 
      #multiple = TRUE,
      #selected = c(3, 6, 9),
      #options = list(maxItems = 10)),
      actionButton("modelFit", 
                   "Run Models"),
      h4(strong("Prediction Tab")),
      radioButtons("modelSelection",
                   "Select Model for Prediction",
                   choices = c("None Selected" = "", "Logistic Regression")),
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
                           "This app helps identify the factors that lead to employee attrition by exploring questions such as comparing monthly income by education and attrition.",
                           h3("Data Set"),
                           "The data set is a fictional data set created by IBM Data Scientist.  The data set presents an employee survey from IBM indicating if there is attrition or not.",
                           "The data set includes variables related to attrition and employee performance such as Age,Department, Hourly Rate, etc.",
                           "The data set was downloaded from Kaggle", a(href = hrDataLink, "here"),".",
                           h3("Tabs"),
                           tags$ul(
                             tags$li(
                               "About: Purpose of app and brief discussion of the data."),
                             tags$li(
                               "Data Exploration: Data visualizations and summaries of the data."),
                             tags$li(
                               "Modeling: Information on three supervised learning models, allowing the user to fit models, and make predictions", "based on user input."),
                             tags$li(
                               "Data: Allows the user to explore the data set and save the data file.")
                           )
                  ),
                  # Create tabs for separate pages.
                  tabPanel("Data Exploration", "Select three variables for scatter plots and one categorical variable for box plots and push play to produce plots.", plotOutput("plotOne"),                                  dataTableOutput("summary")),
                  tabPanel("Modeling",
                           tabsetPanel(
                             tabPanel("Modeling Info",
                                      "This section will include the use of classification models to predict if an employee is likely to quit which could be valuable to HR departments since it would allow them the ability to resolve the situation and prevent attrition.",
                                      h4("Logistic Regression"),
                                      "Generalized linear models can have continuous and categorical predictors.",
                                      "A logistic regression model is a type of generalized model which will",    "be used to predict Employee Attrition.",
                                      "A logistic regression model is used to model the probability of a certain","class or event such as success/failure.",
                                      "A logistic regression model uses a logistic function to model a binary",   "response variable.",
                                      "Benefits of logistic regression models include:ease of implementation, easy to update, greater accuracy, and can easily extend to multiple classes.",
                                      "Cons of logistic regression include: the possibility of overfitting with high dimensional data sets which could lead to inaccurate results on the test set, inability to solve non-linear problems, difficulty capturing complex relationships,and tedious data preparation.",
                                      "The logistic regression model formula is:",
                                      uiOutput("logEq"),
                                      br(),
                                      h4("Classification Tree"),
                                      "A classification tree is a type of decision tree that uses a categorical", "binary response variable which has two levels, (ex. yes or,no or 1 or 0).",
                                      "Classification trees can be used for classification predictive modeling", "problems.",
                                      "Classification trees use tree based methods for prediction which involves", "splitting up your predictor spaces, splitting the spaces into regions, and", "developing predictions based off those regions.",
                                      "The most prevalent", "class for a region is usually used for prediction.", "Benefits of classification trees include being very easy to read, no",     "need to scale predictors, and statistical assumptions are not necessary.", "Drawbacks of using classification trees are that they don't really perform", "that well for predictive purposes, small changes in the data can have an", "impact on the tree, and trees usually need to be pruned.",
                                      br(),
                                      br(),
                                      h4("Random Forest Models"),
                                      "Random Forest models are an extension of the tree based method bagging.",  "The random forest algorithm creates multiple trees from bootstrapped", "samples, includes a random subset of predictors in each tree, and predicts", "based on the average of the results from those trees.",                  "Random forests can be used for classification and regression problems.",  "Advantages of using Random Forest models include better accuracy than", "other classification algorithms, lower risk of overfitting, and random", "forests models perform well on non-linear data.",
                                      "Disadvantages of the random forest model include slow training, bias",     "when dealing with categorical variables, and the loss of interpretability", "when compared to dealing with a single decision tree."),
                             tabPanel("Model Fitting",
                                      #Report accuracy and summaries on the training data
                                      h4("Logistic Regression Fit Statistics on Training Data"),
                                      dataTableOutput("accuracy"),
                                      br(),
                                      #Accuracy on the tree model.
                                      h4("Classification Tree Fit Statistics on Training Data"),
                                      dataTableOutput("treeAccuracy"),
                                      br(),
                                      h4("Random Forest Fit Statistics on Training Data"),
                                      dataTableOutput("RanForAcc"),
                                      # Output the tree plot.
                                      h4("Tree Plot"),
                                      plotOutput("treePlot"),
                                      br(),
                                      h4("Random Forest Variable of Importance"),
                                      plotOutput("RanForVarImp"),
                                      br(),
                                      h4("Model Fit Statistics on the Test Data"),
                                      dataTableOutput("testFitStatz")
                             ),
                             tabPanel("Prediction"))),
                  tabPanel("Data",                              dataTableOutput("DataSet")
                  ))) 
    
  ))
  
)