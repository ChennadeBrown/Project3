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



# Get data.
dataSetAttr <- "./HR-Employee-Attrition.csv"
dataSetAttr

employData <- read_csv(dataSetAttr)
employData

# Clean Data.
# Remove non predictor variables from the data set.
employData$EmployeeCount <- NULL
employData$EmployeeNumber <- NULL
employData$StandardHours <- NULL

#Create variable for input boxes.
not_sel <- "Not Selected"

# Data for summary tables.
# Get numeric variables and group variable (attrition) for table select input box.
tableData <- employData %>% select(Attrition,Gender, Age, DailyRate, DistanceFromHome, HourlyRate, MonthlyIncome, MonthlyRate, NumCompaniesWorked, PercentSalaryHike, TotalWorkingYears, YearsWithCurrManager, TrainingTimesLastYear, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion)
as.data.frame(tableData)


# Image for the about tab.
aboutImage <- "happyofficeworker.png"


# Save link to the data.
hrDataLink <- "https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset"



# Prepare data for modeling.

# Update levels of categorical variables to numeric.
employData2 <- employData %>% mutate(Attrition = if_else(Attrition == "No", "0", "1"))
employData2 <-employData2 %>% mutate(BusinessTravel = if_else(BusinessTravel == "Non-Travel", "1", if_else(BusinessTravel == "Travel_Frequently", "2", "3")))
employData2 <- employData2 %>% mutate(Department = if_else(Department == "Sales", "3", if_else(Department == "Research & Development", "2", "1")))
employData2 <- employData2 %>% mutate(EducationField = if_else(EducationField == "Human Resources", "1", if_else(EducationField == "Life Sciences", "2", if_else(EducationField == "Marketing", "3", if_else(EducationField == "Medical", "4", if_else(EducationField == "Other", "5", "6"))))))
employData2 <- employData2 %>% mutate(Gender = if_else(Gender == "Female", "1", "2"))
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



#Prepare data for predicting for prediction tab.
predData <- employData

#Change Categorical predictors to factors for modeling.
predData$BusinessTravel <- as.factor(predData$BusinessTravel)
predData$Department <- as.factor(predData$Department)
predData$EducationField <- as.factor(predData$EducationField)
predData$Gender <- as.factor(predData$Gender)
predData$JobRole <- as.factor(predData$JobRole)
predData$MaritalStatus <- as.factor(predData$MaritalStatus)
predData$OverTime <- as.factor(predData$OverTime)

#Change Attrition to a binary variable.
predData <- predData %>% mutate(Attrition = ifelse(Attrition == "No", "0", "1"))

#Change Attrition to a factor for prediction.
predData$Attrition <- as.factor(predData$Attrition)


#Remove over 18 column.
predData$Over18 <- NULL





shinyUI(fluidPage(
  titlePanel("Employee Attrition"),
  withMathJax(),
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
      numericInput("dataPart",
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
      #Select variables to use in the model.
      selectInput( "logVars",
                   "Choose Predictors for Logistic Regression Model:",
                   choices = colnames(employData2)[2:31],
                   selected = c("Age", "DailyRate", "Gender", "Department"),
                   multiple = TRUE,
                   selectize = TRUE),
      selectInput(
        "classVars",
        "Choose Predictors for Classification Tree:",
        choices = colnames(employData2)[2:31],
        selected = c("Age", "DailyRate", "Gender", "Department"),
        multiple = TRUE,
        selectize = TRUE),
      selectInput( "forestVars",
                   "Choose Predictors for Random Forest Model:",
                   choices = colnames(employData2)[2:31],
                   selected = c("Age", "DailyRate", "Gender", "Department"),
                   multiple = TRUE,
                   selectize = TRUE),
      actionButton("modelFit", 
                   "Run Models"),
      h4(strong("Prediction Tab")),
      selectInput("marValues",
                  "Choose Values",
                  choices = unique(predData$MaritalStatus),
                  multiple = TRUE,
                  selected = "Single"),
      selectInput("departValues",
                  "Choose Values:",
                  choices = unique(predData$Department),
                  multiple = TRUE,
                  selected = "Sales"),
      selectInput("busValues",
                  "Choose Values:",
                  choices = unique(predData$BusinessTravel),
                  multiple = TRUE,
                  selected = "Non-Travel"),
      actionButton("predict",
                   "Prediction"),
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
                           "The data set includes variables related to attrition and employee performance such as Age, Department, Hourly Rate, etc.",
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
                                      br(),
                                      "This section will include the use of classification models to predict if an employee is likely to quit which could be valuable to HR departments since it would allow them the ability to resolve the situation and prevent attrition.",
                                      h4("Logistic Regression"),
                                      "Generalized linear models can have continuous and categorical predictors.",
                                      "A logistic regression model is a type of generalized model.  A logistic", "regression model will be used to predict Employee Attrition.",
                                      "A logistic regression model is used to model the probability of a certain","class or event such as success/failure.",
                                      "A logistic regression model uses a logistic function to model a binary",   "response variable.",
                                      "Benefits of logistic regression models include:ease of implementation, easy to update, greater accuracy, and the ability to easily extend to multiple classes.",
                                      "Cons of logistic regression include: the possibility of overfitting with high dimensional data sets which could lead to inaccurate results on the test set, inability to solve non-linear problems, difficulty capturing complex relationships,and tedious data preparation.",
                                      "The logistic regression model formula is:",
                                      uiOutput("logEq"),
                                      br(),
                                      h4("Classification Tree"),
                                      "A classification tree is a type of decision tree that uses a categorical", "binary response variable which has two levels (ex. yes or no or 1 or 0).",
                                      "Classification trees can be used for classification predictive modeling", "problems.",
                                      "Classification trees use tree based methods for prediction which involves", "splitting up your predictor spaces, splitting the spaces into regions, and", "developing predictions based off those regions.",
                                      "The most prevalent", "class for a region is usually used for prediction.", "Benefits of classification trees include being very easy to read, no",     "need to scale predictors, and no need for statistical assumptions.", "Drawbacks of using classification trees are that they don't really perform", "that well for predictive purposes, small changes in the data can have an", "impact on the tree, and trees usually need to be pruned.",
                                      br(),
                                      br(),
                                      h4("Random Forest Models"),
                                      "Random Forest models are an extension of the tree based method bagging.",  "The random forest algorithm creates multiple trees from bootstrapped", "samples, includes a random subset of predictors in each tree, and predicts", "based on the average of the results from those trees.",                  "Random forests can be used for classification and regression problems.",  "Advantages of using Random Forest models include better accuracy than", "other classification algorithms, lower risk of overfitting, and random", "forests models perform well on non-linear data.",
                                      "Disadvantages of the random forest model include slow training, bias",     "when dealing with categorical variables, and the loss of interpretability", "when compared to dealing with a single decision tree."),
                             tabPanel("Model Fitting",
                                      #Report accuracy and summaries on the training data
                                      h5("Choose model settings and variables for each model by accessing the modeling tab section on the sidebar panel."),
                                      verbatimTextOutput("logisticSums"),
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
                             tabPanel("Prediction",
                                      h5("Predict the probability of Attrition for given values of Marital Status, Business Travel, & Department."),
                                      h5("Choose values by accessing the prediction section of the sidebar panel then push the prediction button."),
                                      verbatimTextOutput("predOutput")))),
                  tabPanel("Data",                              dataTableOutput("DataSet")
                  ))) 
    
  ))
  
)