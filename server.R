library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(knitr)
library(dplyr)
library(caret)
library(tree)
library(ggplot2)
library(party)

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

# Scatterplot & Barplot functions.
draw_plotOne <- function(getData, numVarOne, numVarTwo, factVars){
  if(numVarOne != not_sel &
     numVarTwo != not_sel &
     factVars != not_sel){
    ggplot(data = getData,
           aes_string(x = numVarOne, y = numVarTwo,
                      color = factVars)) +
      geom_point() + ggtitle("Scatterplot")}
  else if(numVarOne == not_sel & 
          numVarTwo == not_sel & 
          factVars != not_sel){
    ggplot(data = getData,
           aes_string(x = factVars)) +
      geom_bar() + ggtitle("Distribution of Variables")
  }}



shinyServer(function(input, output, session) {
  
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
  
  # Render plots.
  output$plotOne <- renderPlot(plotOne())
  
  
  
  
  
  # Create table
  # Get numeric variables and group variable (attrition) for table select input box.
  tableData <- employData %>% select(Attrition,Gender, Age, DailyRate, DistanceFromHome, HourlyRate, MonthlyIncome, MonthlyRate, NumCompaniesWorked, PercentSalaryHike, TotalWorkingYears, YearsWithCurrManager, TrainingTimesLastYear, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion)
  as.data.frame(tableData)
  
  
  
  # Subset table values by attrition.
  summary <- reactive({
    if(input$stats == "Attrition"){
      req(input$sumVars)
      tableData %>%
        group_by(Attrition) %>% 
        summarise(mean = round(mean(!!sym(input$sumVars)),1), `st. dev` = 
                    round(sd(!!sym(input$sumVars)),1), min = min(!!sym(input$sumVars)), max = max(!!sym(input$sumVars)))} 
    else if(input$stats == "Gender"){
      req(input$sumVars)
      tableData %>% 
        group_by(Gender) %>%
        summarise(mean = round(mean(!!sym(input$sumVars)),1), `st. dev` = 
                    round(sd(!!sym(input$sumVars)),1), min = min(!!sym(input$sumVars)), max = max(!!sym(input$sumVars)))
    }
  })
  # Print table.
  output$summary <- renderDataTable({(summary())})
  
  
  #Prepare data for data table.
  
  # Select selected columns & rows.
  updateSelectizeInput(session, "Columns",
                       server = TRUE,
                       choices = colnames(employData),
                       selected = colnames(employData[1:5]))
  
  updateSelectizeInput(session, "Rows",
                       server = TRUE,
                       choices = unique(employData$Age),
                       selected = 49)
  
  # Output data table.
  output$DataSet <- renderDataTable({
    
    # Filter the data set.
    employData %>% 
      select(input$Columns) %>%
      filter(employData == input$Rows)
  })
  
  # Allow user to download the filtered dataset.
  output$downloadFile <- downloadHandler(
    filename = function(){
      paste("data.csv")},
    content = function(con){
      write.csv(employData %>% 
                  select(input$Columns) %>%
                  filter(employData == input$Rows),
                con,
                row.names = FALSE)}
  ) 
  
  # Info for modeling Page.
  # Mathematical formula for Logistic Regression.
  
  output$logEq <- renderUI({
    withMathJax(
      helpText(
        "$$\\log(\\frac{p}{\\1-p} = \\beta_0 + \\beta_1(var)}$$"))
  })
  
  
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
  employData2
  
  # Store columns to be changed to numeric in an object.
  i <- c(3, 5, 8, 10, 14, 16, 20)
  
  #Change character variables to numeric for modeling.
  employData2[, c(3,5,8,10,14,16,20)] <- sapply(employData2[, c(3,5,8,10,14,16,20)], as.numeric)
  
  # Change Attrition back to a factor.
  employData2$Attrition <- as.factor(employData2$Attrition)
  
  
  
  #Reorder columns.
  employData2 <- employData2[,c(2, 1, 4, 3, 5:31)]
  
  
  
  #Fit models on the training data.
  
  observeEvent(input$modelFit, {
    
    # Create a Progress object
    progress <- Progress$new()
    
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    #Display message to user while models are running.
    progress$set(message = "Models Running", value = 0)
    
    
    #Store variables for each model in objects.
    logVars <- input$logVars
    classVars <- input$classVars
    forestVars <- input$forestVars
    
    #Store model parameters in objects.
    setSeed <- input$setSeed
    dataProps <- input$dataProps
    cvFolds <- input$cvFolds
    
    #Store random forest parameter mtry in an object.
    selMtry <- input$selMtry
    
    
    #Use inputs to set the random seed.
    set.seed(setSeed)
    
    #Store train/testing indexes in an object to split data into training and test sets.
    trainSetInd <- sample(seq_len(nrow(employData2)),
                          size=floor(nrow(employData2)*dataProps))
    
    #Split data into a training and test set based on user input.
    employTrain <- employData2[trainSetInd,]
    employTest <- employData2[-trainSetInd,]
    
    #Ignore warnings from caret during cross validation.
    suppressWarnings(library(caret))
    
    
    # Store cross validation parameters in an object.
    trCtrl <- trainControl(
      method = "cv",
      number = cvFolds
    )
    
    # Run logistic regression model.
    
    
    # Increment the progress bar, and update the detail text.
    progress$inc(0.3, detail = "Logistic Regression Model")
    
    logisticModel <- train(Attrition ~ .,
                           data = employTrain[, c(c("Attrition"),logVars)],
                           method = "glm",
                           family = "binomial",
                           metric = "Accuracy",
                           trControl = trCtrl)
    
    
    # Increment the progress bar, and update the detail text.
    progress$inc(0.5, detail = "Tree Model")
    
    
    # Run Classification Tree Model.
    treeModel <- train(Attrition ~ .,
                       data = employTrain[, c(c("Attrition"), classVars)],
                       method = "ctree2",
                       metric = "Accuracy",
                       trControl = trCtrl,
                       tuneGrid = expand.grid(maxdepth = 3, mincriterion = 0.95))
    
    # Increment the progress bar, and update the detail text.
    progress$inc(0.7, detail = "Random Forest Model")
    
    #Run Random Forest Model.  Removed cross validation as it is causing a warning message.
    rfFit <- train(Attrition ~ .,
                   data = employTrain[,c(c("Attrition"), forestVars)],
                   method = "rf",
                   metric = "Accuracy")
    #tuneGrid = expand.grid(mtry = selMtry))
    #trControl = trCtrl)
    
    
    # Increment the progress bar, and update the detail text.
    progress$inc(1, detail = "Review model performance on the training set")
    
    #Logistic Regression Accuracy. 
    output$accuracy <- renderDataTable({
      
      fitStats <- (t(as.matrix(logisticModel$results)))
      fitStats
      
      colnames(fitStats) <- c("Logistic Regression")
      fitStats
    })
    
    #Logistic Summary (doesn't work)  
    #output$logisticSum <- renderPrint({
    #attFit <- glm(Attrition ~ logVars, 
    #data = employTrain[2:31], 
    #family = "binomial")
    #})
    
    #This doesn't work either.  
    #output$logisticSum <- renderPrint({
    #summary(logisticModel)
    #})
    
    #Classification Tree Accuracy.
    output$treeAccuracy <- renderDataTable({
      
      treeAcc <- print(treeModel)
      
      printTreeSummary <- as.data.frame(treeAcc)
      printTreeSummary
    })  
    
    
    #Output tree plot.
    output$treePlot <- renderPlot({
      treeFrame <- plot(treeModel$finalModel)
      treeFrame
      
      
    })
    
    
    
  })
  
})























