Project 3 - Creating a Shiny App
================

## Purpose of the App

This app explores the IBM HR Analytics Employee Attrition & Performance
data set. The data set uncovers the factors that lead to employee
attrition by comparing factors such as average monthly income by
education and attrition or job satisfaction by job role and attrition.

## Packages Required

-   `caret`: model training
-   `shiny` : build interactive web app in R
-   `DT`: present data frames as HTML tables
-   `knitr` : enables integration of R code into LaTex, HTML, and
    Markdown documents
-   `dplyr` : manipulate data in R
-   `tidyverse` : manipulate and reshape data in R
-   `ggplot2` : create plots
-   `party` : conditional inference tree

## To install the packages run the following code:

``` r
install.packages("caret")
install.packages("shiny")
install.packages("DT")
install.packages("knitr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("party")
```

## Use the following code to run the app.

``` r
shiny::runGitHub("Project3", "ChennadeBrown", ref = "main")
```
