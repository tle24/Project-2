# Project-2

This Fruit Information app looks at the scientific names and nutritional values of different fruits. It will allow for data to be retrieved on specified fruits and allows for summaries to be made on this data. Contingency tables, numerical summaries, and plots can be constructed through specifying what variables to look at. 

## Packages needed to run the app:
- httr
- jsonlite
- tidyverse
- ggplot2
- shiny
- shinydashboard
- dplyr

## Run all packages required:

lapply(c("httr", "jsonlite", "tidyverse", "ggplot2", "shiny", "shinydashboard", "dplyr", "tidyr"), library, character.only = TRUE)

## Code to run app:

shiny::runGitHub("Project-2", "tle24", subdir = "FruitInfo/app.R")