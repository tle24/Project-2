library(shiny)
library(shinydashboard)
library(dplyr)

# Define UI for application
ui <- dashboardPage(

    # Application title
   dashboardHeader(title = "Fruit Information"),
   
    
    #Create sidebar tabs
    dashboardSidebar(
      sidebarMenu(
        menuItem("About Page", tabName = "about"),
        menuItem("Data Download", tabName = "download"),
        menuItem("Data Exploration", tabName = "explore")
      )
    ),
   
   #Create tab body
   dashboardBody(fluidPage(
     tabItems(
       
       #Creating the about page
       tabItem(tabName = "about", 
               titlePanel("About Page"),
               mainPanel(
                 p("This app can be used to retrieve and explore information on different fruits. The information comes from a free API resource. "),
                 HTML("<p>All of the data on the fruit available here is queried from the API <a href = https://www.fruityvice.com/> Fruityvice</a>. The information available on the 49 fruits are the common name, scientific name (family, genus, order), and the nutritional values (calories, fat, sugar, carbohydrates, protien.</p>"),
                 p("There are different pages in this app to help with retrieving specific data and to explore that data. The Data Download page allows you to specifiy what data you want to look at. This can be which fruits you want, the family, genus, or order of the fruits, or which nutritional value to retrieve. The Data Exploration page allows you to create summaries and graphs of the data that you want to look at."),
                 img(src = "https://www.fruityvice.com/images/cherry.png", alt = "Image of cherries from the fruityvice.com website", height = "75%", width = "75%", align = "left")
                 )), 
       
       #Creating the data download page
       tabItem(tabName = "download",
               titlePanel("Data Download"),
               mainPanel(
                 selectInput(inputId = "fruit", 
                             label = "Choose your fruit:", 
                             choices = c("all",
                             "Persimmon", "Strawberry", "Banana", "Tomato", 
                             "Pear", "Durian", "Blackberry", "Lingonberry",
                             "Kiwi", "Lychee", "Pineapple", "Fig", "Gooseberry",
                             "Passionfruit", "Plum", "Orange", "GreenApple", 
                             "Raspberry", "Watermelon", "Lemon", "Mango", 
                             "Blueberry", "Apple", "Guava", "Apricot", "Melon", 
                             "Tangerine", "Pitahaya", "Lime", "Pomegranate",
                             "Dragonfruit", "Grape", "Morus", "Feijoa", "Avocado",
                             "Kiwifruit", "Cranberry", "Cherry", "Peach", 
                             "Jackfruit", "Hazelnut", "Pomelo", "Mangosteen", 
                             "Pumpkin", "Papaya", "Anona")
                             ),
                 selectInput(inputId = "nutrition",
                             label = "Choose nutritional data to display:",
                             choices = c("all", "calories", "fat", "sugar", 
                             "carbohydrates", "protein")),
                 downloadButton(outputId = "downloadfile",
                                label = "Save file"),
                 tableOutput("data")
               )),
       
       #Creating the data exploration page
       tabItem(tabName = "explore",
               titlePanel("Data Exploration"),
               mainPanel(
                 fluidRow(
                   navbarPage("Summaries",
                              
                              #Create contingency table tab
                              tabPanel("Categorical",
                                       p("Contingency tables can be created to represent the categorical data showing the count of data in the chosen group."),
                                       selectInput(inputId = "var1", 
                                                   label = "Choose the first variable:",
                                                   choices = c("Name", "Family", "Genus", "Order")),
                                       selectInput(inputId = "var2",
                                                   label = "Choose the second variable:",
                                                   choices = c("Name", "Family", "Genus", "Order")),
                                       tableOutput("conttable")
                                       ),
                              
                              #Create numerical summaries tab
                              tabPanel("Numerical",
                                       p("For numerical values, data can be summarized across different measures. In this case, the measure of center can be calculated through the mean or median, and the measure of spread can be calculated through the standard deviation or interquartile range (IQR) for the different family groups."),
                                       radioButtons(inputId = "summ", 
                                                    label = "Choose the type of summary:",
                                                    choices = c("Mean", "Median", "Standard deviation", "IQR")),
                                       tableOutput("numsum")
                                       ),
                              
                              #Create plots tab
                              tabPanel("Graphical",
                                       tabsetPanel(
                                         
                                         #Create bar graph
                                         tabPanel("Bar Graph"),
                                         
                                         #Create Pie Chart
                                         tabPanel("Pie Chart"),
                                         
                                         #Create Histogram
                                         tabPanel("Histogram"),
                                         
                                         #Create Scatter Plot
                                         tabPanel("Scatter Plot")
                                       )))
                 )
               ))
     )))
   )

# Define server logic
server <- function(input, output, session) {
  
  #Retrieve data
  output$data <- renderTable(
    get_api(fruit = input$fruit, nutrition = input$nutrition)
  )
  
  #Download data
  output$downloadfile <- downloadHandler(
    filename = function() {
      paste0(input$fruit, "-nutri-", input$nutrition, Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filename, file, row.names = FALSE)
    }
  )
  
  #Contingency table
  output$conttable <- renderTable(
    table(dat[[input$var1]], dat[[input$var2]])
  )
  
  #Numerical summary
  output$numsum <- renderTable(
    if (input$summ == "Mean") {
      dat |> group_by(Family) |> summarize(across(where(is.numeric), 
                         list("mean" = ~ round(mean(.x, na.rm = TRUE), 2)),
                         .names = "{.fn}_{.col}"))
    } else if (input$summ == "Median") {
      dat |> group_by(Family) |> summarize(across(where(is.numeric), 
                              list("median" = ~ round(median(.x, na.rm = TRUE), 2)),
                              .names = "{.fn}_{.col}"))
    } else if (input$summ == "Standard deviation") {
      dat |> group_by(Family) |> summarize(across(where(is.numeric), 
                              list("sd" = ~ round(sd(.x, na.rm = TRUE), 2), 
                              .names = "sd_{.col}")) |> drop_na()
      )
    } else if (input$summ == "IQR") {
      dat |> group_by(Family) |> summarize(across(where(is.numeric), 
                              list("IQR" = ~ round(IQR(.x, na.rm = TRUE), 2)), 
                              .names = "{.fn}_{.col}"))
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
