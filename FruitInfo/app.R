# Library some packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(httr)
library(jsonlite)
library(tidyverse)
library(ggplot2)

# Create function to query api
get_api <- function(fruit = "all", nutrition = "all") {
  
  #Create the url to use httr::GET
  base_url <- "fruityvice.com/api/fruit/"
  full_url <- paste0(base_url, fruit)
  result <- httr::GET(full_url)
  parsed <- as_tibble(fromJSON(rawToChar(result$content)))
  
  #Define which nutritional data to return
  switch(nutrition,
         "all" = nut <- as_tibble(parsed$nutritions),
         "calories" = nut <- as_tibble(parsed$nutritions$calories),
         "fat" = nut <- as_tibble(parsed$nutritions$fat),
         "sugar" = nut <- as_tibble(parsed$nutritions$sugar),
         "carbohydrates" = nut <- as_tibble(parsed$nutritions$carbohydrates),
         "protein" = nut <- as_tibble(parsed$nutritions$protein))  
  
  #Define to retrieve data of all fruits or specific fruits
  if (fruit == "all") {
    nut |> 
      mutate(Name = parsed$name) |>
      mutate(Family = parsed$family) |>
      mutate(Genus = parsed$genus) |>
      mutate(Order = parsed$order) |>
      select(Name, Family, Genus, Order, everything())
  } else {
    nut |> 
      mutate(Name = parsed$name[1]) |>
      mutate(Family = parsed$family[1]) |>
      mutate(Genus = parsed$genus[1]) |>
      mutate(Order = parsed$order[1]) |>
      select(Name, Family, Genus, Order, everything())
  }
}
dat <- get_api()

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
                                                   choices = c("Name", "Family", "Genus", "Order"),
                                                   selected = "Name"),
                                       selectInput(inputId = "var2",
                                                   label = "Choose the second:",
                                                   choices = c("Name", "Family", "Genus", "Order"),
                                                   selected = "Family"),
                                       tableOutput("conttable")
                                       ),
                              
                              #Create numerical summaries tab
                              tabPanel("Numerical",
                                       p("For numerical values, data can be summarized across different measures. In this case, the measure of center can be calculated through the mean or median, and the measure of spread can be calculated through the standard deviation or interquartile range (IQR) for the different family groups."),
                                       selectInput(inputId = "groupby",
                                                   label = "Choose the group:",
                                                   choices = c("Name", "Family", "Genus", "Order")),
                                       radioButtons(inputId = "summ", 
                                                    label = "Choose the type of summary:",
                                                    choices = c("Mean", "Median", "Standard deviation", "IQR")),
                                       tableOutput("numsum")
                                       ),
                              
                              #Create plots tab
                              tabPanel("Graphical",
                                       tabsetPanel(
                                         
                                         #Create bar graph
                                         tabPanel("Bar Graph",
                                                  p("Bar graphs is a visual representation of the categorical data. Pick one of the categorical groups from the data set to see a bar graph showing the count of the data in that group."),
                                                  selectInput(inputId = "bargroup",
                                                              label = "Choose the category to display in a bar graph:",
                                                              choices = c("Name", "Family", "Genus", "Order")),
                                                  plotOutput("bar")),
                                         
                                         #Create Pie Chart
                                         tabPanel("Pie Chart",
                                                  p("A pie chart is similar to bar graphs as it visually shows the count of the categorical data. The pie chart gives a reletive count of the selected group."),
                                                  selectInput(inputId = "piegroup",
                                                              label = "Choose the category display in a pie chart:",
                                                              choices = c("Name", "Family", "Genus", "Order")),
                                                  plotOutput("pie")),
                                         
                                         #Create Histogram
                                         tabPanel("Histogram",
                                                  p("Histograms deal with the numerical data, grouping them into bins. For this histogram, choose one of the numerical categories to make a histogram of the values."),
                                                  selectInput(inputId = "histgroup",
                                                              label = "Choose the category to display in a histogram:",
                                                              choices = c("calories", "fat", "sugar", "carbohydrates", "protein")),
                                                  plotOutput("hist")),
                                         
                                         #Create Scatter Plot
                                         tabPanel("Scatter Plot",
                                                  p("Scatter plots are used to show the relationship between two numerical variables. For this plot, the data points are also color coded by a third variable. Pick three of the nutritional value data to show the relationship between these categories."),
                                                  selectInput(inputId = "scatterx",
                                                              label = "Choose the first variable:",
                                                              choices = c("calories", "fat", "sugar", "carbohydrates", "protein"),
                                                              selected = "calories"),
                                                  selectInput(inputId = "scattery",
                                                              label = "Choose the second variable:",
                                                              choices = c("calories", "fat", "sugar", "carbohydrates", "protein"),
                                                              selected = "sugar"),
                                                  selectInput(inputId = "scattergroup",
                                                              label = "Choose the second:",
                                                              choices = c("calories", "fat", "sugar", "carbohydrates", "protein"),
                                                              selected = "protein"),
                                                  plotOutput("scatter"))
                                         
                                         
                                       ))
                      )))))))
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
    dat |> group_by_(input$var1, input$var2) |> summarise(count = n())
  )
  
  #Numerical summary
  output$numsum <- renderTable(
    if (input$summ == "Mean") {
      dat |> group_by_(input$groupby) |> summarize(across(where(is.numeric), 
                         list("mean" = ~ round(mean(.x, na.rm = TRUE), 2)),
                         .names = "{.fn}_{.col}"))
    } else if (input$summ == "Median") {
      dat |> group_by_(input$groupby) |> summarize(across(where(is.numeric), 
                              list("median" = ~ round(median(.x, na.rm = TRUE), 2)),
                              .names = "{.fn}_{.col}"))
    } else if (input$summ == "Standard deviation") {
      dat |> group_by_(input$groupby) |> summarize(across(where(is.numeric), 
                              list("sd" = ~ round(sd(.x, na.rm = TRUE), 2), 
                              .names = "sd_{.col}")) |> drop_na()
      )
    } else if (input$summ == "IQR") {
      dat |> group_by_(input$groupby) |> summarize(across(where(is.numeric), 
                              list("IQR" = ~ round(IQR(.x, na.rm = TRUE), 2)), 
                              .names = "{.fn}_{.col}"))
    }
  )
  
  #Bar graph
  barplot_dat <- reactive(
    dat |> 
      group_by_(input$bargroup) |>
      summarise(count = n()) |>
      arrange(desc(count)) |>
      head(10)
  )
  
  output$bar <- renderPlot(
    ggplot(barplot_dat(), 
           aes(x = input$bargroup, y = count, fill = input$bargroup)) +
      geom_col() +
      scale_fill_viridis_d() +
      labs(y = "Count", title = "Bar Plot of Top 10 Fruits") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
  
  #Pie chart
  pie_dat <- reactive(
    dat |>
      group_by_(input$piegroup) |>
      summarise(count = n()))
  
  output$pie <- renderPlot(
    ggplot(pie_dat(), aes(x = "", y = count, fill = input$piegroup)) +
      geom_col(color = "black") +
      coord_polar(theta = "y") +
      labs(x = "", y = "", title = "Pie Chart of Fruit Groups")
  )
  
  #Histogram
  output$hist <- renderPlot(
    ggplot(dat, aes_string(x = input$histgroup, color = input$histgroup)) +
    geom_histogram(binwidth = 10, fill = "red", color = "black") +
    labs(x = input$histgroup, y = "Count", title = "Histogram of Fruit Nutrition")
  )
  
  #Scatter plot
  scatter_dat <- reactive(dat |>
    mutate(group = if_else(input$scattergroup > 5, "High", 
                           if_else(input$scattergroup < 1, "Low", "Medium"))))
  
  output$scatter <- renderPlot(
    ggplot(scatter_dat(), aes(x = input$scatterx, y = input$scattery, color = group)) +
      geom_point() +
      geom_text(aes(label = Name, vjust = 1)) +
      labs(x = input$scatterx, y = input$scattery, title = "Scatter plot Comparing Nutritional Values of Fruits") +
      scale_color_discrete(name = "Levels")
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
