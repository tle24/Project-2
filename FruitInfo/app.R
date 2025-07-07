library(shiny)
library(shinydashboard)

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
   dashboardBody(
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
                             "Jackfruit", "Horned Melon", "Hazelnut", "Pomelo",
                             "Mangosteen", "Pumpkin", "Japanese Persimmon", 
                             "Papaya", "Anona", "Ceylon Gooseberry")
                             ),
                 selectInput(inputId = "nutrition",
                             label = "Choose nutritional data to display:",
                             choices = c("all", "calories", "fat", "sugar", 
                             "carbohydrates", "protein")),
                 tableOutput("data")
               )),
       
       #Creating the data exploration page
       tabItem(tabName = "explore",
               titlePanel("Data Exploration"),
               mainPanel(
                 
               ))
     )
   )
)

# Define server logic
server <- function(input, output) {
  output$data <- renderTable(
    get_api(fruit = input$fruit, nutrition = input$nutrition)
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
