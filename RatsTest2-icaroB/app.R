#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("tidyverse")

calls <- read_csv("ratsZip5.csv")

# Function to estimate number of rats in any year (%y between 11 and 21) 
capture_recapture <- function(year, zop) {
  lots_year_1 <- calls %>% filter(Year == year - 1) %>% filter(Month < 7) %>% pull(BBL)
  lots_year_2 <- calls %>% filter(Year == year) %>% filter(Month < 7) %>% pull(BBL)
  lots_year_1Zop <- calls %>% filter(Year == year - 1) %>% filter(Zop == zop) %>% pull(BBL)
  number_in_year_1 <- length(unique(lots_year_1))
  number_in_year_1Zop <- length(unique(lots_year_1Zop))
  number_in_year_2 <- length(unique(lots_year_2))
  number_in_year_1_and_2 <- sum(unique(lots_year_1) %in% unique(lots_year_2))
  50 * number_in_year_1Zop * number_in_year_2/number_in_year_1_and_2
  
}

# Define UI for application that draws graph
ui <- fluidPage(
  
  # Application title
  titlePanel("The Rats of NYC"),
  
  # Sidebar with a slider input for years
  sidebarLayout(
    sidebarPanel(
      sliderInput("years",
                  "Starting year:",
                  min = 11,
                  max = 21,
                  value = 11),
      
      sliderInput("years2",
                  "End year:",
                  min = 11,
                  max = 21,
                  value = 21),
      
      sliderInput("zopp",
                  "Zip:",
                  min = 10001,
                  max = 10050,
                  value = 10010)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)


# Define server logic required to draw graph
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    year <- input$years
    year2 <- input$years2
    Zopp <- input$zopp
    
    qplot(x = factor((2000+year):(2000+year2)),
          weight = sapply(year:year2, capture_recapture, zop = Zopp),
          geom = "bar")  +
      labs(x = "year",
           y = "estimated rat population") +
      theme_bw()
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
