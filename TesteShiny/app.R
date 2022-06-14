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

calls <- read_csv("nyc_rodent_compliants.csv")

# select "Rat Sightings" with year and BBL (lot) not missing
df <-
  calls %>%
  mutate(Date = as.Date(`Created Date`, format = "%m/%d/%Y"),
         Year = format(Date, "%y")) %>%
  filter(Descriptor == "Rat Sighting",
         Year != 22) %>%
  select(Year, BBL) %>%
  na.omit() %>%
  group_by(BBL, Year) %>%
  summarize(count = n()) %>%
  filter(BBL != 0)

# Function to estimate number of rats in any year (%y between 11 and 21) 
capture_recapture <- function(year) {
  lots_year_1 <- df %>% filter(Year == year - 1) %>% pull(BBL)
  lots_year_2 <- df %>% filter(Year == year) %>% pull(BBL)
  number_in_year_1 <- length(lots_year_1)
  number_in_year_2 <- length(lots_year_2)
  number_in_year_1_and_2 <- sum(lots_year_1 %in% lots_year_2)
  50 * number_in_year_1 * number_in_year_2 / number_in_year_1_and_2
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
                        value = 21)
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
      
      qplot(x = factor((2000+year):(2000+year2)),
            weight = sapply(year:year2, capture_recapture),
            geom = "bar")  +
        labs(x = "year",
             y = "estimated rat population") +
        theme_bw()

    })
}


# Run the application 
shinyApp(ui = ui, server = server)
