
library(shiny)
library(ggplot2)


df <- read.csv("Data/rats_cleaned.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Rat Estimate"),
  
  numericInput("zip", label = "Which ZIP code are you interested in?", value = 00000),
  
  plotOutput("rats")
  
)

server <- function(input, output) {
  
  output$rats <- renderPlot({
    df1 <- df %>% 
      filter(zip == input$zip) %>%
      group_by(Year) %>%
      summarize(Count = sum(count))
    
    ggplot(df1, aes(x=Year, y=Count)) +
      geom_bar(stat="identity") +
      labs(title = paste("Estimated Rat Population in the zip code: ", input$zip)) +
      theme(panel.grid = element_blank(), 
            axis.line.x = element_line(), 
            axis.line.y = element_line(), 
            panel.background = element_rect(fill = "white"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

