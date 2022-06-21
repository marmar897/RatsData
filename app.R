#install.packages("shiny")
library(shiny)
#install.packages("leaflet")
#install.packages("sf")
library(ggplot2)
library(sf)
library(leaflet)
#install.packages("tigris")
library(tigris)

#modzcta shapefiles from NYC open data
unzip("Modified Zip Code Tabulation Areas (MODZCTA).zip")
modzcta <- st_read("geo_export_9d592ba8-7629-4558-9e78-e1e536b453d9.shp")
head(modzcta)

df1 <- read.csv("rats_cleaned.csv")
df <- df1 %>%
  filter(Year > 2014)

#percent of calls in each zip code
total <- sum(df$count) #total 311 calls
df <- df %>%
  group_by(zip) %>%
  summarize(percent = (sum(count)/total)*100) 
df$zip<-as.character(df$zip)

all_modzcta <- geo_join(modzcta, df, "modzcta", "zip", how = "inner")
all_modzcta <-all_modzcta %>%
  st_transform(crs = "+init=epsg:4326")

#Palettes and Labels for percent map
labels_percent <-sprintf(
  "<strong>%s</strong><br/>%g%% ",
  all_modzcta$modzcta,all_modzcta$percent) %>%
  lapply(htmltools::HTML)

pal <-colorBin(palette="Reds", 9, domain = all_modzcta$percent)


# 3 tabs
ui <- navbarPage("NYC Rat Population Estimate",
  
    tabPanel("Rat Sighting Reports by ZIP",
             leafletOutput("percent_map")),
    
    tabPanel("Estimated Rat Population by ZIP",
              sidebarLayout(
                sidebarPanel(
                  numericInput("zip", 
                  label = "Which ZIP code are you interested in?", 
                  value = 00000)
               ),
               mainPanel(
                 plotOutput("rats")),
               )
             ),
    tabPanel("Other Data Visualizations")
)


server <- function(input, output, session) {

    output$percent_map <- renderLeaflet({  
      leaflet(all_modzcta) %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        addPolygons(label = labels_percent,
                    stroke = FALSE,
                    smoothFactor = 0.5,
                    opacity = 1,
                    fillOpacity = 0.7,
                    fillColor = ~pal(percent),
                    highlightOptions = highlightOptions(
                      weight = 5,
                      color = "black",
                      fillOpacity = 1,
                      opacity = 1,
                      bringToFront = TRUE)
        )%>%
        addLegend("topleft",
                  pal = pal,
                  values = ~percent,
                  title = "% of Total 311 Rat Sightings Reported",
                  opacity = 0.7)
 
    })
    
    output$rats <- renderPlot({
      plotRats <- df1 %>% 
        filter(zip == input$zip) %>%
        group_by(Year) %>%
        summarize(Count = sum(count))
      
      ggplot(plotRats, aes(x=Year, y=Count)) +
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
