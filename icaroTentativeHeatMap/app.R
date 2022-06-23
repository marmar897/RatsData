library(shiny)
library(ggplot2)
library(sf)
library(leaflet)
library(tigris)

#modzcta shapefiles from NYC open data

modzcta <- st_read("geo_export_9d592ba8-7629-4558-9e78-e1e536b453d9.shp")
df4 <- read.csv("ratsComplete3.csv")


# shinyapp with tabs
ui <- fluidPage("NYC Rat Population Estimate",
                 
                 tabPanel("Estimated Rat Population by ZIP",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("year",
                                          "Year:",
                                           min = 11,
                                           max = 21,
                                           value = 11)
                            ),
                            mainPanel(
                              leafletOutput("pop_map")),
                           )

                )
)


server <- function(input, output, session) {
  
  
  output$pop_map <- renderLeaflet({
    
    df4$Zip<-as.character(df4$Zip)
    time <- input$year
    
    df5 <- reactive({
      
      req(df4$Year)
      df4 <- filter(df4, Year == time)
      
    })
  
    
    #final dataframes to use
    all_modzcta <- geo_join(modzcta, df5(), "modzcta", "Zip", how = "inner")
    all_modzcta <-all_modzcta %>%
      st_transform(crs = "+init=epsg:4326")
    
    
    #Palettes and Labels for pop map
    labels_pop <-sprintf(
      "<strong>%s</strong><br/>%g RATS!",
      all_modzcta$modzcta,all_modzcta$RatsN) %>%
      lapply(htmltools::HTML)
    
    pal2 <-colorBin(palette="Reds", 9, domain = all_modzcta$total)
    
    
    leaflet(all_modzcta) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(label = labels_pop,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal2(RatsN),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "black",
                    fillOpacity = 1,
                    opacity = 1,
                    bringToFront = TRUE)
      )%>%
      addLegend("topleft",
                pal = pal2,
                values = ~RatsN,
                title = "Estimated Number of Rats",
                opacity = 0.7)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)