library(shiny)
library(ggplot2)
library(sf)
library(leaflet)
library(tigris)
library(rjson)

#modzcta shapefiles from NYC open data

modzcta <- st_read("geo_export_9d592ba8-7629-4558-9e78-e1e536b453d9.shp")

df11 <- read.csv("ratsCompleteNeighborhoodBuff11.csv")
df11$zip<-as.character(df11$zip)

df12 <- read.csv("ratsCompleteNeighborhoodBuff12.csv")
df12$zip<-as.character(df12$zip)

df13 <- read.csv("ratsCompleteNeighborhoodBuff13.csv")
df13$zip<-as.character(df13$zip)

df14 <- read.csv("ratsCompleteNeighborhoodBuff14.csv")
df14$zip<-as.character(df14$zip)

df15 <- read.csv("ratsCompleteNeighborhoodBuff15.csv")
df15$zip<-as.character(df15$zip)

df16 <- read.csv("ratsCompleteNeighborhoodBuff16.csv")
df16$zip<-as.character(df16$zip)

df17 <- read.csv("ratsCompleteNeighborhoodBuff17.csv")
df17$zip<-as.character(df17$zip)

df18 <- read.csv("ratsCompleteNeighborhoodBuff18.csv")
df18$zip<-as.character(df18$zip)

df19 <- read.csv("ratsCompleteNeighborhoodBuff19.csv")
df19$zip<-as.character(df19$zip)

df20 <- read.csv("ratsCompleteNeighborhoodBuff20.csv")
df20$zip<-as.character(df20$zip)

df21 <- read.csv("ratsCompleteNeighborhoodBuff21.csv")
df21$zip<-as.character(df21$zip)

#final dataframes to use
all_modzcta11 <- geo_join(modzcta, df11, "modzcta", "zip", how = "inner")
all_modzcta11 <- all_modzcta11 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta12 <- geo_join(modzcta, df12, "modzcta", "zip", how = "inner")
all_modzcta12 <- all_modzcta12 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta13 <- geo_join(modzcta, df13, "modzcta", "zip", how = "inner")
all_modzcta13 <- all_modzcta13 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta14 <- geo_join(modzcta, df14, "modzcta", "zip", how = "inner")
all_modzcta14 <- all_modzcta14 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta15 <- geo_join(modzcta, df15, "modzcta", "zip", how = "inner")
all_modzcta15 <- all_modzcta15 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta16 <- geo_join(modzcta, df16, "modzcta", "zip", how = "inner")
all_modzcta16 <- all_modzcta16 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta17 <- geo_join(modzcta, df17, "modzcta", "zip", how = "inner")
all_modzcta17 <- all_modzcta17 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta18 <- geo_join(modzcta, df18, "modzcta", "zip", how = "inner")
all_modzcta18 <- all_modzcta18 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta19 <- geo_join(modzcta, df19, "modzcta", "zip", how = "inner")
all_modzcta19 <- all_modzcta19 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta20 <- geo_join(modzcta, df20, "modzcta", "zip", how = "inner")
all_modzcta20 <- all_modzcta20 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta21 <- geo_join(modzcta, df21, "modzcta", "zip", how = "inner")
all_modzcta21 <- all_modzcta21 %>%
  st_transform(crs = "+init=epsg:4326")


# shinyapp with tabs
ui <- fluidPage("NYC Rat Population Estimate",
                
                tabPanel("Estimated Rat Population by zip",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("year", "Year:",
                                         c(2011,2012,2013,2014,2015,2016,
                                           2017,2018,2019,2020,2021))),
                           mainPanel(
                             leafletOutput("pop_map")),
                         )
                         
                )
)


server <- function(input, output, session) {
  
  
  output$pop_map <- renderLeaflet({
    
    time <- input$year
    
    
    if(time == 2011){
      all_modzcta <- all_modzcta11
      
    } else if(time == 2012){
      all_modzcta <- all_modzcta12
      
    } else if(time == 2013){
      all_modzcta <- all_modzcta13
      
    } else if(time == 2014){
      all_modzcta <- all_modzcta14
      
    } else if(time == 2015){
      all_modzcta <- all_modzcta15
      
    } else if(time == 2016){
      all_modzcta <- all_modzcta16
      
    } else if(time == 2017){
      all_modzcta <- all_modzcta17
      
    } else if(time == 2018){
      all_modzcta <- all_modzcta18
      
    } else if(time == 2019){
      all_modzcta <- all_modzcta19
      
    } else if(time == 2020){
      all_modzcta <- all_modzcta20
      
    } else if(time == 2021){
      all_modzcta <- all_modzcta21
      
    }
    
    #Palettes and Labels for pop map
    labels_pop <-sprintf(
      "<strong>%s</strong><br/>%g RATS!",
      all_modzcta$modzcta,all_modzcta$Number) %>%
      lapply(htmltools::HTML)
    
    pal2 <-colorBin(palette="Reds", 9, domain = all_modzcta$total)
    
    
    leaflet(all_modzcta) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(label = labels_pop,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~pal2(Number),
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "black",
                    fillOpacity = 1,
                    opacity = 1,
                    bringToFront = TRUE)
      )%>%
      addLegend("topleft",
                pal = pal2,
                values = ~Number,
                title = "Estimated Number of Rats",
                opacity = 0.7)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)