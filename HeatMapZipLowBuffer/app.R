library(shiny)
library(ggplot2)
library(sf)
library(leaflet)
library(tigris)
library(rjson)

#modzcta shapefiles from NYC open data

modzcta <- st_read("geo_export_9d592ba8-7629-4558-9e78-e1e536b453d9.shp")

df11 <- read.csv("ratsComplete11_10.csv")
df11$Zip<-as.character(df11$Zip)

df12 <- read.csv("ratsComplete12_10.csv")
df12$Zip<-as.character(df12$Zip)

df13 <- read.csv("ratsComplete13_10.csv")
df13$Zip<-as.character(df13$Zip)

df14 <- read.csv("ratsComplete14_10.csv")
df14$Zip<-as.character(df14$Zip)

df15 <- read.csv("ratsComplete15_10.csv")
df15$Zip<-as.character(df15$Zip)

df16 <- read.csv("ratsComplete16_10.csv")
df16$Zip<-as.character(df16$Zip)

df17 <- read.csv("ratsComplete17_10.csv")
df17$Zip<-as.character(df17$Zip)

df18 <- read.csv("ratsComplete18_10.csv")
df18$Zip<-as.character(df18$Zip)

df19 <- read.csv("ratsComplete19_10.csv")
df19$Zip<-as.character(df19$Zip)

df20 <- read.csv("ratsComplete20_10.csv")
df20$Zip<-as.character(df20$Zip)

df21 <- read.csv("ratsComplete21_10.csv")
df21$Zip<-as.character(df21$Zip)

#final dataframes to use
all_modzcta11 <- geo_join(modzcta, df11, "modzcta", "Zip", how = "inner")
all_modzcta11 <- all_modzcta11 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta12 <- geo_join(modzcta, df12, "modzcta", "Zip", how = "inner")
all_modzcta12 <- all_modzcta12 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta13 <- geo_join(modzcta, df13, "modzcta", "Zip", how = "inner")
all_modzcta13 <- all_modzcta13 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta14 <- geo_join(modzcta, df14, "modzcta", "Zip", how = "inner")
all_modzcta14 <- all_modzcta14 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta15 <- geo_join(modzcta, df15, "modzcta", "Zip", how = "inner")
all_modzcta15 <- all_modzcta15 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta16 <- geo_join(modzcta, df16, "modzcta", "Zip", how = "inner")
all_modzcta16 <- all_modzcta16 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta17 <- geo_join(modzcta, df17, "modzcta", "Zip", how = "inner")
all_modzcta17 <- all_modzcta17 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta18 <- geo_join(modzcta, df18, "modzcta", "Zip", how = "inner")
all_modzcta18 <- all_modzcta18 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta19 <- geo_join(modzcta, df19, "modzcta", "Zip", how = "inner")
all_modzcta19 <- all_modzcta19 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta20 <- geo_join(modzcta, df20, "modzcta", "Zip", how = "inner")
all_modzcta20 <- all_modzcta20 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta21 <- geo_join(modzcta, df21, "modzcta", "Zip", how = "inner")
all_modzcta21 <- all_modzcta21 %>%
  st_transform(crs = "+init=epsg:4326")


# shinyapp with tabs
ui <- fluidPage("NYC Rat Population Estimate",
                
                tabPanel("Estimated Rat Population by ZIP",
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