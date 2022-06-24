#install.packages("shiny")
library(shiny)
#install.packages("leaflet")
#install.packages("sf")
library(ggplot2)
library(sf)
library(leaflet)
#install.packages("tigris")
library(tigris)
library(dplyr)
library(magrittr)
#install.packages('rsconnect')

#modzcta shapefiles from NYC open data
#unzip("Modified Zip Code Tabulation Areas (MODZCTA).zip")
modzcta <- st_read("geo_export_9d592ba8-7629-4558-9e78-e1e536b453d9.shp")

df <- read.csv("Dataframes/dfNYC.csv")
icaro <- read.csv("ratsComplete3.csv")
#df <- df %>% rename(zip = Incident.Zip)

#ORGANIZING DATA FOR PERCENT MAP (angela)
#percent of calls in each zip code
total <- sum(df$count) #total 311 calls
rats_percent <- df %>%
  filter(Year == 20) %>%
  group_by(Zip) %>%
  summarize(percent = (sum(count)/total)*100) 
rats_percent$Zip<-as.character(rats_percent$Zip)


#ORGANIZING DATA FOR POPULATION MAP (mari)

# creating 2020 and 2021 dataframes
zips_year_1 <- df %>% filter(Year == 20) 
zips_year_2 <- df %>% filter(Year == 21)

# estimate number of rats in each zipcode
#capture_recapture function with zipcode filter and addition year 2019-2020 dataset

count_per_zip <- function(zipcode){
  lots_y2020 <- zips_year_1 %>% filter(Zip == zipcode)  %>%pull(BBL)
  lots_y2021 <- zips_year_2 %>% filter (Zip == zipcode) %>%pull(BBL)
  number_in_year_1 <- length(lots_y2020)
  #print(number_in_year_1)
  number_in_year_2 <- length(lots_y2021)
  #print(number_in_year_2)
  number_in_year_1_and_2 <- sum(lots_y2021 %in% lots_y2020)
  #sum(lots_year_1 %in% lots_year_2)
  #print(number_in_year_2)
  50 * number_in_year_1 * number_in_year_2 / number_in_year_1_and_2
}

#data processing
rats_pop <- df %>%
  group_by(Zip) %>%
  summarize(total = (count_per_zip(Zip))) 
rats_pop$Zip<-as.character(rats_pop$Zip)

#ORGANIZING DATA FOR YEAR MAP (icaro)
#separate dataframes for rat count each year 

df4 <- read.csv("Dataframes/ratsComplete3.csv")
df4$Zip<-as.character(df4$Zip)

df11 <- read.csv("Dataframes/ratsComplete11.csv")
df11$Zip<-as.character(df11$Zip)

df12 <- read.csv("Dataframes/ratsComplete12.csv")
df12$Zip<-as.character(df12$Zip)

df13 <- read.csv("Dataframes/ratsComplete13.csv")
df13$Zip<-as.character(df13$Zip)

df14 <- read.csv("Dataframes/ratsComplete14.csv")
df14$Zip<-as.character(df14$Zip)

df15 <- read.csv("Dataframes/ratsComplete15.csv")
df15$Zip<-as.character(df15$Zip)

df16 <- read.csv("Dataframes/ratsComplete16.csv")
df16$Zip<-as.character(df16$Zip)

df17 <- read.csv("Dataframes/ratsComplete17.csv")
df17$Zip<-as.character(df17$Zip)

df18 <- read.csv("Dataframes/ratsComplete18.csv")
df18$Zip<-as.character(df18$Zip)

df19 <- read.csv("Dataframes/ratsComplete19.csv")
df19$Zip<-as.character(df19$Zip)

df20 <- read.csv("Dataframes/ratsComplete20.csv")
df20$Zip<-as.character(df20$Zip)

df21 <- read.csv("Dataframes/ratsComplete21.csv")
df21$Zip<-as.character(df21$Zip)


#final dataframes to use
rats <- merge(rats_percent, rats_pop)
all_modzcta <- geo_join(modzcta, rats, "modzcta", "Zip", how = "inner")
all_modzcta <-all_modzcta %>%
  st_transform(crs = "+init=epsg:4326")

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


#Palettes and Labels for percent map
labels_percent <-sprintf(
  "<strong>%s</strong><br/>%g%% ",
  all_modzcta$modzcta,all_modzcta$percent) %>%
  lapply(htmltools::HTML)

pal1 <-colorBin(palette="Blues", 9, domain = all_modzcta$percent)

#Palettes and Labels for pop map
labels_pop <-sprintf(
  "<strong>%s</strong><br/>%g rats ",
  all_modzcta$modzcta,all_modzcta$total) %>%
  lapply(htmltools::HTML)

pal2 <-colorBin(palette="Reds", 9, domain = all_modzcta$total)


# shinyapp with tabs
ui <- navbarPage("NYC Rat Population Estimate",
    
    #angela
    tabPanel("2020 Rat Sighting Reports by ZIP",
             leafletOutput("percent_map")),
    
    #mari
    tabPanel("Estimated 2020 Rat Population by ZIP",
            leafletOutput("pop_map")),
    
    #icaro
    tabPanel("Estimated Rat Population by Year",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year",
                             "Year:",
                             min = 11,
                             max = 21,
                             value = 11)
               ),
               mainPanel(
                 leafletOutput("year_map")),
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
                    fillColor = ~pal1(percent),
                    highlightOptions = highlightOptions(
                      weight = 5,
                      color = "black",
                      fillOpacity = 1,
                      opacity = 1,
                      bringToFront = TRUE)
        )%>%
        addLegend("topleft",
                  pal = pal1,
                  values = ~percent,
                  title = "% of Total 311 Rat Sightings Reported",
                  opacity = 0.7)
 
    })
    
    output$pop_map <- renderLeaflet({
      leaflet(all_modzcta) %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        addPolygons(label = labels_pop,
                    stroke = FALSE,
                    smoothFactor = 0.5,
                    opacity = 1,
                    fillOpacity = 0.7,
                    fillColor = ~pal2(total),
                    highlightOptions = highlightOptions(
                      weight = 5,
                      color = "black",
                      fillOpacity = 1,
                      opacity = 1,
                      bringToFront = TRUE)
        )%>%
        addLegend("topleft",
                  pal = pal2,
                  values = ~total,
                  title = "Estimated Number of Rats",
                  opacity = 0.7)

    })
    
    output$year_map <- renderLeaflet({
      time <- input$year
      
      
      if(time == 11){
        all_modzcta <- all_modzcta11
        
      } else if(time == 12){
        all_modzcta <- all_modzcta12
        
      } else if(time == 13){
        all_modzcta <- all_modzcta13
        
      } else if(time == 14){
        all_modzcta <- all_modzcta14
        
      } else if(time == 15){
        all_modzcta <- all_modzcta15
        
      } else if(time == 16){
        all_modzcta <- all_modzcta16
        
      } else if(time == 17){
        all_modzcta <- all_modzcta17
        
      } else if(time == 18){
        all_modzcta <- all_modzcta18
        
      } else if(time == 19){
        all_modzcta <- all_modzcta19
        
      } else if(time == 20){
        all_modzcta <- all_modzcta20
        
      } else if(time == 21){
        all_modzcta <- all_modzcta21
        
      }
      
      #Palettes and Labels for pop map
      labels_year <-sprintf(
        "<strong>%s</strong><br/>%g RATS!",
        all_modzcta$modzcta,all_modzcta$RatsN) %>%
        lapply(htmltools::HTML)
      
      pal3 <-colorBin(palette="Reds", 9, domain = all_modzcta$RatsN)
      
      
      leaflet(all_modzcta) %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        addPolygons(label = labels_year,
                    stroke = FALSE,
                    smoothFactor = 0.5,
                    opacity = 1,
                    fillOpacity = 0.7,
                    fillColor = ~pal3(RatsN),
                    highlightOptions = highlightOptions(
                      weight = 5,
                      color = "black",
                      fillOpacity = 1,
                      opacity = 1,
                      bringToFront = TRUE)
        )%>%
        addLegend("topleft",
                  pal = pal3,
                  values = ~RatsN,
                  title = "Estimated Number of Rats",
                  opacity = 0.7)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
