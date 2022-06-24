###########################################
# Analysis of 311 Calls for Rat Sightings #
###########################################

# This script estimates (very roughly) the NYC rat population from 311 Calls for "Rat Sightings"
# For more detailed analysis see https://rss.onlinelibrary.wiley.com/doi/full/10.1111/j.1740-9713.2014.00764.x

# Author: Jonathan Auerbach
# Contact: jauerba@gmu.edu
# Date: 6/5/2022


library("tidyverse")
library(shiny)
library(sf)
library(leaflet)
library(tigris)

#modzcta shapefiles from NYC open data
unzip("Modified Zip Code Tabulation Areas (MODZCTA).zip")
modzcta <- st_read("geo_export_9d592ba8-7629-4558-9e78-e1e536b453d9.shp")
modzcta <- modzcta %>% mutate(modzcta = as.double(modzcta))


calls <- read_csv("nyc_rodent_compliants.csv")


# select "Rat Sightings" with year and BBL (lot) not missing
df <-
  calls %>%
  mutate(Date = as.Date(`Created Date`, format = "%m/%d/%Y"),
         Year = format(Date, "%y")) %>% 
  mutate(Date = as.Date(`Created Date`, format = "%m/%d/%Y"),
         Month = format(Date, "%m")) %>% 
  filter(Descriptor == "Rat Sighting",
         Year != 22) %>% 
  select(Year, BBL, Month, Borough, zip) %>%
  na.omit() %>%
  group_by(BBL, Year, Month, Borough, zip) %>%
  summarize(count = n()) %>%
  filter(BBL != 0)

write.csv(df, file="C:\\Users\\Icaro\\Documents\\ratsComplete.csv", row.names = FALSE)


ratsN <- read_csv("ratsComplete.csv")


# Function to estimate number of rats in any year (%y between 11 and 21) 
capture_recapture <- function(Yearr, Zop) {
  lots_year_1 <- ratsN %>% filter(Year == Yearr - 1) %>% filter(Month < 7) %>% pull(BBL)
  lots_year_2 <- ratsN %>% filter(Year == Yearr) %>% filter(Month < 7) %>% pull(BBL)
  lots_year_1Zop <- ratsN %>% filter(Year == Yearr - 1) %>% filter(zip == Zop) %>% pull(BBL)
  number_in_year_1 <- length(unique(lots_year_1))
  number_in_year_1Zop <- length(unique(lots_year_1Zop))
  number_in_year_2 <- length(unique(lots_year_2))
  number_in_year_1_and_2 <- sum(unique(lots_year_1) %in% unique(lots_year_2))
  
  50 * number_in_year_1Zop * number_in_year_2/number_in_year_1_and_2
  
}

  
df2 <-
  ratsN %>% select(Year, zip) %>% group_by(Year, zip) %>%
  summarize(count = n()) %>% select(Year, zip)

df3 <- df2 %>% mutate(RatsN = mapply(capture_recapture, Yearr = Year, Zop = zip))

ratsN2 <- read_csv("ratsComplete3.csv")

all_modzcta <- geo_join(modzcta, ratsN2, "modzcta", "Zip", how = "inner")
all_modzcta <-all_modzcta %>%
  st_transform(crs = "+init=epsg:4326")

saveRDS(all_modzcta, "all_modzcta.RDS")

#Palettes and Labels for percent map
labels_percent <-sprintf(
  "<strong>%s</strong><br/>%g%% ",
  all_modzcta$modzcta,all_modzcta$percent) %>%
  lapply(htmltools::HTML)

pal <-colorBin(palette="Blues", 9, domain = all_modzcta$percent)

#Palettes and Labels for pop map
labels_pop <-sprintf(
  "<strong>%s</strong><br/>%g rats ",
  all_modzcta$modzcta,all_modzcta$total) %>%
  lapply(htmltools::HTML)

pal2 <-colorBin(palette="Reds", 9, domain = all_modzcta$total)

map_interactive <-all_modzcta %>%
  st_transform(crs = "+init=epsg:4326") %>% #transform data into coordinate system
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(label = labels,
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
                bringToFront = TRUE))%>%
  addLegend("topleft",
            pal = pal,
            values = ~percent,
            title = "% of Total 311 Rat Sightings Reported",
            opacity = 0.7)
map_interactive



df4 <- read.csv("ratsComplete3.csv")
df4$Zip<-as.character(df4$Zip)



#final dataframes to use
all_modzcta <- geo_join(modzcta, df4, "modzcta", "Zip", how = "inner")
all_modzcta <-all_modzcta %>%
  st_transform(crs = "+init=epsg:4326")
all_modzcta <- all_modzcta %>% filter(Year > 10)






get_Year <- function(year) {
  
  df4 %>% filter(Year == year)
  
}

df7 = get_Year(12)


all_modzcta <- geo_join(modzcta, df7, "modzcta", "Zip", how = "inner")
all_modzcta <-all_modzcta %>%
  st_transform(crs = "+init=epsg:4326")
all_modzcta <- all_modzcta %>% filter(Year > 10)




df11 <- df4 %>% filter(Year == 11)
df12 <- df4 %>% filter(Year == 12)
df13 <- df4 %>% filter(Year == 13)
df14 <- df4 %>% filter(Year == 14)
df15 <- df4 %>% filter(Year == 15)
df16 <- df4 %>% filter(Year == 16)
df17 <- df4 %>% filter(Year == 17)
df18 <- df4 %>% filter(Year == 18)
df19 <- df4 %>% filter(Year == 19)
df20 <- df4 %>% filter(Year == 20)
df21 <- df4 %>% filter(Year == 21)


write.csv(df11, file="C:\\Users\\Icaro\\Documents\\ratsComplete11.csv", row.names = FALSE)
write.csv(df12, file="C:\\Users\\Icaro\\Documents\\ratsComplete12.csv", row.names = FALSE)
write.csv(df13, file="C:\\Users\\Icaro\\Documents\\ratsComplete13.csv", row.names = FALSE)
write.csv(df14, file="C:\\Users\\Icaro\\Documents\\ratsComplete14.csv", row.names = FALSE)
write.csv(df15, file="C:\\Users\\Icaro\\Documents\\ratsComplete15.csv", row.names = FALSE)
write.csv(df16, file="C:\\Users\\Icaro\\Documents\\ratsComplete16.csv", row.names = FALSE)
write.csv(df17, file="C:\\Users\\Icaro\\Documents\\ratsComplete17.csv", row.names = FALSE)
write.csv(df18, file="C:\\Users\\Icaro\\Documents\\ratsComplete18.csv", row.names = FALSE)
write.csv(df19, file="C:\\Users\\Icaro\\Documents\\ratsComplete19.csv", row.names = FALSE)
write.csv(df20, file="C:\\Users\\Icaro\\Documents\\ratsComplete20.csv", row.names = FALSE)
write.csv(df21, file="C:\\Users\\Icaro\\Documents\\ratsComplete21.csv", row.names = FALSE)



















# #################################################
# 
# #install.packages("shiny")
# library(shiny)
# #install.packages("leaflet")
# #install.packages("sf")
# library(ggplot2)
# library(sf)
# library(leaflet)
# #install.packages("tigris")
# library(tigris)
# 
# #modzcta shapefiles from NYC open data
# modzcta <- st_read("geo_export_9d592ba8-7629-4558-9e78-e1e536b453d9.shp")
# 
# 
# df4 <- read.csv("ratsComplete3.csv")
# df4$Zip<-as.character(df4$Zip)
# 
# 
# 
# #final dataframes to use
# all_modzcta <- geo_join(modzcta, df4, "modzcta", "Zip", how = "inner")
# all_modzcta <-all_modzcta %>%
#   st_transform(crs = "+init=epsg:4326")
# 
# 
# # shinyapp with tabs
# ui <- fluidPage("NYC Rat Population Estimate",
#                 
#                 tabPanel("Estimated Rat Population by ZIP",
#                          sidebarLayout(
#                            sidebarPanel(
#                              sliderInput("year",
#                                          "Year:",
#                                          min = 11,
#                                          max = 21,
#                                          value = 11)
#                            ),
#                            mainPanel(
#                              leafletOutput("pop_map")),
#                          )
#                          
#                 )
# )
# 
# 
# server <- function(input, output, session) {
#   
#   
#   output$pop_map <- renderLeaflet({
#     
#     time <- input$year
#     #all_modzcta <- all_modzcta %>% filter(Year == time)
#     
#     
#     #Palettes and Labels for pop map
#     labels_pop <-sprintf(
#       "<strong>%s</strong><br/>%g RATS!",
#       all_modzcta$modzcta,all_modzcta$RatsN) %>%
#       lapply(htmltools::HTML)
#     
#     pal2 <-colorBin(palette="Reds", 9, domain = all_modzcta$total)
#     
#     
#     leaflet(all_modzcta) %>%
#       addProviderTiles(provider = "CartoDB.Positron") %>%
#       addPolygons(label = labels_pop,
#                   stroke = FALSE,
#                   smoothFactor = 0.5,
#                   opacity = 1,
#                   fillOpacity = 0.7,
#                   fillColor = ~pal2(RatsN),
#                   highlightOptions = highlightOptions(
#                     weight = 5,
#                     color = "black",
#                     fillOpacity = 1,
#                     opacity = 1,
#                     bringToFront = TRUE)
#       )%>%
#       addLegend("topleft",
#                 pal = pal2,
#                 values = ~RatsN,
#                 title = "Estimated Number of Rats",
#                 opacity = 0.7)
#     
#   })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
# 
