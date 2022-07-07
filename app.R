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

df <- read.csv("df_NYC.csv")

#ORGANIZING DATA FOR PERCENT MAP (angela)
#percent of calls in each zip code
total <- sum(df$count) #total 311 calls
df21 <- df %>%
  filter(Year == 2021) 
total <- sum(df21$count)

rats_percent <- df21 %>%
  group_by(Zip) %>%
  summarize(percent = (sum(count)/total)*100) 
rats_percent$Zip<-as.character(rats_percent$Zip)


#ORGANIZING DATA FOR POPULATION MAP (mari)
df2 <- df %>%
  group_by(Zip,BBL) %>%
  summarize(reports2020 = (sum(Year==2020)),
            reports2021 = (sum(Year==2021)), 
            both = (reports2020 > 0 & reports2021 > 0)
  )

# grouping by zip to count total lots in each zipcode and capture recapture for each lot
rats_pop <- df2 %>% 
  group_by(Zip) %>%
  summarize(markedlotstotal2020 = sum(reports2020 > 0), 
            markedlotstotal2021 = sum(reports2021 > 0),
            
            #number of identical lots in both years 
            #figure out rat sightings in identical lots in both years
            both = (sum(both > 0)),
            total = ifelse( both == 0, 0, 50*markedlotstotal2020*markedlotstotal2021/both)) %>%
  
  #  ifelse( both > 0 & markedlotstotal2021 > 0 & markedlotstotal2020 > 0,       #50*markedlotstotal2020*markedlotstotal2021/both,
  #  ifelse(both == 0 & markedlotstotal2021 >0 & markedlotstotal2020 >0,  50*markedlotstotal2020*markedlotstotal2021,
  # ifelse(markedlotstotal2020 == 0,  50*markedlotstotal2021,
  #  ifelse(markedlotstotal2021 == 0,  50*markedlotstotal2020))))) %>% 
  mutate(zip = as.character(Zip)) %>%
  select(Zip, total)

rats_pop$total <- round(rats_pop$total,-2)

#ORGANIZING DATA FOR YEAR MAP (icaro)
#separate dataframes for rat count each year 

#capture recapture by nyc 
df11 <- read.csv("ratsComplete11.csv")
df11$Zip<-as.character(df11$Zip)

df12 <- read.csv("ratsComplete12.csv")
df12$Zip<-as.character(df12$Zip)

df13 <- read.csv("ratsComplete13.csv")
df13$Zip<-as.character(df13$Zip)

df14 <- read.csv("ratsComplete14.csv")
df14$Zip<-as.character(df14$Zip)

df15 <- read.csv("ratsComplete15.csv")
df15$Zip<-as.character(df15$Zip)

df16 <- read.csv("ratsComplete16.csv")
df16$Zip<-as.character(df16$Zip)

df17 <- read.csv("ratsComplete17.csv")
df17$Zip<-as.character(df17$Zip)

df18 <- read.csv("ratsComplete18.csv")
df18$Zip<-as.character(df18$Zip)

df19 <- read.csv("ratsComplete19.csv")
df19$Zip<-as.character(df19$Zip)

df20 <- read.csv("ratsComplete20.csv")
df20$Zip<-as.character(df20$Zip)

df21 <- read.csv("ratsComplete21.csv")
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


#capture recapture by zip 
df11_1 <- read.csv("ratsComplete11_10.csv")
df11_1$Zip<-as.character(df11_1$Zip)

df12_1 <- read.csv("ratsComplete12_10.csv")
df12_1$Zip<-as.character(df12_1$Zip)

df13_1 <- read.csv("ratsComplete13_10.csv")
df13_1$Zip<-as.character(df13_1$Zip)

df14_1 <- read.csv("ratsComplete14_10.csv")
df14_1$Zip<-as.character(df14_1$Zip)

df15_1 <- read.csv("ratsComplete15_10.csv")
df15_1$Zip<-as.character(df15_1$Zip)

df16_1 <- read.csv("ratsComplete16_10.csv")
df16_1$Zip<-as.character(df16_1$Zip)

df17_1 <- read.csv("ratsComplete17_10.csv")
df17_1$Zip<-as.character(df17_1$Zip)

df18_1 <- read.csv("ratsComplete18_10.csv")
df18_1$Zip<-as.character(df18_1$Zip)

df19_1 <- read.csv("ratsComplete19_10.csv")
df19_1$Zip<-as.character(df19_1$Zip)

df20_1 <- read.csv("ratsComplete20_10.csv")
df20_1$Zip<-as.character(df20_1$Zip)

df21_1 <- read.csv("ratsComplete21_10.csv")
df21_1$Zip<-as.character(df21_1$Zip)

all_modzcta11_1 <- geo_join(modzcta, df11_1, "modzcta", "Zip", how = "inner")
all_modzcta11_1 <- all_modzcta11_1 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta12_1 <- geo_join(modzcta, df12_1, "modzcta", "Zip", how = "inner")
all_modzcta12_1 <- all_modzcta12_1 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta13_1 <- geo_join(modzcta, df13_1, "modzcta", "Zip", how = "inner")
all_modzcta13_1 <- all_modzcta13_1 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta14_1 <- geo_join(modzcta, df14_1, "modzcta", "Zip", how = "inner")
all_modzcta14_1 <- all_modzcta14_1 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta15_1 <- geo_join(modzcta, df15_1, "modzcta", "Zip", how = "inner")
all_modzcta15_1 <- all_modzcta15_1 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta16_1 <- geo_join(modzcta, df16_1, "modzcta", "Zip", how = "inner")
all_modzcta16_1 <- all_modzcta16_1 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta17_1 <- geo_join(modzcta, df17_1, "modzcta", "Zip", how = "inner")
all_modzcta17_1 <- all_modzcta17_1 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta18_1 <- geo_join(modzcta, df18_1, "modzcta", "Zip", how = "inner")
all_modzcta18_1 <- all_modzcta18_1 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta19_1 <- geo_join(modzcta, df19_1, "modzcta", "Zip", how = "inner")
all_modzcta19_1 <- all_modzcta19_1 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta20_1 <- geo_join(modzcta, df20_1, "modzcta", "Zip", how = "inner")
all_modzcta20_1 <- all_modzcta20_1 %>%
  st_transform(crs = "+init=epsg:4326")

all_modzcta21_1 <- geo_join(modzcta, df21_1, "modzcta", "Zip", how = "inner")
all_modzcta21_1 <- all_modzcta21_1 %>%
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
    tabPanel("2021 Rat Sighting Reports by ZIP",
             sidebarLayout(
               sidebarPanel(p("This interactive map shows the frequency of rat sightings reported to New York City's service request hotline in 2021 by ZIP code.
                              Darker blue areas indicate more calls. As seen, there are more reported sightings in the Upper West Side and Queens."),
                            p("However, it is important to note the discrepancy between 311 call rates and the estimated rat population in the zip codes. For instance, upon comparing the maps, the zip code 10025 in the Upper West Side had high call rates but relatively low rates of rat infested lots. This indicates that, at least for the rat problem, 311 calls are not completely representative.")
                            ),
               mainPanel(
                 leafletOutput("percent_map")
                 )
               )
             ),
    
    #mari
    tabPanel("Estimated 2021 Rat Population by ZIP",
             sidebarLayout(
               sidebarPanel(p("Estimating the rat population in 2021 using 311 calls and the mark-recapture method."),
                            p("For this map, we are using cleaned and processed 311 call data from NYC opendata. We are also using the Mark recapture technique which is used to estimate the size of a population where it is impractical to count every individual. The basic idea is that you capture a small number of individuals, put a harmless mark on them, and release them back into the population. At a later date, you catch another small group, and record how many have a mark. In a small population, you are more likely to recapture marked individuals, whereas in a large population, you are less likely."),
                            p("We are using this technique in a slightly different way, instead of capturing and marking rats, we are using BBLs, building block lots, and 311 calls. In place of marking rats, we are marking bbls that have a rat sighting, and the recapture is if the same BBL is called in again 1 year later with another rat sighting."),
                            p("Assumptions: If any of the following assumptions or conditions are violated, it may affect the accuracy of the population estimate. "),
                            p("1. If no marked individuals are recaptured, R = 0 and your result is undefined. Mark more individuals and try again."),
                            p("2. Each called in BBL contains a colony of 50 rats"),
                            p("3. There must be no immigration into or emigration out of the population"),
                            p("4. The marking experience must not make an individual more or less likely to be recaptured."),
                            p("The first and last assumption is one to think about, if in some zip codes people are more likely to call 311 then there is more data and a more accurately estimated population can arise from it. But as we can see in this map, some zip codes have an estimated zero rat population. Surely this cannot be correct, this occurs when there are no identical BBLs that are reported in either year or when R = 0. This does not mean no rat population but that 311 calls may not be the best way to estimate numbers for this zip code."),
                            p("There are 38 zip codes that returned a zero rat population: let's examine why this happened"),
                            p("8 zip codes had no calls in 2020 and 2021(these are very small zip codes with no real lots in them) and 8 other zip codes had no calls reported in either 2020 or 2021. The remaining 22 zip codes had calls from both years but none of the BBLs were identical (R = 0)")
               ),
               mainPanel(
                 leafletOutput("pop_map")
               )
             )
    ),
    
    #icaro
    tabPanel("Estimated Rat Population by Year (nyc)",
             sidebarLayout(
               sidebarPanel(
                 selectInput("year", "Year:",
                             c(2011,2012,2013,2014,2015,
                                2016,2017,2018,2019,2020,2021)),
                 p("A HeatMap showing the number of rats in NYC’s neighborhoods from 2011 to 2021. 
      The numbers were obtained using a Capture-Recapture based method and the recapture factor was assumed to be constant throughout NYC. 
      Also, a buffer period of 6 months was taken into account."),
                 br(),
                 p("Notable observations:"),
                 p("     - Red regions are not abrupt but gradual → within expectations"),
                 p("     - The number of rats decreased from 2012 to 2015, but the improvement proved to be short-lived."),
                 p("     - Queens and Brooklyn show higher rats density"),
                 p("     - In Manhattan and The Bronx, the neighborhoods with a higher number of rats seem to be located close to parks")
               ),
               mainPanel(
                 leafletOutput("year_map"))
             )
             ),
    
    tabPanel("Estimated Rat Population by Year (zip)",
             sidebarLayout(
               sidebarPanel(
                 selectInput("year1", "Year:",
                             c(2011,2012,2013,2014,2015,
                               2016,2017,2018,2019,2020,2021))),
               mainPanel(
                 leafletOutput("year_map_zip"))
             )
    ),
    
    tabPanel("Other Data Visualizations",
             sidebarLayout(
               sidebarPanel(
                 p("Rats are a part of New York, whether we like it or not. From the iconic Pizza rat to their mascot Buddy the Rat, who dons a suit and a giant rat mask, New York has no shortage of rats. But how many Rats are there really in New York? Which borough harbors the most rats? Which boroughs have had their rat neighbors grow the most? Here we have two animated graphs showing just that. The first graph shows the cumulative number of rats separated into boroughs over the years. While the first graph better shows the total number of rats, it is difficult to see the population growth by borough. The second graph better represents how much the rat population has grown in each borough."),
                 p("Disclaimer: The methods used in estimating the number of rats may differ depending on how the data is processed. For example, aplying the capture_recapture function to the New York City as a whole gives a total rat population of 3.1 million. However, applying the previously mentioned function on each of the  New York City Boroughs and then adding their results, gives a total of 3.3 million rats. Acceptable varaince still needs to be discussed. So, take these values with a grain of salt and a ±10% range of error.")
               ),
               mainPanel(img(src="graph2anim.gif", align = "left", height='300px', width='600px'),
                         img(src="graph1anim.gif", align = "left", height='300px', width='600px'),
                        img(src="graph1.png", align = "left", height='300px', width='600px')))
             )
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
    
    output$year_map_zip <- renderLeaflet({
      time <- input$year1
      
      
      if(time == 2011){
        all_modzcta <- all_modzcta11_1
        
      } else if(time == 2012){
        all_modzcta <- all_modzcta12_1
        
      } else if(time == 2013){
        all_modzcta <- all_modzcta13_1
        
      } else if(time == 2014){
        all_modzcta <- all_modzcta14_1
        
      } else if(time == 2015){
        all_modzcta <- all_modzcta15_1
        
      } else if(time == 2016){
        all_modzcta <- all_modzcta16_1
        
      } else if(time == 2017){
        all_modzcta <- all_modzcta17_1
        
      } else if(time == 2018){
        all_modzcta <- all_modzcta18_1
        
      } else if(time == 2019){
        all_modzcta <- all_modzcta19_1
        
      } else if(time == 2020){
        all_modzcta <- all_modzcta20_1
        
      } else if(time == 2021){
        all_modzcta <- all_modzcta21_1
        
      }
      
      #Palettes and Labels for pop map
      labels_year_zip <-sprintf(
        "<strong>%s</strong><br/>%g RATS!",
        all_modzcta$modzcta,all_modzcta$RatsN) %>%
        lapply(htmltools::HTML)
      
      pal4 <-colorBin(palette="Reds", 9, domain = all_modzcta$RatsN)
      
      
      leaflet(all_modzcta) %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        addPolygons(label = labels_year_zip,
                    stroke = FALSE,
                    smoothFactor = 0.5,
                    opacity = 1,
                    fillOpacity = 0.7,
                    fillColor = ~pal4(RatsN),
                    highlightOptions = highlightOptions(
                      weight = 5,
                      color = "black",
                      fillOpacity = 1,
                      opacity = 1,
                      bringToFront = TRUE)
        )%>%
        addLegend("topleft",
                  pal = pal4,
                  values = ~RatsN,
                  title = "Estimated Number of Rats",
                  opacity = 0.7)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
