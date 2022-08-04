###########################################
# Analysis of 311 Calls for Rat Sightings #
###########################################


library("tidyverse")

data <- read_csv("ratsCompleteNeighborhoodBuffNei.csv")

#Dynamic Repacture function used in the final app
capture_recaptureDynamic <- function(Yearr, Zop, Nei, Per) {
  
  per <- 0.015
  
  if(Yearr == 2011){
    per <- 0.015936255
  } else if(Yearr == 2012){
    per <- 0.016712591
  } else if(Yearr == 2013){
    per <- 0.01482511
  } else if(Yearr == 2014){
    per <- 0.014650338
  } else if(Yearr == 2015){
    per <- 0.014080793
  } else if(Yearr == 2016){
    per <- 0.015540414
  } else if(Yearr == 2017){
    per <- 0.01520979
  } else if(Yearr == 2018){
    per <- 0.015356265
  } else if(Yearr == 2019){
    per <- 0.016419336
  } else if(Yearr == 2020){
    per <- 0.016305089
  } else if(Yearr == 2021){
    per <- 0.015075866
  }
  
  if(Per <= per){
    
    lots_year_1 <- data %>% filter(Year == Yearr - 1) %>% filter(Month < 10) %>% pull(BBL)
    lots_year_2 <- data %>% filter(Year == Yearr) %>% pull(BBL)
    lots_year_1Zop <- data %>% filter(Year == Yearr - 1) %>% filter(Month < 10) %>% filter(zip == Zop) %>% pull(BBL)
    number_in_year_1 <- length(unique(lots_year_1))
    number_in_year_1Zop <- length(unique(lots_year_1Zop))
    number_in_year_2 <- length(unique(lots_year_2))
    number_in_year_1_and_2 <- sum(unique(lots_year_1) %in% unique(lots_year_2))
    
    50 * number_in_year_1Zop * number_in_year_2/number_in_year_1_and_2
    
  } else if(Per > per){
    
    lots_year_1 <- data %>% filter(Year == Yearr - 1) %>% filter(Month < 10) %>% filter(Neighborhood == Nei) %>% pull(BBL)
    lots_year_2 <- data %>% filter(Year == Yearr) %>% filter(Neighborhood == Nei) %>% pull(BBL)
    lots_year_1Zop <- data %>% filter(Year == Yearr - 1) %>% filter(Month < 10) %>% filter(zip == Zop) %>% pull(BBL)
    number_in_year_1 <- length(unique(lots_year_1))
    number_in_year_1Zop <- length(unique(lots_year_1Zop))
    number_in_year_2 <- length(unique(lots_year_2))
    number_in_year_1_and_2 <- sum(unique(lots_year_1) %in% unique(lots_year_2))
    
    50 * number_in_year_1Zop * number_in_year_2/number_in_year_1_and_2
  }
  
}


#Writing a file with the values
df2 <-
  data %>% select(Year, zip, Borough, Neighborhood, Percentage) %>% group_by(Year, zip, Borough, Neighborhood, Percentage) %>%
  summarize(count = n()) %>% select(Year, zip, Borough, Neighborhood, Percentage)

df3 <- df2 %>% mutate(Number = mapply(capture_recaptureDynamic, Yearr = Year, Zop = zip, Nei = Neighborhood, Per = Percentage))

write.csv(df3, file="C:\\Users\\Icaro\\Documents\\ratsCompleteNeighborhoodBuff.csv", row.names = FALSE)


#Getting one file for each year
df3 <- read_csv("ratsCompleteNeighborhoodBuff.csv")


df11 <- df3 %>% filter(Year == 11)
df12 <- df3 %>% filter(Year == 12)
df13 <- df3 %>% filter(Year == 13)
df14 <- df3 %>% filter(Year == 14)
df15 <- df3 %>% filter(Year == 15)
df16 <- df3 %>% filter(Year == 16)
df17 <- df3 %>% filter(Year == 17)
df18 <- df3 %>% filter(Year == 18)
df19 <- df3 %>% filter(Year == 19)
df20 <- df3 %>% filter(Year == 20)
df21 <- df3 %>% filter(Year == 21)


write.csv(df11, file="C:\\Users\\Icaro\\Documents\\ratsCompleteNeighborhoodBuff11.csv", row.names = FALSE)
write.csv(df12, file="C:\\Users\\Icaro\\Documents\\ratsCompleteNeighborhoodBuff12.csv", row.names = FALSE)
write.csv(df13, file="C:\\Users\\Icaro\\Documents\\ratsCompleteNeighborhoodBuff13.csv", row.names = FALSE)
write.csv(df14, file="C:\\Users\\Icaro\\Documents\\ratsCompleteNeighborhoodBuff14.csv", row.names = FALSE)
write.csv(df15, file="C:\\Users\\Icaro\\Documents\\ratsCompleteNeighborhoodBuff15.csv", row.names = FALSE)
write.csv(df16, file="C:\\Users\\Icaro\\Documents\\ratsCompleteNeighborhoodBuff16.csv", row.names = FALSE)
write.csv(df17, file="C:\\Users\\Icaro\\Documents\\ratsCompleteNeighborhoodBuff17.csv", row.names = FALSE)
write.csv(df18, file="C:\\Users\\Icaro\\Documents\\ratsCompleteNeighborhoodBuff18.csv", row.names = FALSE)
write.csv(df19, file="C:\\Users\\Icaro\\Documents\\ratsCompleteNeighborhoodBuff19.csv", row.names = FALSE)
write.csv(df20, file="C:\\Users\\Icaro\\Documents\\ratsCompleteNeighborhoodBuff20.csv", row.names = FALSE)
write.csv(df21, file="C:\\Users\\Icaro\\Documents\\ratsCompleteNeighborhoodBuff21.csv", row.names = FALSE)
