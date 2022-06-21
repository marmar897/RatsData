###########################################
# Analysis of 311 Calls for Rat Sightings #
###########################################

# This script estimates (very roughly) the NYC rat population from 311 Calls for "Rat Sightings"
# For more detailed analysis see https://rss.onlinelibrary.wiley.com/doi/full/10.1111/j.1740-9713.2014.00764.x

# Author: Jonathan Auerbach
# Contact: jauerba@gmu.edu
# Date: 6/5/2022

library("tidyverse")


# "nyc_rodent_compliants.csv" contains Service Requests Accessed 6/5/2022 from NYC Open Data Portal
## For full data see 
## https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9
## n.b. filtered: Complaint Type == "Rodent" 
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

# Estimate the number of lots with rats in 2011?
lots_year_1 <- df %>% filter(Year == "10") %>% pull(BBL)
lots_year_2 <- df %>% filter(Year == "11") %>% pull(BBL)
number_in_year_1 <- length(lots_year_1)
number_in_year_2 <- length(lots_year_2)
number_in_year_1_and_2 <- sum(lots_year_1 %in% lots_year_2)
  
number_in_year_1 * number_in_year_2 / number_in_year_1_and_2

# Estimated number of rats if each lot has 50 rats
50 * number_in_year_1 * number_in_year_2 / number_in_year_1_and_2

# Function to estimate number of rats in any year (%y between 11 and 21) 
capture_recapture <- function(year) {
  lots_year_1 <- df %>% filter(Year == year - 1) %>% pull(BBL)
  lots_year_2 <- df %>% filter(Year == year) %>% pull(BBL)
  number_in_year_1 <- length(lots_year_1)
  number_in_year_2 <- length(lots_year_2)
  number_in_year_1_and_2 <- sum(lots_year_1 %in% lots_year_2)
  50 * number_in_year_1 * number_in_year_2 / number_in_year_1_and_2
}

# simple visualization of estimated rat population
qplot(x = factor(2011:2021),
      weight = sapply(11:21, capture_recapture),
      geom = "bar")  +
  labs(x = "year",
       y = "estimated rat population") +
  theme_bw()

#Mariana's edits to R script for more graphs, plots and get familiar
# R, Rstudio and data :)


