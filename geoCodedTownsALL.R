library(ggmap)
library(dplyr)
library(tidyr)
library(rgdal)
library(ggplot2)
library(USAboundaries)
library(stringr)

## Import and clean data

setwd(##setwd here)
mapdata <- read.csv (##download NNV data set and direct file here,
                     stringsAsFactors = FALSE)

head(mapdata)
mapdata <- mutate(mapdata, locale = str_c(mapdata$State, mapdata$Territory))
towns <- str_c(mapdata$Town, mapdata$locale, sep= ", ")
allT <- (str_trim(unique(sort(towns), stringsAsFactors=FALSE)))
##remove first 28 towns bc they are false positives
allTowns <- allT[33:3059]

##bring all towns into a data frame and geocode. Note: due to limits of Google's geocode API this must
## be spread out over the course of two days.

firstTowns <- data.frame (towns = c (allT[33:2000]),stringsAsFactors = FALSE)
geocodedTownOne <- geocode(firstTowns$towns)

secondTowns <- data.frame (towns = c (allT[2001:3059]),stringsAsFactors = FALSE)
geocodedTownTwo <- geocode(secondTowns$towns)

firstTowns <- cbind(firstTowns, geocodedTownOne)
secondTowns <- cbind(secondTowns, geocodedTownTwo)

allTowns <- rbind(firstTowns, secondTowns)

##write.csv(allTowns, file= "allTowns.csv")

##perform the same operation with the 36 cities

cities <- str_c(mapdata$City, mapdata$locale, sep= ", ")
allC <- (str_trim(unique(sort(cities), stringsAsFactors=FALSE)))
##remove first 32 bc false positives
allCities <- allC[33:68]

##geocode cities using Google's api. Note: header named 'towns' for easy binding to allTowns

allCities <- data.frame (towns = c (allC[33:68]),stringsAsFactors = FALSE)
geocodedCities <- geocode(allCities$towns)
allCities <- cbind(allCities, geocodedCities)

##write.csv(allCities, file= "allCities.csv")

allLocations <- rbind(allTowns, allCities)

allLocations <- filter(allLocations, lon >= -95)
allLocations <- filter(allLocations, lon <= -50)
allLocations <- filter(allLocations, lat >= 20)

##write.csv(allLocations, file= "allLocations.csv")

allLocations <- read.csv ("allLocations.csv",
                      stringsAsFactors = FALSE)

##merge allLocations to all-votes.csv

str(mapdata)
mapdata <- mutate(mapdata, 
                  location = str_c(mapdata$Town, mapdata$City))
mapdata <-  mutate(mapdata, 
                location = str_c(mapdata$location, mapdata$locale, sep= ", "))

str(allLocations)
names(allLocations) <- c("x", "location", "lon", "lat")
str(allLocations)

mapdata_merged <- left_join(mapdata, allLocations, by = c("location" = "location"))
str(mapdata_merged)

##write.csv(mapdata_merged, file= "mapdata_merged.csv")

## check # of geocoding queries remaining
geocodeQueryCheck(userType = "free")

## Map Code
USA <- c("Alabama", "Arkansas", "Connecticut", "Delaware", "Florida Territory", "Georgia",
         "Illinois", "Indiana", "Kentucky", "Louisiana", "Maine", "Maryland", 
         "Massachusetts", "Michigan",  "Mississippi", "Missouri", "New Hampshire", 
         "New Jersey", "New York", "North Carolina", "Northwest Territory", "Ohio", "Orleans Territory",
         "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Vermont","Virginia")  
map <- us_boundaries(as.Date("1825-03-15"), type = "county", state = USA)
usMap <- ggplot() +  geom_polygon(data=map, aes(x=long, y=lat, group=group))
usMap +
  ggtitle("County Boundaries on March 15, 1825") +
  geom_text(data = allLocations, aes(x = lon, y = lat, label = location), 
            color="gray",
            vjust = -1) +
  geom_point(data = allLocations, aes(x = lon, y = lat), color= "red") +
  theme(legend.position = "bottom" ) +
  theme_minimal()
