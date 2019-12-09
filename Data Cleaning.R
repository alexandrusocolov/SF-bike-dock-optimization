getwd()
setwd("/Users/alexandru/Documents/MIT/Academics/15.095 Machine Learning/Project/Data")

library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

station <- read.csv("station.csv")
# status <- read.csv("status.csv")   #largest file, load in only if needed
trip <- read.csv("trip.csv")
weather <- read.csv("weather.csv")

#subsetting status
status$time <- ymd_hms(status$time)
status$date <- as.Date(status$time)


status_daily <- status %>% 
  group_by(date, station_id) %>% 
  summarize(avg_bikes_available = mean(bikes_available),
            avg_docks_available = mean(docks_available))
write.csv(status_daily, "status_daily.csv")

status <- read.csv("status_daily.csv")

#converting lat long coordinates of stations into zipcodes to be able to join with the weather data
library(revgeo)

station$zipcode = revgeo(longitude = station$long, 
                         latitude = station$lat, provider = "photon", output="frame")$zip

library(plyr)
station$zipcode <- revalue(station$zipcode, c("95112-5005"="95112", "94304-1050" = "94304"))
station <- station %>%
  mutate(zip_code = ifelse(
    zipcode %in% c("94108", "914105", "94103", "94102", "94113", "94105", "94158", "94107"), "94107", 
    ifelse(zipcode %in% c("94061", "94063"), "94063", 
           ifelse(zipcode %in% c("94304", "94303", "94301"), "94301", 
                  ifelse(zipcode %in% c("94043", "94040", "94041"), "94041", "95113")))))


#left joining station with status_hourly
status_station <- status %>% left_join(station[, c("id", "zip_code", "installation_date", "dock_count")], 
                                       by = c("station_id" = "id")) %>% group_by("station_id", "date")
  
#left joining status_station with weather 
glimpse(weather)
glimpse(status_station)

weather$date <- mdy(weather$date)
weather$zip_code <- as.character(weather$zip_code)
status_station$date <- ymd(status_station$date)

pop <- data.frame(zip_code = c("95113", "94063", "94041", "94301", "94107"), 
                  population = c(893, 30949, 13292, 16995, 805235))

status_station_weather <- status_station %>% 
  left_join(weather, by = c("date" = "date", "zip_code" = "zip_code")) %>% 
  left_join(pop)

full_date <- data.frame(station_id = rep(station$id, each = 733), date = rep(seq(as.Date("2013-08-29"), as.Date("2015-08-31"), "days"), 70))
status_station_weather <- full_date %>% left_join(status_station_weather, 
                                by = c("date" = "date", "station_id" = "station_id"))

status_station_weather <- status_station_weather %>% arrange(station_id, date) %>% group_by(station_id) %>%
  mutate(avg_docks_1D = lag(avg_docks_available,1L,default=0),
         avg_docks_7D = lag(avg_docks_available,7L,default=0),
         avg_docks_30D = lag(avg_docks_available,30L,default=0))

status_station_weather$avg_docks_1D <- ifelse(status_station_weather$avg_docks_1D == 0, NA, status_station_weather$avg_docks_1D)
status_station_weather$avg_docks_7D <- ifelse(status_station_weather$avg_docks_7D == 0, NA, status_station_weather$avg_docks_7D)
status_station_weather$avg_docks_30D <- ifelse(status_station_weather$avg_docks_30D == 0, NA, status_station_weather$avg_docks_30D)

write.csv(status_station_weather, "status_station_weather.csv")

status_station_weather_NAremoved <- status_station_weather[!is.na(status_station_weather$avg_bikes_available),]
write.csv(status_station_weather_NAremoved, "status_station_weather_NAremoved.csv")
