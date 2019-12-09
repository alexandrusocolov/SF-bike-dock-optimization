library(dplyr)
library(lubridate)
library(fastDummies)

data <- read.csv("/Users/alexandru/Dropbox (MIT)/Machine Learning Project/status_station_weather.csv")
data$date <- ymd(data$date)
data_last_month <- data[data$date >= "2015-08-01", ]
dim(data_last_month)

zipcodes = unique(data_last_month$zip_code)

station_in_zipcode <- unique(data_last_month[, c("station_id", "zip_code")])
station_in_zipcode$zip_code <- factor(station_in_zipcode$zip_code)
station_in_zipcode <- dummy_cols(station_in_zipcode)
station_in_zipcode <- station_in_zipcode[, c(1, 2, 7, 4, 3, 6, 5)]

population_per_zipcode <- unique(data_last_month[, c("population")])


write.csv(station_in_zipcode, '/Users/alexandru/Dropbox (MIT)/Machine Learning Project/station_in_zipcode.csv')
write.csv(population_per_zipcode, '/Users/alexandru/Dropbox (MIT)/Machine Learning Project/population_per_zipcode.csv')


