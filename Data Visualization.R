library(readr)
library(tidyverse)
library(ggmap)
library(lubridate)
library(zipcode)

dock_allocation_WITH_fairness <- read.csv("dock_allocation_WITH_fairness.csv", header = FALSE)
list_changing_min_docks <- read.csv("list_changing_min_docks.csv", header = FALSE)
list_changing_lambda <- read.csv("list_changing_lambda.csv", header = FALSE)


list_changing_lambda <- list_changing_lambda %>% rename(lambda = V1, 
                                                        number_of_bikes_transported = V2, 
                                                        number_of_stations_affected = V3)
ggplot(list_changing_lambda) + 
  geom_line(aes(x = lambda, y = number_of_bikes_transported), color = "blue", size = 1) +
  geom_line(aes(x = lambda, y = number_of_stations_affected*5), color = "red", size = 1) + 
  scale_y_continuous(sec.axis = sec_axis(~./5, name = "Number of Stations Affected")) +
  labs(y = "Number of Bikes Transported", x = "Lambda") + 
  theme_light() + 
  theme(axis.title.y.left = element_text(color = "blue", size = 15),
        axis.text.y.left = element_text(color = "blue", size = 15),
        axis.title.y.right = element_text(color = "red", size =15),
        axis.text.y.right = element_text(color = "red", size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 15))


list_changing_min_docks <- list_changing_min_docks %>% rename(minimum_number_required = V1, 
                                                           number_of_bikes_transported = V2, 
                                                           number_of_stations_affected = V3)

ggplot(list_changing_min_docks) + 
  geom_line(aes(x = minimum_number_required, y = number_of_bikes_transported), color = "blue", size = 1) +
  geom_line(aes(x = minimum_number_required, y = number_of_stations_affected*5), color = "red", size = 1) + 
  scale_y_continuous(sec.axis = sec_axis(~./5, name = "Number of Stations Affected")) +
  labs(y = "Number of Bikes Transported", x = "Minimum Number of Docks Required") + 
  theme_light() + 
  theme(axis.title.y.left = element_text(color = "blue", size = 15),
        axis.text.y.left = element_text(color = "blue", size = 15),
        axis.title.y.right = element_text(color = "red", size =15),
        axis.text.y.right = element_text(color = "red", size = 15),
        axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 15))


status_station_weather <- read.csv("status_station_weather_NAremoved.csv")
station <- read.csv("station.csv")
viz <- status_station_weather %>% mutate(year = year(date)) %>%
  group_by(station_id, dock_count, zip_code) %>% 
  summarise(worstcase_docks_available = min(avg_docks_available, na.rm = TRUE),
            avg_docks_available = mean(avg_docks_available, na.rm = TRUE)) %>% 
  left_join(station[,c(1,3,4)], by = c("station_id" = "id"))

viz_94107 <- viz[viz$zip_code == "94107",]
dock_allocation_WITH_fairness <- dock_allocation_WITH_fairness %>% 
  rename(num_docks_moved_withF = V1)
viz_94107 <- viz_94107 %>% merge(dock_allocation_WITH_fairness, by = 0)
viz_94107 <- viz_94107 %>% mutate(new_docks_count = dock_count + num_docks_moved_withF,
                                  color = ifelse(num_docks_moved_withF > 0, "red", "blue"))

coords <- c(left = -122.425, 
            bottom = 37.765, 
            right = -122.375, 
            top = 37.81)


library(gridExtra)
basemap <- get_map(location = coords,
                   maptype = 'roadmap')
plot1  <- ggmap(basemap) + 
  geom_point(aes(x = long, y = lat, size = dock_count), 
             data = viz_94107, color = "blue", alpha = 0.7) +
  theme(legend.position = "none") +
  xlab("Longitude") +
  ylab("Latitude")

plot2 <- ggmap(basemap) + 
  geom_point(aes(x = long, y = lat, size = new_docks_count, color = color), 
             data = viz_94107, alpha = 0.7) + 
  scale_color_manual(values = c("blue", "red"), name = "", labels = c("not changed", "changed")) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = "none") 

grid.arrange(plot1, plot2, ncol=2)







#####
viz_95113 <- viz[viz$zip_code == "95113",]

coords <- c(left = -121.91, 
            bottom = 37.32, 
            right = -121.87, 
            top = 37.36)

basemap <- get_map(location = coords,
                   maptype = 'roadmap')

ggmap(basemap) + 
  geom_point(aes(x = long, y = lat, color = dock_count), 
             data = viz_95113, size = 2)


                                                                                                   + )
