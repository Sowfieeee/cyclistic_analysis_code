#this installs packages
install.packages("tidyverse")

#this loads libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(data.table)
library(mutate)

#loads csv. files
May_2022 <- read_csv("C:/Users/SOPHIA/Desktop/Trip Data May 2022 - April 2023/202205-divvy-tripdata.csv")
June_2022 <- read_csv("C:/Users/SOPHIA/Desktop/Trip Data May 2022 - April 2023/202206-divvy-tripdata.csv")
July_2022 <- read_csv("C:/Users/SOPHIA/Desktop/Trip Data May 2022 - April 2023/202207-divvy-tripdata.csv")
August_2022 <- read_csv("C:/Users/SOPHIA/Desktop/Trip Data May 2022 - April 2023/202208-divvy-tripdata.csv")
September_2022 <- read_csv("C:/Users/SOPHIA/Desktop/Trip Data May 2022 - April 2023/202209-divvy-tripdata.csv")
October_2022 <- read_csv("C:/Users/SOPHIA/Desktop/Trip Data May 2022 - April 2023/202210-divvy-tripdata.csv")
November_2022 <- read_csv("C:/Users/SOPHIA/Desktop/Trip Data May 2022 - April 2023/202211-divvy-tripdata.csv")
December_2022 <- read_csv("C:/Users/SOPHIA/Desktop/Trip Data May 2022 - April 2023/202212-divvy-tripdata.csv")
January_2023 <- read_csv("C:/Users/SOPHIA/Desktop/Trip Data May 2022 - April 2023/202301-divvy-tripdata.csv")
February_2023 <- read_csv("C:/Users/SOPHIA/Desktop/Trip Data May 2022 - April 2023/202302-divvy-tripdata.csv")
March_2023 <- read_csv("C:/Users/SOPHIA/Desktop/Trip Data May 2022 - April 2023/202303-divvy-tripdata.csv")
April_2023 <- read_csv("C:/Users/SOPHIA/Desktop/Trip Data May 2022 - April 2023/202304-divvy-tripdata.csv")

#this compares the column names
compare_df_cols(May_2022,June_2022,July_2022,August_2022,September_2022,October_2022,November_2022,December_2022,January_2023,February_2023,March_2023,April_2023)

#this merges all of the data frames into a year view
all_trips <- rbind(May_2022,June_2022,July_2022,August_2022,September_2022,October_2022,November_2022,December_2022,January_2023,February_2023,March_2023,April_2023)
str(all_trips)

#this removes columns not needed for my analysis
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

#checking the summary and other details
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
tail(all_trips)
str(all_trips)
summary(all_trips)

#Removing bad or duplicates data and setting another name for data frame
all_trips_clean <- all_trips[!duplicated(all_trips$ride_id),]
View(all_trips_clean)

#character to Date-time conversion
all_trips_clean$started_at = strptime(all_trips_clean$started_at,"%Y-%m-%d %H:%M:%S")
all_trips_clean$ended_at = strptime(all_trips_clean$ended_at,"%Y-%m-%d %H:%M:%S")
str(all_trips_clean)

#this creates and formats columns
all_trips_clean$date <- as.Date(all_trips_clean$started_at)
all_trips_clean$month <- format(as.Date(all_trips_clean$date), "%m")#creates column for month
all_trips_clean$day <- format(as.Date(all_trips_clean$date), "%d")#creates column for day
all_trips_clean$year <- format(as.Date(all_trips_clean$date), "%Y")#creates column for year
all_trips_clean$day_of_week <- format(as.Date(all_trips_clean$date), "%A")#creates column for day of week

#this creates new column for ride length
all_trips_clean$ride_length <- difftime(all_trips_clean$ended_at,all_trips$started_at)
str(all_trips_clean)

#converts data type to numeric
all_trips_clean$ride_length <- as.numeric(as.character(all_trips_clean$ride_length))
is.numeric(all_trips_clean$ride_length)

#remove unnecessary data in the “ride_length”column and setting another name for data frame
all_trips_v2 <- all_trips_clean[!(all_trips_clean$start_station_name == "HQ QR" | all_trips_clean$ride_length<0),]
View(all_trips_v2)

#inspect new data frame
summary(all_trips_v2)

#remove all rows with missing data(NA) and set another name for data frame
all_trips_v3<-na.omit(all_trips_v2)
summary(all_trips_v3)
view(all_trips_v3)

#view statistical summary of ride_length
summary(all_trips_v3$ride_length)

#export data frame for further analysis on Tableau
write.csv(all_trips_v3, "all_trips_v3.csv")

#group by day_of_week and set new name for further analysis on R
riders <- all_trips_v3 %>% group_by(day_of_week)
summary(riders)
view(riders)

#this is to see the average ride time by each day for members vs casual users
aggregate(riders$ride_length ~ riders$member_casual + riders$day_of_week, FUN = mean)
#order by day of week starting from Monday
riders$day_of_week <- ordered(riders$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


#percentage and count of riders Casual vs Members
riders %>%
  group_by(member_casual) %>%
  summarise(count = length(ride_length),
            "%" = (length(ride_id)/nrow(riders))*100)

#total rides by customer type
ggplot(riders, aes(member_casual, fill = member_casual)) + 
  geom_bar() + labs(title = "Total rides by Customer Type", x = "Customer Type")+
  scale_fill_manual("legend", values = c("casual" = "orange", "member" = "blue"))

#average duration by customer type
riders %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = member_casual, y = average_duration, fill = member_casual)) +
  labs(title = "Average Duration by Customer Type", x = "Customer Type")+
  scale_fill_manual("legend", values = c("casual" = "orange", "member" = "blue"))+ geom_col(position = "dodge")
 
#total rides per customer type by day of week
riders %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups="drop") %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides per Customer Type by Day of week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  scale_fill_manual("legend", values = c("casual" = "orange", "member" = "blue"))+ 
  geom_col(position = "dodge")

#average trip duration per customer type
riders %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(average_trip_duration = mean(ride_length)) %>%
  ggplot(aes(x = day_of_week, y = average_trip_duration, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration per Customer type by Day of week") +
  scale_fill_manual("legend", values = c("casual" = "orange", "member" = "blue"))+ 
  geom_col(position = "dodge")

#total rides by bike type
riders %>%
  ggplot(aes(rideable_type, fill = member_casual)) +
  geom_bar()+
  labs(x="Bike Type", title= "Total rides by Bike Type")+
  scale_fill_manual("legend", values = c("casual" = "orange", "member" = "blue"))

#most used start location
riders %>%
  group_by(start_station_name, member_casual) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  filter(start_station_name != "", member_casual != 'member') %>%
  arrange(-number_of_ride) %>%
  head(n=10) %>%
  select(-member_casual) 
            