library("readr")
library("dplyr")

#Importing data
divvy_202209 <- read_csv("~/divvy-tripdata/202209-divvy-publictripdata.csv")

#Checking data dimension
dim(divvy_202209)

#Checking columns names
colnames(divvy_202209)

#Selecting columns
divvy_202209 <- divvy_202209 %>% 
  select(c(ride_id, rideable_type, started_at,ended_at,member_casual))

#Inspecting new table
View(divvy_202209)
nrow(divvy_202209)
dim(divvy_202209)
str(divvy_202209)

#Make new column for ride length
divvy_202209$ride_length <- difftime(divvy_202209$ended_at,divvy_202209$started_at, units="mins")
divvy_202209$ride_length <- as.double(divvy_202209$ride_length)

#Make new column for bike usage in Date, Year, Month, Day, Day of the week 
divvy_202209$Date <- as.Date(divvy_202209$started_at)
divvy_202209$Year <- format(as.Date(divvy_202209$Date), "%Y")
divvy_202209$Month <- format(as.Date(divvy_202209$Date), "%m")
divvy_202209$Day <- format(as.Date(divvy_202209$Date), "%d")
divvy_202209$Day_of_week <- format(as.Date(divvy_202209$Date), "%A")

#Remove the row with negative value of ride length
divvy_202209_v2 <- divvy_202209[!divvy_202209$ride_length<0,]

dim(divvy_202209_v2)

#Summary of ride length
summary(divvy_202209_v2$ride_length)

#Boxplot ofride length
boxplot(divvy_202209_v2$ride_length)

#Remove outliers
Q1 <- quantile(divvy_202209_v2$ride_length, .25)
Q3 <- quantile(divvy_202209_v2$ride_length, .75)
IQR <- IQR(divvy_202209_v2$ride_length)

divvy_202209_v3 <- subset(divvy_202209_v2, nrdivvy_202209_v2$ride_length > (Q1 - 1.5*IQR) & divvy_202209_v2$ride_length < (Q3 + 1.5*IQR))
View(divvy_202209_v3)
#Checking bike type
distinct(divvy_202209_v2,rideable_type)

#Checking member type
distinct(divvy_202209_v2, member_casual)

#Compare member and casual user in ride length
aggregate(divvy_202209_v2$ride_length ~ divvy_202209_v2$member_casual, FUN = mean)
aggregate(divvy_202209_v2$ride_length ~ divvy_202209_v2$member_casual, FUN = min)
aggregate(divvy_202209_v2$ride_length ~ divvy_202209_v2$member_casual, FUN = max)
aggregate(divvy_202209_v2$ride_length ~ divvy_202209_v2$member_casual, FUN = median)

#Compare member and casual user with rideable type
library("janitor")
tabyl(divvy_202209_v2,member_casual,rideable_type)

#Compare member and casual user in ride length each day
aggregate(divvy_202209_v2$ride_length ~ divvy_202209_v2$member_casual + divvy_202209_v2$Day_of_week, FUN =mean)

#Ordering day
divvy_202209_v2$Day_of_week <- ordered(divvy_202209_v2$Day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday","Sunday"))

#Analyze number of rides on weekday by user type 
divvy_202209_v2 %>%
  mutate(weekday = lubridate::wday(Date, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday, rideable_type) %>%  #groups by usertype and weekday'
  summarise(number_of_rides = n(), #calculates the number of rides and average duration
            average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(member_casual, weekday) %>% # sorts
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Number of Rides on Weekday", x="Day", y="Number of Rides", fill="User type") +
  theme(plot.title =element_text(hjust = 0.5)) 

#Analyze number of rides on weekday by user type and bike type
divvy_202209_v2 %>%
  mutate(weekday = lubridate::wday(Date, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday, rideable_type) %>%  #groups by usertype and weekday'
  summarise(number_of_rides = n(), #calculates the number of rides and average duration
            average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(member_casual, weekday) %>% # sorts
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Number of Rides on Weekday", x="Day", y="Number of Rides", fill="User type") +
  theme(plot.title =element_text(hjust = 0.5)) +
  facet_wrap("rideable_type",ncol=2,nrow =2)

#Analyze average duration on weekday by user type 
divvy_202209_v2 %>%
  mutate(weekday = lubridate::wday(Date, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday'
  summarise(number_of_rides = n(), #calculates the number of rides and average duration
            average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(member_casual, weekday) %>% # sorts
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Average Duration on Weekday", x="Day", y="Average Duration (in secs)", fill="User type") +
  theme(plot.title =element_text(hjust = 0.5))

#Analyze average duration on weekday by user type 
divvy_202209_v2 %>%
  mutate(weekday = lubridate::wday(Date, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday, rideable_type) %>%  #groups by usertype and weekday'
  summarise(number_of_rides = n(), #calculates the number of rides and average duration
            average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(member_casual, weekday) %>% # sorts
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Average Duration on Weekday", x="Day", y="Average Duration (in secs)", fill="User type") +
  theme(plot.title =element_text(hjust = 0.5)) +
  facet_wrap("rideable_type",ncol=2,nrow =2)
