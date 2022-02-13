# Cyclistic
Exploratory data analysis for the Cyclistic clients data

# Installing packages
install.packages("dplyr")
install.packages("readr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")                     
library(lubridate)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Setting working directory
setwd("C:/Users/Owner/Documents/Data Analyst/Data/Cyclistic/10_11_12")

# Import data
Cyc_data<-read_csv("Last three months.csv")

# Change data type to date
new_start<-strptime(Cyc_data$started_at, "%F %R")
class(new_start)

new_end<-strptime(Cyc_data$ended_at, "%F %R")
class(new_end)

# Create duration column
cyc_data_dur <- Cyc_data %>%
                select(rideable_type, started_at, ended_at, member_casual) %>%
                mutate(started_at = as.POSIXct(started_at)) %>%
                mutate(ended_at = as.POSIXct(ended_at))%>%
                mutate(duration = ended_at - started_at)

# Clean data for analysis
# Inspect the new table that has been created
colnames(cyc_data_dur)
nrow(cyc_data_dur)
dim(cyc_data_dur)
str(cyc_data_dur)
summary(cyc_data_dur)

levels(as.factor(cyc_data_dur$member_casual))

# Add columns that list the date, month, day, and year of each ride
This will allow us to aggregate ride data for each month, day, or year ... before 
completing these operations we could only aggregate at the ride level

cyc_data_dur$date <- as.Date(cyc_data_dur$started_at) #The default format is yyyy-mm-dd
cyc_data_dur$month <- format(as.Date(cyc_data_dur$date), "%m")
cyc_data_dur$day <- format(as.Date(cyc_data_dur$date), "%d")
cyc_data_dur$year <- format(as.Date(cyc_data_dur$date), "%Y")
cyc_data_dur$day_of_week <- format(as.Date(cyc_data_dur$date), "%A")


# Inspect the structure of the columns
str(cyc_data_dur)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.numeric(cyc_data_dur$duration)
cyc_data_dur$duration <- as.numeric(as.character(cyc_data_dur$duration))
is.numeric(cyc_data_dur$duration)

# Remove "bad data
The dataframe includes a few hundred entries when bikes were taken out of docks and checked
for quality by Divvy or ride_length was negative
Create a new version of the dataframe (v2) since data is being removed



# Conduct Descriptive Analysis
# Descriptive analysis on ride duration (all figures in seconds)
summary(cyc_data_v2$duration)

# Compare members and casual users
aggregate(cyc_data_v2$duration ~ cyc_data_v2$member_casual, FUN = mean)
aggregate(cyc_data_v2$duration ~ cyc_data_v2$member_casual, FUN = median)
aggregate(cyc_data_v2$duration ~ cyc_data_v2$member_casual, FUN = max)
aggregate(cyc_data_v2$duration ~ cyc_data_v2$member_casual, FUN = min)


# See the average ride time by each day for members vs casual users
aggregate(cyc_data_v2$duration ~ cyc_data_v2$member_casual + cyc_data_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
cyc_data_v2$day_of_week <- ordered(cyc_data_v2$day_of_week, 
                                    levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(cyc_data_v2$duration ~ cyc_data_v2$member_casual + cyc_data_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
cyc_data_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(duration)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
cyc_data_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + labs(title = "Number of Rides by Rider Type")

# Let's create a visualization for average duration
cyc_data_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")  + labs(title = "Average Duration of Ride")


# Let's visualize the number of rides by bike type
cyc_data_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(rideable_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration)) %>% 
  arrange(rideable_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge") + labs(title = "Number of Rides by Bike Type")


nrow(cyc_data_v2)


#  EXPORT SUMMARY FILE FOR FURTHER ANALYSIS

# Create a csv file that we will visualize in Excel, Tableau, or my presentation software


write.csv(cyc_data_v2, 'Cyclistic.csv')
