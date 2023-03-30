setwd("C:/Users/Abhimanyu/Desktop/Google Case Study")

library("dplyr")
library("lubridate")
library("ggplot2")
#Reading all the 12 files

rides202203 = read.csv("C:/Users/Abhimanyu/Desktop/Google Case Study/202203-divvy-tripdata/202203-divvy-tripdata.csv")
rides202204 = read.csv("C:/Users/Abhimanyu/Desktop/Google Case Study/202204-divvy-tripdata/202204-divvy-tripdata.csv")
rides202205 = read.csv("C:/Users/Abhimanyu/Desktop/Google Case Study/202205-divvy-tripdata/202205-divvy-tripdata.csv")
rides202206 = read.csv("C:/Users/Abhimanyu/Desktop/Google Case Study/202206-divvy-tripdata/202206-divvy-tripdata.csv")
rides202207 = read.csv("C:/Users/Abhimanyu/Desktop/Google Case Study/202207-divvy-tripdata/202207-divvy-tripdata.csv")
rides202208 = read.csv("C:/Users/Abhimanyu/Desktop/Google Case Study/202208-divvy-tripdata/202208-divvy-tripdata.csv")
rides202209 = read.csv("C:/Users/Abhimanyu/Desktop/Google Case Study/202209-divvy-tripdata/202209-divvy-publictripdata.csv")
rides202210 = read.csv("C:/Users/Abhimanyu/Desktop/Google Case Study/202210-divvy-tripdata/202210-divvy-tripdata.csv")
rides202211 = read.csv("C:/Users/Abhimanyu/Desktop/Google Case Study/202211-divvy-tripdata/202211-divvy-tripdata.csv")
rides202212 = read.csv("C:/Users/Abhimanyu/Desktop/Google Case Study/202212-divvy-tripdata/202212-divvy-tripdata.csv")
rides202301 = read.csv("C:/Users/Abhimanyu/Desktop/Google Case Study/202301-divvy-tripdata/202301-divvy-tripdata.csv")
rides202302 = read.csv("C:/Users/Abhimanyu/Desktop/Google Case Study/202302-divvy-tripdata/202302-divvy-tripdata.csv")

#Combining all 2022 rides into one dataframe
rides2022 = rbind(rides202203,rides202204,rides202205,rides202206,rides202207,rides202208,rides202209,rides202210,rides202211,rides202212)

#Combining all 2023 rides into one dataframe
rides2023 = rbind(rides202301,rides202302)

#Combining the entire data into a single dataframe called rides
rides = rbind(rides2022,rides2023)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
rides <- rides %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(rides)  #List of column names
nrow(rides)  #How many rows are in data frame?
dim(rides)  #Dimensions of the data frame?
head(rides)  #See the first 6 rows of data frame.  Also tail(rides)
str(rides)  #See list of columns and data types (numeric, character, etc)
summary(rides)  #Statistical summary of data. Mainly for numerics



#Counting the number of null values in each dataframe
sum(is.na(rides2022)) #11390 nulls
sum(is.na(rides2023)) #486 nulls
sum(is.na(rides)) #11876 nulls

summary(rides2022)
#The end_lat and end_lng variables have 5695 missing values in each column for 2022
summary(rides2023)
#The end_lat and end_lng variables have 243 missing values in each column for 2023

#Cleaning and manipulating data
rides$Date <- as.Date(rides$started_at, format="%d-%m-%Y")
rides$Month <- format(as.Date(rides$Date),"%m")
rides$Day <- format(as.Date(rides$Date), "%d")
rides$Year <- format(as.Date(rides$Date),"%Y")
rides$ride_length <- difftime(as.POSIXct(rides$ended_at,format="%d-%m-%Y %H:%M"),as.POSIXct(rides$started_at,format="%d-%m-%Y %H:%M"))



# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(rides$ride_length)
rides$ride_length <- as.numeric(as.character(rides$ride_length))
is.numeric(rides$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
rides_v2 <- rides[!(rides$start_station_name == "HQ QR" | rides$ride_length<0),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(rides_v2$ride_length) #straight average (total ride length / rides)
median(rides_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(rides_v2$ride_length) #longest ride
min(rides_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(rides_v2$ride_length)

#Minimum ride length is 0 seconds
#Maximum ride length is 2483220 seconds (41387 minutes, seems too long)
#Median ride length is 600 seconds (10 minutes)
#Mean ride length is 1153 seconds (19.21 minutes)

# Compare members and casual users
aggregate(rides_v2$ride_length ~ rides_v2$member_casual, FUN = mean)
aggregate(rides_v2$ride_length ~ rides_v2$member_casual, FUN = median)
aggregate(rides_v2$ride_length ~ rides_v2$member_casual, FUN = max)
aggregate(rides_v2$ride_length ~ rides_v2$member_casual, FUN = min)

#Mean ride length for casual users is 1736 seconds, as opposed to 754 seconds for members
#Median ride length for casual users is 780 seconds, as opposed to 540 seconds for members
#Max ride legnth for casual users is 2483220 seconds, as opposed to 90000 seconds for members
#Min ride lnegth for both member types is 0 seconds

# See the average ride time by each day for members vs casual users
aggregate(rides_v2$ride_length ~ rides_v2$member_casual + rides_v2$day_of_week, FUN = mean)

#The average ride time is higher for casual users on each day

# Notice that the days of the week are out of order. Let's fix that.
rides_v2$day_of_week <- recode(rides_v2$day_of_week, "1"="Sunday","2"="Monday","3"="Tuesday","4"="Wednesday","5"="Thursday","6"="Friday","7"="Saturday")

# Now, let's run the average ride time by each day for members vs casual users
aggregate(rides_v2$ride_length ~ rides_v2$member_casual + rides_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#Casual users use the bike for a much higher average duration
#The variance in the average_duration is also high for casual compared to members

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
counts <- aggregate(rides_v2$ride_length ~ rides_v2$member_casual + rides_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')

#Exporting the aggregated dataframe with last 12 months' data to a csv file
write.csv(rides_v2,"Last 12 months rides.csv")
