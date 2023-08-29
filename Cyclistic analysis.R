#### Previous_Twelve_Months_Analysis

#===========================================================
#Prepare data and install required packages
#===========================================================
#Install packages
install.packages("tidyverse")
install.packages("ggrepel")
install.packages("ggtext")
install.packages("ggpubr")
install.packages("dplyr")

#Load library
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(ggtext)
library(ggpubr)
library(readxl)


#Set working directory
setwd("D:/Users/Pc User/Desktop/cyclist trip data/cyclistic excel files")

#display your working directory
getwd()

#View the list of the files in the directory
list.files()

#Upload datasets 
y22_07 <- read_xlsx("202207-divvy-tripdata.xlsx")
y22_08 <- read_xlsx("202208-divvy-tripdata.xlsx")
y22_09 <- read_xlsx("202209-divvy-tripdata.xlsx")
y22_10 <- read_xlsx("202210-divvy-tripdata.xlsx")
y22_11 <- read_xlsx("202211-divvy-tripdata.xlsx")
y22_12 <- read_xlsx("202212-divvy-tripdata.xlsx")
y23_01 <- read_xlsx("202301-divvy-tripdata.xlsx")
y23_02 <- read_xlsx("202302-divvy-tripdata.xlsx")
y23_03 <- read_xlsx("202303-divvy-tripdata.xlsx")
y23_04 <- read_xlsx("202304-divvy-tripdata.xlsx")
y23_05 <- read_xlsx("202305-divvy-tripdata.xlsx")
y23_06 <- read_xlsx("202306-divvy-tripdata.xlsx")

#=========================================================
#Combine previous 12 months data to a single table
#=========================================================

#Check column name consistency
colnames(y22_07)
colnames(y22_08)
colnames(y22_09)
colnames(y22_10)
colnames(y22_11)
colnames(y22_12)
colnames(y23_01)
colnames(y23_02)
colnames(y23_03)
colnames(y23_04)
colnames(y23_05)
colnames(y23_06)

#inspect dataframes for inconsistency
str(y22_07)
str(y22_08)
str(y22_09)
str(y22_10)
str(y22_11)
str(y22_12)
str(y23_01)
str(y23_02)
str(y23_03)
str(y23_04)
str(y23_05)
str(y23_06)

#inconsistency in end_station_id data type in y22_09
#change end_station_id data type to character, so that 
#it matches other tables end_station_id data types.
y22_09 <- mutate(y22_09, end_station_id = as.character(end_station_id))

#Combine each month data frames into a single big data frames and table
twelve_months_trips <- bind_rows(y22_07, y22_08, y22_09, y22_10, y22_11, y22_12, 
				y23_01, y23_02, y23_03, y23_04, y23_05, y23_06)

#inspect the tables
#understand its dimension, columns, data types and statistical summary
dim(twelve_months_trips)
colnames(twelve_months_trips)
str(twelve_months_trips)
summary(twelve_months_trips)
head(twelve_months_trips)

#======================================================================
#Data Cleaning for analysis
#======================================================================

#Drop rows that have NA rows in data
clean_12_mths_trips <- drop_na(twelve_months_trips)

#Remove the entries that will have negative ride length
#This can be obtained by filter out the rides that have smaller started_at with ended_at
#This also set the lower bound for the ride length which is 1
clean_12_mths_trips <- clean_12_mths_trips %>%
	filter(started_at < ended_at) 

#Calculate ride_length format in seconds
clean_12_mths_trips$ride_length <- difftime(clean_12_mths_trips$ended_at,clean_12_mths_trips$started_at)

#Convert ride_length type to numeric
clean_12_mths_trips$ride_length <-
	as.numeric(as.character(clean_12_mths_trips$ride_length))

#Check ride length as numeric type is True
is.numeric(clean_12_mths_trips$ride_length)

#Rename ride_length to ride_duration for clarity
clean_12_mths_trips <-
	rename(clean_12_mths_trips, ride_duration = ride_length)

#Filter out the outlier of the ride duration
#Assume that there are consensus that the ride duration is only acceptable
#within 24 hours for this data analysis
clean_12_mths_trips <- 
	clean_12_mths_trips %>%
	filter(ride_duration < 86400)

#Check the clean data
summary(clean_12_mths_trips)
head(clean_12_mths_trips)
str(clean_12_mths_trips)
dim(clean_12_mths_trips)

#====================================================================
#Analyse the data
#1) Pie Chart
#2) Bar Chart
#====================================================================

#==================================================================
1) Pie chart
#==================================================================
#Create pie chart for subscriber type that subscribe to Cyclistic. 
#Create the table for member_casual percentage 
member_casual_percentage_stat <- 
	clean_12_mths_trips %>%
	group_by(member_casual) %>%
	summarize(total_rider = n()) %>%
	mutate(percentage = paste0(round(total_rider/sum(total_rider) * 100, 2), "%"))

#Find the position for repel label
repel_label_position <- 
	member_casual_percentage_stat %>%
	arrange(desc(member_casual)) %>%
	mutate(pos = cumsum(total_rider) - total_rider / 2.5)

#Create pie chart with repel label ggplot
member_casual_pie_chart <- 
	member_casual_percentage_stat %>%
	ggplot(aes(x = "", y = total_rider, fill = member_casual)) +
	geom_bar(stat = "identity", width=1, color = "black") +
	coord_polar("y", start=0) +
	geom_text(aes(label = total_rider), position = position_stack(vjust = 0.5), size=4.5) +
	geom_label_repel(repel_label_position ,mapping = aes(y = pos, label = percentage), 
		size = 5.0, nudge_x = 0.75, nudge_y = 0.75, show.legend = FALSE) +
	guides(fill = guide_legend(title = "Customer type")) +
	labs(title = "Cyclistic's Customers Type Percentage", 
		subtitle = "Data collected from July 2022 to June 2023 \nTotal number of customers:  4116858",
		caption = "Data Source: Divvy Data")+
	theme_void() + #remove background, grid, and numeric labels
	theme(plot.title = element_text(size = 15, face="bold")) 	

#=========================================================================
Stack Bar chart for rideable type within customer type
#=========================================================================
#Create the stacked bar chart of member_casual to understand the amount of 
#rider type that subscribe to cyclistic

#Remove scientific notation with options
options(scipen=999)

#Create the table for rideable type and customer type 
rideable_type_stat <- 
	clean_12_mths_trips %>%
	count(member_casual, rideable_type, sort = TRUE) %>%
	rename(total_rider = n) %>%
	mutate(percentage = paste0(round(total_rider/sum(total_rider) * 100, 1), "%"))

#Create stack bar chart for member_casual with rideable_type as composition
rideable_type_stack_bar_chart <- 
	rideable_type_stat %>%
	ggplot(aes(x = member_casual, y = total_rider, fill = rideable_type)) +
	geom_bar(stat = "identity") +
	geom_text(aes(label = percentage), position = position_stack(vjust = 0.5), size=4.5) +
	guides(fill = guide_legend(title = "rideable type")) +
	labs(title = "Cyclistic's Rideable Type Percentage", 
		subtitle = "Data collected from July 2022 to June 2023 \nTotal number of customers:  4116858",
		caption = "Data Source: Divvy Data", x="Customer Type", y="Total number of rides")+
	theme(plot.title = element_text(size = 15, face="bold")) 

#====================================================================
#Boxplot for ride length of customer type
#====================================================================
#Create boxplot for ride duration of customer type to understand the 
#ride duration distribution

#Create ride duration stat table for member_casual
rides_duration_member_casual_stat <-
	clean_12_mths_trips %>%
	group_by(member_casual) %>%
	summarise(average_ride_duration = round(mean(ride_duration),2),
		first_quartile = quantile(ride_duration, probs = c(0.25)),
		median = median(ride_duration),
		third_quartile = quantile(ride_duration, probs = c(0.75)),
		min = min(ride_duration),
		max = max(ride_duration))

#Create ride duration stat table for member_casual and rideable_type 
rides_duration_rideable_type_stat <-
	clean_12_mths_trips %>%
	group_by(member_casual, rideable_type) %>%
	summarise(average_ride_duration = round(mean(ride_duration),2),
		first_quartile = quantile(ride_duration, probs = c(0.25)),
		median = median(ride_duration),
		third_quartile = quantile(ride_duration, probs = c(0.75)),
		min = min(ride_duration),
		max = max(ride_duration))

#Create Boxplot to compare statistical distribution of member_casual
casual_member_boxplot <-
	clean_12_mths_trips %>%
	ggplot(mapping=aes(x=member_casual, y=ride_duration, fill = member_casual)) +
	geom_boxplot(outlier.shape = NA) +
	coord_cartesian(ylim=c(0, 3000)) +
	guides(fill = guide_legend(title = "customer type")) +
	labs(title = "Cyclistic's Rides Duration Versus Customer Type Box Plot", 
		subtitle = "Data collected from July 2022 to June 2023",
		caption = "Data Source: Divvy Data", x="Customer Type", y="Rides duration (Sec)")+
	theme(plot.title = element_text(size = 12, face="bold")) 
		
#Create Boxplot compare rideable_type statistical distribution for member_casual
rideable_type_boxplot <-
	clean_12_mths_trips %>%
	ggplot(mapping=aes(x=member_casual, y=ride_duration, fill = rideable_type)) +
	geom_boxplot(outlier.shape = NA) +
	coord_cartesian(ylim=c(0, 6500)) +
	guides(fill = guide_legend(title = "rideable type")) +
	labs(caption = "Data Source: Divvy Data", x="Customer Type", y="Rides duration (Sec)") 

#Combine both casual_member_boxplot and rideable_type_boxplot on one page
Cyclistic_box_plot <- ggarrange(casual_member_boxplot, rideable_type_boxplot, ncol = 1)

#============================================================================
#line chart for ride duration analysis
#============================================================================
#Create table that contains start date, month and years
clean_12_mths_trips_2 <-
	clean_12_mths_trips %>%
	mutate(start_date = as.Date(started_at), start_hour = hour(started_at)) %>%
	mutate(start_month_years = format(as.Date(started_at), "%Y-%m"))
		
#Line chart customer type comparison for average ride duration per day for the 12 months	
ride_duration_day_stat <-
	clean_12_mths_trips_2 %>%
	group_by(start_date, member_casual) %>%
	summarise(average_ride_duration = round(mean(ride_duration),2))

#Create line chart average ride length per day comparison for member and casual  
avg_ride_duration_per_day <-
	ride_duration_day_stat %>%
	ggplot(mapping=aes(x=start_date, y = average_ride_duration)) +
	geom_line(aes(color = member_casual)) +
	geom_point(aes(color = member_casual)) + 
	labs(title = "Cyclistic's Average Rides Duration Versus Date", 
		subtitle = "Data collected from July 2022 to June 2023",
		caption = "Data Source: Divvy Data", x="Date", y="Average Rides duration (Sec)")+
	theme(plot.title = element_text(size = 12, face="bold")) 

#Line chart customer type comparison for average ride duration per month for the 12 months	
ride_duration_month_stat <-
	clean_12_mths_trips_2 %>%
	group_by(start_month_years, member_casual) %>%
	summarise(average_ride_duration = round(mean(ride_duration),2))

#Create line chart average ride length per day comparison for member and casual  
avg_ride_duration_per_month <-
	ride_duration_month_stat %>%
	ggplot(mapping=aes(x=start_month_years, y = average_ride_duration, group = member_casual)) +
	geom_line(aes(color = member_casual)) +
	geom_point(aes(color = member_casual)) + 
	labs(title = "Cyclistic's Average Rides Duration Versus Month", 
		subtitle = "Data collected from July 2022 to June 2023",
		caption = "Data Source: Divvy Data", x="Month (yrs - mth)", y="Average Rides duration (Sec)")+
	theme(plot.title = element_text(size = 12, face="bold")) +
	theme(axis.text.x = element_text(angle = 45))
 
#Line chart customer type comparison for average ride duration day of week for the 12 months	
ride_duration_weekday_stat <-
	clean_12_mths_trips_2 %>%
	group_by(day_of_week, member_casual) %>%
	summarise(average_ride_duration = round(mean(ride_duration),2))

#Create line chart average ride duration by day of week comparison for member and casual  
avg_ride_duration_day_of_week <-
	ride_duration_weekday_stat %>%
	ggplot(mapping=aes(x=day_of_week, y = average_ride_duration, group = member_casual)) +
	geom_line(aes(color = member_casual)) +
	geom_point(aes(color = member_casual)) + 
	coord_cartesian(ylim=c(500, 1700)) +
	labs(title = "Cyclistic's Average Rides Duration Versus Day of Week", 
		subtitle = "Data collected from July 2022 to June 2023",
		caption = "Data Source: Divvy Data", x="Day of Week", y="Average Rides duration (Sec)")+
	theme(plot.title = element_text(size = 12, face="bold")) 
	
#Line chart customer type comparison for average ride duration day of week for the 12 months	
ride_duration_hour_stat <-
	clean_12_mths_trips_2 %>%
	group_by(start_hour, member_casual) %>%
	summarise(average_ride_duration = round(mean(ride_duration),2))

#Create line chart average ride duration by hour comparison for member and casual  
avg_ride_duration_hour <-
	ride_duration_hour_stat %>%
	ggplot(mapping=aes(x=start_hour, y = average_ride_duration)) +
	geom_line(aes(color = member_casual)) +
	geom_point(aes(color = member_casual)) +
	coord_cartesian(ylim=c(500, 1700)) +
	labs(title = "Cyclistic's Average Rides Duration Versus hour", 
		subtitle = "Data collected from July 2022 to June 2023",
		caption = "Data Source: Divvy Data", x="Hours of a day", y="Average Rides duration (Sec)")+
	theme(plot.title = element_text(size = 12, face="bold")) 

