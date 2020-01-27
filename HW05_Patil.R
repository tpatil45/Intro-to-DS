############################################################
#IST 687, HW05

#Student Name: Tejas Patil
#Homework Number: HW04
#Date Due: 09/23/2019

# Attribution statement: 
#  # 2. I did this work with help from the book and the professor and these Internet sources: <https://dplyr.tidyverse.org/reference/tally.html>

#Running codes for clean test of Prep code
dev.off() #clear the graph window
cat('\014') #clear the console
rm(list = ls()) #clear all user objects from the enviornment 

#Setting the working directory
setwd("C:/Users/hp/Desktop/IST 687/Homework")

#libraring the packages
library(RCurl)
library(jsonlite)

#import data from URL and convert to R object
dataset <- getURL("http://opendata.maryland.gov/resource/pdvh-tf2u.json")
df <- jsonlite::fromJSON(dataset)

#Understand the data
View(df)
str(df)
summary(df)
head(df)

#help("getURL")

#help("fromJSON")

#Count the no. of accidents
nrow(df)                                               #no. of observations
unique(df$case_number)                                 #unique accidents
which(df$case_number == df$case_number[which(duplicated(df$case_number))])
                                                       #which observations are repeated

#Summary and Structure
summary(df)
str(df)

#covert data type from character to numeric 
df$vehicle_count <- as.numeric(df$vehicle_count)
str(df$vehicle_count)

#install.packages("tidyverse")
library(tidyverse)

value <- df %>%
  filter(str_trim(day_of_week)=="THURSDAY") %>%
  pull(vehicle_count) %>%
  mean(na.rm=TRUE)
value
#this code gives me the mean of vehicle_count of accidents occured only on THURSDAY,
#excluding the the observations containing NA's
#This code is executed using the pipe operator of tidyverse package

value <- mean( df$vehicle_count[str_trim(df$day_of_week)=="THURSDAY"], na.rm=TRUE)
value
#this code gives me the mean of vehicle_count of accidents occured only on THURSDAY,
#excluding the the observations containing NA's


#CHECK for NA's
apply(is.na(df),2,sum)

#Total number of accident with injuries is 301
length(df$case_number[str_trim(df$injury) == "YES"])

#Total number of accidents on FRIDAY
length(df$case_number[str_trim(df$day_of_week) == "FRIDAY"])

#Total number of accidents on FRIDAY where injury = YES
length(df$case_number[str_trim(df$day_of_week) == "FRIDAY" & str_trim(df$injury) == "YES"])

#Total number of accidents on FRIDAY where injury = NO
length(df$case_number[str_trim(df$day_of_week) == "FRIDAY" & str_trim(df$injury) == "NO"])

#For next question using group_by command of dplyr package
help("group_by")

#How many injuries on each day of the week?
df %>%
  filter(str_trim(df$injury) == "YES") %>%
  group_by(day_of_week) %>%
  tally()

#replace the NA's value with mean value for vehicle_count
df$vehicle_count[is.na(df$vehicle_count)] <- round(mean(df$vehicle_count, na.rm = TRUE))
df$vehicle_count[is.na(df$vehicle_count)] #checking

#Creating a new dataframe with accidents only on friday 
FriAcc <- df[which(str_trim(df$day_of_week) == "FRIDAY") , ]
View(FriAcc)

#mean and histogram of no. of vehicles involved in accident only on friday
mean(FriAcc$vehicle_count)
hist(FriAcc$vehicle_count)

#histogram of no. of vehicles involved in accident only on sunday
hist(df$vehicle_count[which(str_trim(df$day_of_week) == "SUNDAY")])
abline(v=quantile(df$vehicle_count[which(str_trim(df$day_of_week) == "SUNDAY")],c(0.05,0.95),col="red"))
#quantile of no. of vehicles involved in accident only on Friday and Sunday
quantile(FriAcc$vehicle_count, c(0,0.05,0.25,0.5,0.75,0.95,1))
quantile(df$vehicle_count[which(str_trim(df$day_of_week) == "SUNDAY")], c(0,0.05,0.25,0.5,0.75,0.95,1))

#COMPARE DISTRIBUTION ON SUNDAY AND FRIDAY
#1] The Maximum number of vehicle count on sunday and friday were 4 and 5 respectively.
#2] Vehicle count 2 is more on sunday while vehicle count 1 is more on fridays.
#3] Both the distributions look right-skewed.
#4] The median for vehicle count on sunday and friday is 1 and 2 respectively.
