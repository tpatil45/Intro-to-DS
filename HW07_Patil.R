############################################################
#IST 687, HW07

#Student Name: Tejas Patil
#Homework Number: HW07
#Date Due: 10/14/2019

# Attribution statement: 
# 1. I did this homework by myself, with help from the book and the professor

#Running codes for clean test of Prep code
dev.off() #clear the graph window
cat('\014') #clear the console
rm(list = ls()) #clear all user objects from the enviornment 

#Setting the working directory
setwd("C:/Users/hp/Desktop/IST 687/Homework")

#Read the dataset into "mydata" 
mydata <- read.csv("MedianZIP.csv", stringsAsFactors = FALSE)
#cehck the structure of the data
str(mydata)
#changing the datatpye of "Mean" variable from character to numeric
mydata$Mean <- as.numeric(mydata$Mean)
#check for the NA's
summary(mydata)
#Replacing NA's in mean by corresponding median value
na_index <- which(is.na(mydata$Mean))
for (i in na_index) {
  mydata$Mean[i] <- mydata$Median[i]
}
#check if NA's are replaced
summary(mydata)
#another way to do it
#mydata$Mean[is.na(mydata$Mean)] <- mydata$Median[is.na(mydata$Mean)]

#View data and know the columns

#2 - Install the zipcode package
install.packages("zipcode")
#library the package
library(zipcode)
#Repair the zipcode column
mydata$zip <- clean.zipcodes(mydata$zip)
#Call the zipcode dataset 
data(zipcode)
#merge the two datasets
dfNew <- merge(mydata, zipcode, by="zip")
View(dfNew)

#Combine these predefined variables in R into a single dataframe
stateNameDF <- data.frame(state=state.abb, stateName=state.name, center=state.center)
#changing all the values of common variable to lower case for merging
stateNameDF$stateName <- tolower(stateNameDF$stateName)
View(stateNameDF)
#merge the dfnew and stateNameDF datasets
new_states <- merge(dfNew, stateNameDF, by="state")
View(new_states)


############## HOMEWORK ###############

library(ggplot2)
library(ggmap)
library(maps)

#call the map data of united states in "us"
us <- map_data("state")

#Using ggplot on new_states, in asesthetics map_id is stateName variable, this plot the map according to the content of stateName avariable
dotmap <- ggplot(new_states, aes(map_id = stateName))
#define geometry as maps, in which map = us
dotmap <- dotmap + geom_map(map = us)
#define geometry as scatterplot, with x variable as longitude, y as latitude and color scale according to the Mean variable
dotmap <- dotmap + geom_point(aes(x = longitude, y = latitude, color = Mean))
#Display the plot
dotmap

#EXPLANATION: WHY THIS MAP IS NOT GOOD?
#THIS MAP IS NOT GOOD BECAUSE IT DOES NOT FRAME THE US MAP VERY WELL, THE EXTRA PORTION OF 
#MAP IS DISPLAYED, WHICH REDUCES THE ZOOM LEVEL AND WE CANNOT FOCUS ON THE DETAILS

#STEP 2
#library the tidyverse() package
library(tidyverse)


summaryDF <- new_states %>%  #load the new_states data into summaryDF
  group_by(stateName) %>%    #the data is grouped by the diffrent values in stateName variable using pipe operator
  summarize(totalpop = sum(Pop), Income = sum(Mean * Pop)) #Define a new column Income and summarize will add the population in respect to diffrent values of StateName

str(summaryDF)
#So the dataframe has 50 rows(observations)
#The new dataframe was created from new_states dataset, stateName variable was retained
#2 new variables were created using the summarize function, Income and totalPop
View(summaryDF)

summaryDF$meanIncome <- summaryDF$Income / summaryDF$totalpop 

#create a map visualisation where color represents meanIncome

miFill <- ggplot(summaryDF, aes(map_id = stateName))
miFill <- miFill + geom_map(map = us, aes(fill = meanIncome))
miFill <- miFill + expand_limits(x = us$long, y = us$lat)
miFill <- miFill + coord_map()
miFill 


#HERE NO STATES ARE GREY, BUT IF THEY WERE. THAT'LL BE BECAUSE OF THE NA'S IN DATASET

#THE DATASET HAS ALREADY BEEN TREATED FOR NA'S

SNsummary <- merge(stateNameDF, summaryDF, by = "stateName")
View(SNsummary)

incomePoint <- ggplot(SNsummary, aes(map_id = stateName))
incomePoint <- incomePoint + geom_map(map = us, aes(fill = meanIncome))
incomePoint <- incomePoint + geom_point(aes(x = center.x, y = center.y, size = totalpop, color = "red"))
incomePoint <- incomePoint + expand_limits(x = us$long, y = us$lat)
incomePoint <- incomePoint + coord_map()
incomePoint 

