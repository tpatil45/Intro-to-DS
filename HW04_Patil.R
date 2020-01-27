############################################################
#IST 687, HW04

#Student Name: Tejas Patil
#Homework Number: HW04
#Date Due: 09/23/2019

# Attribution statement: 
# 1. I did this homework by myself, with help from the book and the professor

#Running codes for clean test of Prep code
dev.off() #clear the graph window
cat('\014') #clear the console
rm(list = ls()) #clear all user objects from the enviornment 

#Setting the working directory
setwd("C:/Users/hp/Desktop/IST 687/Homework")

#creating function
printVecInfo <- function(numVec){
  print(mean(numVec))
  print(median(numVec))
  print(min(numVec))
  print(max(numVec))
  print(sd(numVec))
  print(quantile(numVec, probs = c(0.05,0.95)))
}

#creating test vector
testVector <- 1:10

#executing function
printVecInfo(testVector)

help("cat")

#creating function
printVecInfo <- function(numVec){
  paste(c("Mean", "Median", "Min Value", "Max Value", "Std. Deviation", "5th Percentile",
  "95th Percentile"), c(mean(numVec), median(numVec), min(numVec), max(numVec), sd(numVec),
  quantile(numVec, probs = c(0.05,0.95))))
}

#executing function
printVecInfo(testVector)

#Importing airquality data into new dataframe
myAQdata <- airquality 

View(myAQdata)
help("airquality") #Understanding the data

head(myAQdata, 5) #Display first 5 instances

# ----------------------------- HOMEWORK ---------------------------


#STEP 1:

myAQdata$Ozone[is.na(myAQdata$Ozone)]

is.na(myAQdata$Ozone)

#Explanation: So is.na() marks all the indices with missing values as TRUE and every other
# values as FALSE. When we select those indices within that column(line 55) it gives the the
# value at that place, which all will be NAs.


#Imputing the mean value of that column to the missing values in that column
myAQdata$Ozone[is.na(myAQdata$Ozone)] <- round(mean(myAQdata$Ozone, na.rm = TRUE))


#Checking which other column has NAs in it
summary(myAQdata)

#So summary shows that Solar.R has NAs, applying same code as in line 70
myAQdata$Solar.R[is.na(myAQdata$Solar.R)] <- round(mean(myAQdata$Solar.R, na.rm = TRUE))

#STEP 2


#Install new package
#install.packages("imputeTS")
library(imputeTS)

#creating new dataframe 
myAQdata1 <- airquality

help("na_interpolation")

#Since NAs are only in first 2 variables, we will apply code to only them
myAQdata1$Ozone <- na_interpolation(myAQdata1$Ozone)
myAQdata1$Solar.R <- na_interpolation(myAQdata1$Solar.R)

#Compare first 5 rows of myAQdata and myAQdata1
head(myAQdata, 5)
head(myAQdata1, 5)
# Compare myAQdata and myAQdata1
# SO THE 5TH VALUES IN OZONE AND SOLAR.R ARE DIFFRENT, NA_INTERPOLATION VALUES ARE MORE
# RELEVANT TO THE FIRST 4 OBSERVATIONS OF DATASET THAN THE IMPUTED MEAN VALUES.

#STEP 3

#sample first 10 obesrvations of Wind column
#set.seed(5)
first10 <- sample(myAQdata$Wind, 10, replace = TRUE)

printVecInfo(first10) # display statistical characteristics

hist(first10) #histogram of first 10 observations

#Explain: 1] replace = TRUE :- Once a value is selected for sampling, it is again placed in
# the data, so the next time every value has the same probability for getting selected
#replace = FALSE:- Once a value is selected, it wont be replaced in the data, so that same values
# wont be repeating in the same sample

# I used replace = TRUE


#1st attempt
first10 <- sample(myAQdata$Wind, 10, replace = TRUE)
printVecInfo(first10) # display statistical characteristics
hist(first10) #histogram of first 10 observations

#2nd attempt
first10 <- sample(myAQdata$Wind, 10, replace = TRUE)
printVecInfo(first10) # display statistical characteristics
hist(first10) #histogram of first 10 observations

#3rd attempt
first10 <- sample(myAQdata$Wind, 10, replace = TRUE)
printVecInfo(first10) # display statistical characteristics
hist(first10) #histogram of first 10 observations

#EXPLANATION: EVERY TIME WHILE SAMPLING, A DIFFRENT SAMPLE IS SELECTED AND WE GET DIFFRENT
# VALUES IN OUR SAMPLE EVERYTIME, HENCE THE STATISTICAL VALUES AND HISTOGRAM IS DIFFRENT EVERYTIME.


#STEP 4


#Replicating the mean of sample wind and plotting histogram of variable wind
hist(replicate(200, mean(sample(myAQdata$Wind, 10, replace = TRUE)), simplify = TRUE))

#repeating 2 more times
hist(replicate(200, mean(sample(myAQdata$Wind, 10, replace = TRUE)), simplify = TRUE))
hist(replicate(200, mean(sample(myAQdata$Wind, 10, replace = TRUE)), simplify = TRUE))


#Compare the histograms 
# THE HISTOGRAM THAT WE OBTAINED AFTER REPLICATING THE MEAN OF THE SAMPLE 200 time,
# SHOWS THE CENTRAL TENDENCY EFFECT. DUE TO THE COMBINED EFFECT OF CT AND LAW OF LARGE NUMBERS, 
# HISTOGRAM SHOWS A NORMAL DISTRIBUTION.
