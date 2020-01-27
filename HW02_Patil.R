############################################################
#IST 687, HW02

#Student Name: Tejas Patil
#Homework Number: HW02
#Date Due: 09/09/2019

# Attribution statement: 
# 1. I did this homework by myself, with help from the book and the professor

#Running codes for clean test of Prep code
dev.off() #clear the graph window
cat('\014') #clear the console
rm(list = ls()) #clear all user objects from the enviornment 

#Setting the working directory
setwd("C:/Users/hp/Desktop/IST 687/Homework")

#creating a new dataframe by assining preloaded dataset "USArrests"
myArrests <- USArrests

#checking if the new dataset was properly populated
View(myArrests)

#getting the attributes for dataframe
colnames(myArrests)

#getting the instances for dataframe
rownames(myArrests)

#getting the summary of this dataset
summary(myArrests)

#creating dataframe of my Family
#creating a vector(column) with the names of family members
myFamilyNames <- c("Mom", "Dad", "Me")

#creating a vector assining ages to the names
myFamilyAges <- c(48,50,23)

#creating a vector representing eye colors of family members
myFamilyEyeColors <- c("Black","Brown","Black")

#creating a dataframe by combining the columns
myFamily <- data.frame(myFamilyNames, myFamilyAges, myFamilyEyeColors)

#checking the use of str() command
str(myFamily)

#How does the dataframe looks?
View(myFamily)

#removing third row from the datframe
myFamily <- myFamily[-3, ]

#removing eye colour column from dataframe
myFamily <- myFamily[ ,-3]

#checking the final dataframe
View(myFamily)


#Homework
#STEP A: Explore the myArrests dataframe from PE02.


#summary command for myArrests dataframe to refamiliarize with the data
summary(myArrests)

#STEP B: Explore the assault rate

#lower assault rate is better, lesser the assault rate, safer the state
#mean assualt rate is the average assault rate of all states
AvgAssaultrate <- mean(myArrests$Assault)
AvgAssaultrate

#A state which has best(lesser) assualt rate is "North Dakota"
rownames(myArrests[which.min(myArrests$Assault),])


#STEP C: Explore the murder rate


#A state which has highest murder rate is "Georgia"
rownames(myArrests[which.max(myArrests$Murder),])

#creating a sorted dataframe based on descending murder rates
sortedDF1 <- myArrests[order(-myArrests$Murder),]

#10 states with highest murder rates are
head(sortedDF1, 10)
#OR
rownames(sortedDF1[1:10 , ])


#STEP D: Which is the safest state?


#attributes which are appropriate for determining the safest state:
#murder, assault, rape

#The two diffrent ways to arithmatically combine these attributes are:
#1] Sum of all 3 attributes
#2] Average of all 3 attributes

#creating a new column 'SafeIndex' containing the sum of all 3 attributes
myArrests$SafeIndex <- myArrests$Murder + myArrests$Assault + myArrests$Rape

#Finding the safest state based on safe index
rownames(myArrests[which.min(myArrests$SafeIndex),])
#North Dakota is the Safest state

#OR for finding the safest state create a new dataframe safeStates with ascending order of SafeIndex
safeStates <- myArrests[order(myArrests$SafeIndex), ]
rownames(safeStates[1,])


#STEP E: In depth look at the state with "best" combination of the arrest attributes.


#Finding the 5 safe states when rape and murder counted twice
myArrests$SafeIndex1 <- (myArrests$Murder)*2 + myArrests$Assault + (myArrests$Rape)*2
safeStates1 <- myArrests[order(myArrests$SafeIndex1), ]
rownames(safeStates1[1:5,])

#Finding the 5 safe states when rape and murder counted twice but with SCALING
myArrests$SafeIndex2 <- 2*scale(myArrests$Murder) + scale(myArrests$Assault) + 2*scale(myArrests$Rape)
safeStates2 <- myArrests[order(myArrests$SafeIndex2), ]
rownames(safeStates2[1:5,])


