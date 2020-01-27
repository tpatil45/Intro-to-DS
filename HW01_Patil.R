############################################################
#IST 687, HW PE01

#Student Name: Tejas Patil
#Homework Number: HW01
#Date Due: 09/02/2019

# Attribution statement: 
# 1. I did this homework by myself, with help from the book and the professor

#Running codes for clean test of Prep code
dev.off() #clear the graph window
cat('\014') #clear the console
rm(list = ls()) #clear all user objects from the enviornment 

#Setting the working directory
setwd("C:/Users/hp/Desktop/IST 687/Prep")

#Assining values to vectors 'height' and 'weight'
height <- c(59, 60, 61, 58, 67, 72, 70)
weight <- c(150, 140, 180, 220, 160,  140,130)

#Defining variable 'a'
a <- 150

#computing average height
mean(height)
#computing average weight
mean(weight)

#no of observations in height
h <- length(height)
#no of observations in weight
w <- length(weight)

#calculating sum of heights
sh <- sum(height)
#calculating sum of weights
sw <- sum(weight)

#average by sum of heights
sh/h
#average by sum of heights
sw/w

#calculating maximum height
maxH <- max(height)
#calculating minimum weight
minW <- min(weight)

#creating new vector "extraWeight" for additional 25 units
extraWeight <- weight + 25
#calculating average of "extraweight"
mean(extraWeight)

#practice code for conditional statements
if  ( 100 < 150 ) "100 is less than 150" else "100 is greater than 150"

#Is maxH greater than 70?
if  ( maxH > 70 ) "YES" else "NO"

#Is minW greater than variable 'a'?
if  ( minW > a ) "YES" else "NO"

#creating vector "bigHT" with all heights greater than 60
bigHT <- height[height > 60]

#creating vector "smallWT" with 2nd and 4th element of weight
smallWT <- weight[c(2,4)]

#remove 3rd element from weight vector
weight <- weight[-3]

#checking height(3)
height(3)

#SO height(3) GENERATES AN ERROR BECAUSE TO SELECT AN ELEMENT FROM VECTOR
#WE HAVE TO USE SQUARE BRACKET AND NOT PARENTHESIS

#CORRECT CODE
height[3]
