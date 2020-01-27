############################################################
#IST 687, HW03

#Student Name: Tejas Patil
#Homework Number: HW03
#Date Due: 09/16/2019

# Attribution statement: 
# 1. I did this homework by myself, with help from the book and the professor

#Running codes for clean test of Prep code
dev.off() #clear the graph window
cat('\014') #clear the console
rm(list = ls()) #clear all user objects from the enviornment 

#Setting the working directory
setwd("C:/Users/hp/Desktop/IST 687/Homework")

help("read.csv")
#stringsAsFactors:- logical: should character vectors be converted to factors?
#                   Note that this is overridden by as.is and colClasses, both
#                   of which allow finer control.


#dfStates <- read.csv("https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv", stringsAsFactors = FALSE)
#was working at first not workinh now

dfStates <- read.csv("Nst-est2011.csv", stringsAsFactors = FALSE)

View(dfStates)    #Viewing the raw data
head(dfStates)    #check the first six instances of the data
tail(dfStates)    #check the last six instances of the data
summary(dfStates) #statistical analysis of the data
str(dfStates)     #check dimensions of dataframe and datatype of variables

#delete unwanted rows and columns
dfStates <- dfStates[-c(1:8,60:66), -(6:10)]
dim(dfStates) #check dimensions of dataframe

#Variable nomenclature - giving meaningful names
colnames(dfStates) <- c("stateName", "Census",  "Estimated", "Pop2010", "Pop2011")

#remove ',' from the instances using 'gsub' command
help("gsub")
dfStates[,2] <- gsub(",", "", dfStates[,2])
dfStates[,3] <- gsub(",", "", dfStates[,3])
dfStates[,4] <- gsub(",", "", dfStates[,4])
dfStates[,5] <- gsub(",", "", dfStates[,5])
View(dfStates)

#converting 'character' datatype to 'numeric'
help("as.numeric")
dfStates[,2] <- as.numeric(dfStates[,2])
dfStates[,3] <- as.numeric(dfStates[,3])
dfStates[,4] <- as.numeric(dfStates[,4])
dfStates[,5] <- as.numeric(dfStates[,5])
str(dfStates)

#Claculate mean of numeric variables
mean(dfStates[,2]) #Census
mean(dfStates[,3]) #Estimation
mean(dfStates[,4]) #Pop2010
mean(dfStates[,5]) #Pop2011

#HOMEWORK
#STEP 1
#Create a function so that we can clean this dataset in one go!
testdata <- read.csv("Nst-est2011.csv", stringsAsFactors = FALSE)
readStates <- function(x){
  x <- x[-c(1:8,60:66), -(6:10)]
  colnames(x) <- c("stateName", "Census",  "Estimated", "Pop2010", "Pop2011")
  x[,2] <- gsub(",", "", x[,2])
  x[,3] <- gsub(",", "", x[,3])
  x[,4] <- gsub(",", "", x[,4])
  x[,5] <- gsub(",", "", x[,5])
  x[,2] <- as.numeric(x[,2])
  x[,3] <- as.numeric(x[,3])
  x[,4] <- as.numeric(x[,4])
  x[,5] <- as.numeric(x[,5])
  return(x)
}
readStates(testdata)
dfStates <- readStates(testdata)

#calc. min, mean and max population for Pop2011
min(dfStates$Pop2011)
mean(dfStates$Pop2011)
max(dfStates$Pop2011)

which.max(dfStates$Pop2011) #find row number of state with largest pop
dfStates[which.max(dfStates$Pop2011), 1] #give name of state with largest pop
#which.max() gives us index number, it can be used to extract name of required state

which.min(dfStates$Pop2011) #find row number of state with smallest pop
dfStates[which.min(dfStates$Pop2011), 1] #give name of state with smallest pop
#which.max() gives us index number, it can be used to extract name of required state

#STEP 2
#sort the states according to increasing population in 2011 
dfStatesOrdered <- dfStates[order(dfStates$Pop2011), ]

#STEP 3
#plot a histogram of any variable
hist(dfStates$Pop2011)
# I observed that the distribution of the variable is "right-skewed".
#Lot of states have population lesser than the median value and some
#states have very high population than the median value

hist(rnorm(dfStates$Pop2011)) #histogram of normally distributed variable


