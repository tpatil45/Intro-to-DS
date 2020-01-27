############################################################
#IST 687, HW06

#Student Name: Tejas Patil
#Homework Number: HW06
#Date Due: 10/07/2019

# Attribution statement: 
# 1. I did this homework by myself, with help from the book and the professor

#Running codes for clean test of Prep code
dev.off() #clear the graph window
cat('\014') #clear the console
rm(list = ls()) #clear all user objects from the enviornment 

#Setting the working directory
setwd("C:/Users/hp/Desktop/IST 687/Homework")

#Create a function so that we can clean this dataset in one go!
readStates <- function(){
  x <- read.csv("Nst-est2011.csv", stringsAsFactors = FALSE)
  x <- x[-c(1:8,60:66), -(6:10)]
  colnames(x) <- c("stateName", "Census",  "Estimated", "Pop2010", "Pop2011")
  x[,1] <- sub(".", "", x[,1])
  for (i in 2:5) {
    x[,i] <- as.numeric(gsub(",", "", x[,i]))
  }
  return(x)
}

states <- readStates()                                             #Execute the function and read the dataset in 'states' dataframe
arrests <- USArrests                                               #Read the USArrests in 'arrests' dataframe
arrests$stateName <- rownames(arrests)                             #Add a column in 'arrests' of all the state names
mergeDF <- merge(states, arrests, by = "stateName", all.x = TRUE)  #Merging 'arrests' and 'states' using a common variable 'stateName'
#help("merge")
#str(mergeDF)
#View(mergeDF)

library(ggplot2)                                                   

ggplot(mergeDF) + aes(y = Murder) + geom_boxplot() + ggtitle("Boxplotting Murders in US States")
                                                                   #Plotting(boxplot) the Murders varible of mergeDF dataframe


myplot <- ggplot(mergeDF, aes(x = Murder))
myplot <- myplot + geom_histogram(binwidth = 2, color = "black", fill = "white")
myplot <- myplot + ggtitle("Histogram of Murders in US States")
myplot
