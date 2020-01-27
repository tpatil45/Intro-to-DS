############################################################
#IST 687, HW09

#Student Name: Tejas Patil
#Homework Number: HW09
#Date Due: 10/30/2019

# Attribution statement: 
# 2. I did this work with help from the book and the professor and these Internet sources:
  #<https://stackoverflow.com/questions/7442207/how-do-i-get-a-contingency-table>
  #<https://www.youtube.com/watch?v=6dq11srnqjA>

#Running codes for clean test of Prep code
  dev.off() #clear the graph window
  cat('\014') #clear the console
  rm(list = ls()) #clear all user objects from the enviornment 

#Setting the working directory
  setwd("C:/Users/hp/Desktop/IST 687/Homework")
 
  getwd() #know the working directory

#load the titanic dataset
  load("titanic.raw.rdata")

#store it in a dataframe calles "badboat"
  badboat <- data.frame(titanic.raw)

#How does my data look?  
  View(badboat)

#How to make a contingency table in RStudio?  
  library(descr)
  CrossTable(badboat$Sex, badboat$Age)
  help("crossTable")

#Install the following two packages and library them:
  library(arules)
  library(arulesViz)

################## HOMEWORK ##################  
  
#STEP 1
#A]
#Review the data and comment on it!
  View(badboat)
  str(badboat)
#COMMENTS:
#THIS DATASET CONTAINS 4 VARIABLES CLASS, SEX, AGE AND SURVIVED
#STR() COMMAND HELPS US UNDERSTAND THAT CLASS HAS 4 VALUES: 1ST, 2ND, 3RD AND 4TH
#SEX HAS TWO LEVELS FEMALE, MALE
#AGE HAS TWO LEVELS ADULT, CHILD
#SURVIVED HAS TWO LEVELS NO, YES
#THIS DATASET IS NOT A SPARSE MATRIX  
  
#B]  
#Count the number of people in each category of Survived variable
  table(badboat$Survived)
#COMMENT: I CAN SEE THE COUNT OF NUMBER OF PEOPLE WHO SURVIVED AND WHO COULDN'T
  
#C]
#Show the survived numbers in form of percentages (proportions)
  prop.table(table(badboat$Survived))

#D]
#Calculate the numbers for the remaining variables
  prop.table(table(badboat$Class))  #Class
  prop.table(table(badboat$Sex))    #Sex
  prop.table(table(badboat$Age))    #Age
  
#E]
#Creating a contingency table for Age and sex variables
  CrossTable(badboat$Age, badboat$Sex, prop.r = T, prop.c = F, prop.t = F, prop.chisq = F)
#COMMENTS:
#CONTINGENCY TABLE GIVES ME THE VALUES OF AGE VARIABLE AGAINST THE SEX VARIABLE
#THESE VALUES ARE THE COUNT OF CROSS TABULATED VALUES
#PROP.R = TRUE, HELPS US GIVE THE PERCENTAGES (PROPORTIONS) AS WELL
  
#STEP 2
#A]

  badboatX <- as(badboat, "transactions")
  View(badboatX)
#COMMENTS: THE PREDEFINED "as" COMMAND TELL R STUDIO TO CONVERT BADBOAT DATAFRAME TO SPARSE MATRIX
#ALSO, THE NAME "transactions" TELLS R STUDIO TO COERCE OUR OBJECT INTO TRANSACTION CLASS.

#B]
  
  inspect(badboatX)
#inspect COMMAND HELP US TO VIEW US THE SPARSE MATRIX LIKE WE VIEW THE DATAFRAME
#IT PRINTS THE COLUMN NAME, COLUMN VALUE AND THE TRANSACTION ID GENERATED FOR IT REPECTIVELY.
  
  itemFrequency(badboatX)
#itemFrequency COMMAND GIVES US THE PERCENTAGE (PROPORTIONS) OF OCCURENCE OF EACH UNIQUE VALUE WITH RESPECT  TO
#THE COLUMN TO WHICH THEY BELONG
  
  itemFrequencyPlot(badboatX)
#itemFrequencyPlot PLOTS THE HISTOGRAM OF PROPORTIONS OF ALL THE UNIQUE VALUES WE GET BY itemFrequency COMMAND.  

#C]
#View Sparse Matrix created by us
  View(badboatX)
#COMMENTS:
#INSTEAD OF DISPLAYING THE ENTIRE MATRIX, VIEW COMMAND RETURNS JUST THE INFORMATION ABOUT THE DATA
#IT RETURNS THREE VALUES: DATA, ITEMINFO AND ITEMSETINFO
#DATA: TELLS US THE DIMENSIONS OF THE MATRIX AND SOME MORE INFORMATION ABOUT IT.
#ITEMINFO: TELLS US THE LABELS OF VALUES OF MATRIX, VARIABLE NAME AND COUNT, COMBINED TOTAL OF LEVELS.
#ITEMSETINFO: GIVES US COUNT OF TRANSACTION ID AND ITS UNIQUE VALUES.
  
#D]
#Diffrence between badboat and badboatX:
#COMMENTS:
#badboat is a dataframe with 4 columns (4 variables) which contains the actual values of
#the levels of that column
#badboatX is the sparse transaction matrix with 10 columns (4 variables) which contains 
#value either 0 or 1 representing the presence or absence of that entity.
  
#STEP 3
#A]
#APPLY apriori COMMAND ON badboatX MATRIX TO GET THE TRANSACTION RULES FOR THIS DATA  
  ruleset <- apriori(badboatX,
#DEFINING MINIMUM VALUE OF SUPPORT AND CONFIDENCE FOR A RULE TO BE VALID                     
    parameter = list(support = 0.005, confidence = 0.5),
#DEFINING THE LHS AND RHS, i.e. DEPENDENT AND CONDITIONS RESPECTIVELY                     
      appearance = list(default = "lhs", rhs = ("Survived=Yes")))

#B] 
  inspect(ruleset)
  
#C]
  inspectDT(ruleset)
  
#D]
#USING BOTH inspect AND inspectDT COMMAND, WE INFER THAT,
#THE CHANCES OF SURVIVING OF A "2ND CLASS CHILD" WERE THE HIGHEST.
#WE HAVE GENERALIZED THE SEX PART HERE, i.e. IRRESPECTIVE OF THE GENDER BECAUSE,
#EVEN IF LIFT AND CONFIDENCE VALUES WERE SAME FOR FEMALE GENDER AND WITHOUT GENDER,
#THE SUPPORT VALUE IS MORE FOR WITHOUT GENDER.