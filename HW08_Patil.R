############################################################
#IST 687, HW08

#Student Name: Tejas Patil
#Homework Number: HW08
#Date Due: 10/21/2019

# Attribution statement: 
# 1. I did this homework by myself, with help from the book and the professor

#Running codes for clean test of Prep code
dev.off() #clear the graph window
cat('\014') #clear the console
rm(list = ls()) #clear all user objects from the enviornment 

#Setting the working directory
setwd("C:/Users/hp/Desktop/IST 687/Homework")

#download.file("http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls", destfile = localfile.zip)
#Code is ran, gives an error in readxl command and data also not showing in excel file

#library the package
  library(readxl)

#read the downloaded data in df dataframe
  df <- read_excel('mlr01.xls')

#View the dataframe
  View(df)

#Check the structure of dataframe
  str(df)

#Change vague column names to relevant ones 
  colnames(df) <- c('SF_Count', 'Size_AAP', 'A_Preciptn', 'Winter_Index')

#Replace mispelled 3rd column name with the correct one 
  colnames(df)[colnames(df)=="A_Preciptn"] <- "A_Precipitation"

  
############################ HOMEWORK ###############################
 
#library the ggplot2   
  library(ggplot2)
  
#STEP1 A]  
#plotting scatterplot of Number of Fawns vs Adult Antelope Population  
  bivariate1 <- ggplot(data = df, mapping = aes(df$Size_AAP, df$SF_Count))
  bivariate1 <- bivariate1 + geom_point() + xlab("Adult Antelope Population") + ylab("Number of Fawns")
  bivariate1
#THESE VARIABLES HAVE A LINEAR, POSITIVE AND STRONG RELATIONSHIP.


#STEP1 B]    
#plotting scatterplot of Number of Fawns vs Precipitation  
  bivariate2 <- ggplot(data = df, mapping = aes(df$A_Precipitation, df$SF_Count))
  bivariate2 <- bivariate2 + geom_point() + xlab("Precipitation") + ylab("Number of Fawns")
  bivariate2
#THESE VARIABLES ALSO HAVE A LINEAR, POSITIVE AND STRONG RELATIONSHIP.
  

#STEP1 C]    
#plotting scatterplot of Number of Fawns vs Winter Index  
  bivariate3 <- ggplot(data = df, mapping = aes(df$Winter_Index, df$SF_Count))
  bivariate3 <- bivariate3 + geom_point() + xlab("Winter Index") + ylab("Number of Fawns")
  bivariate3
#THESE VARIABLES HAVE A LINEAR, NEGATIVE AND STRONG RELATIONSHIP.
  
  

#STEP2 A]  
#APPLYING LINEAR REGRESSION MODEL ON 'df' DATASET, SF_Count = Y variable and all others as X-variables  
  LRmodel <- lm(SF_Count ~., data = df)


#STEP2 B]    
#TO GET R-SQUARED VALUE, CHECKING THE SUMMARY OF MODEL  
  summary(LRmodel)  

#THE R-SQUARED VALUE FOR OUR LINEAR REGRESSION MODEL IS = 0.9743, BUT FOR MULTIPLE VARIABLES
#WE CONSIDER ADJUSTED R-SQUARED, WHICH IS EQUAL TO:- 0.955

#SIGNIFICANCE OF ADJUSTED R-SQUARED(0.955)
# 95.5% OF VARIATION IN "SF_Count" IS EXPLAINED BY VARIATION IN X VARIABLES 
# HERE X VARIABLES ARE - "Winter_Index", "A_Precipitation", "Size_AAP"
  

#STEP2 C]      
#STATISTICALLY MOST SIGNIFICANT VARIABLE IS - "A_Precipitation" (p-value - 0.217)
#WE CHECK FOR p-values OF EACH VARIABLE IN SUMMARY OF THE REGRESSION MODEL
#THE VARIABLE WITH LEAST p-value IS THE MOST SIGNIFICANT VARIABLE
  

  
#STEP3 A]  
#OVERALL INTERPRETATION OF THE MODEL
   #1 LINEAR REGRESSION MODEL WITH 3 PREDICTORS AND 1 NUMERICAL CONTINUOUS DEPENDENT VARIABLE
   #2 THE MODEL IS 95.5% ACCURATE (ADJUSTED R-SQUARED - 0.955)
   #3 ALL 3 PREDICTORS ARE SIGIFICANT AS ALL HAVE p-values BELOW 0.05


#STEP3 B]    
#MULTIPLE REGRESSION EQUATION:
#  Y(SF_Count) = -5.92201 + 0.33822(Size_AAP) + 0.40150(A_Precipitation) + 0.26295(Winter_Index)

#INETRPRET THE EQUATION:
  
#  INTERCEPT: WHEN ALL THE X VARIABLES ARE 0, THEN Y IS PREDICTED TO BE EQUAL TO THE INTERCEPT VALUE

#  Size_AAP COEFFICIENT:        KEEPING ALL OTHER PREDICTORS CONSTANT, 1 UNIT INCREASE IN Size_AAP 
#                               IS PREDICTED TO INCREASE THE "SF_Count" BY 0.33822
  
#  A_precipitation COEFFICIENT: KEEPING ALL OTHER PREDICTORS CONSTANT, 1 UNIT INCREASE IN A_precipitation 
#                               IS PREDICTED TO INCREASE THE "SF_Count" BY 0.40150
  
#  Winter_Index COEFFICIENT:    KEEPING ALL OTHER PREDICTORS CONSTANT, 1 UNIT INCREASE IN Winter_Index 
#                               IS PREDICTED TO INCREASE THE "SF_Count" BY 0.26295