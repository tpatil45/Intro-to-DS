############################################################
#IST 687, HW10

#Student Name: Tejas Patil
#Homework Number: HW10
#Date Due: 11/06/2019

# Attribution statement: 
# 1. I did this homework by myself, with help from the book and the professor

#Running codes for clean test of Prep code
  dev.off() #clear the graph window
  rm(list = ls()) #clear all user objects from the enviornment 
  cat('\014') #clear the console
  
#Setting the working directory
  setwd("C:/Users/hp/Desktop/IST 687/Homework")

#library ggplot2 for getting the dataset
  library(ggplot2)

#library kernlab for the svm functions
  library(kernlab)

#import the data from ggplot2 library
  data("diamonds")

#load the data into a dataframe
  df <- data.frame(diamonds) 

#Observe that there will be 5 values in cut variable of dataset:
#Fair, Good, Very Good, Premium and Ideal
  View(df)
  str(df$cut)

#create a subset with only premium and  Ideal cut diamonds
  goodDiamonds <- df[which(df$cut == "Premium" | df$cut == "Ideal"), ]

#Viewing the new dataset
  View(goodDiamonds)

#Verifying if the dataset has been properly subsetted from the df dataframe
  table(df$cut)
  dim(goodDiamonds)

#Convert clarity and color variables to numeric form
  goodDiamonds$clarity <- as.numeric(goodDiamonds$clarity)
  goodDiamonds$color <- as.numeric(goodDiamonds$color)

#Verifying the the conversion of datatype of variables
  View(goodDiamonds)
  str(goodDiamonds)

#Fixing the additional level issue in "cut" column
  goodDiamonds$cut <- as.factor(as.character(goodDiamonds$cut)) 
  str(goodDiamonds)
  View(goodDiamonds)

#Describing the variables:
  help("diamonds")
#1 CARAT - THIS VARIABLE CONTAINS THE WEIGHT OF DIAMONDS. RANGE(0.2 - 5.01)
#2 CUT   - THIS TELLS US THE QUALITY OF CUTTING WORK DONE OVER DIAMOND (PREMIUM OR IDEAL)
  unique(goodDiamonds$color)
#3 COLOR - IT GIVES THE INFO ABOUT COLOR OF DIAMOND, 1(BEST) TO 7(WORST)
  unique(goodDiamonds$clarity)
#4 CLARITY - IT GIVES THE INFO ABOUT CLARITY OF DIAMOND, 1(WORST) TO 8(BEST)
#5 X     - THIS VARIABLE TELLS US THE LENGTH OF DIAMOND IN MM. RANGE(0 - 10.74)
#6 Y     - THIS VARIABLE TELLS US THE WIDTH OF DIAMOND IN MM. RANGE(0 - 58.9)
#7 Z     - THIS VARIABLE TELLS US THE DEPTH OF DIAMOND IN MM. RANGE(0 - 31.8)
#8 DEPTH - THIS VARIABLE HAS THE CALCULATED VALUES OF TOTAL DEPTH PERCENTAGE
#          total depth percentage = z / mean(x, y) = (2*z*100) / (x + y) RANGE(43-79)
#9 TABLE - THIS VARIABLE GIVES THE WIDTH OF TOP SECTION OF DIAMOND RELATIVE TO THE WIDEST
#          SECTION IN THAT DIAMOND
#10 PRICE - THIS VARIABLE HAS THE PRICE OF THAT DIAMOND. CURRENCY: US DOLLARS RANGE($326 - $18,823)


#Confusion Matrix:
#Confusion matrix is used to measure the performance of classification models where,
#dependent variable can have two or more elements. Considering the case with 2 elements
#in the dependent variable, confusion matrix will have four values. False-Positive,
#False-Negative, True-Positive and True-Negative.
#What it will do is compare the predicted values with the actual dependent values of
#test dataset and tell us:
#1 How many correct values predicted as correct values(True-Positive)
#2 How many wrong values predicted as correct values(False-Positive)
#3 How many correct values predicted as wrong values(True-Negative)
#4 How many wrong values predicted as wrong values(False-Negative)

#Theoretical process:
#table(predicted values, actual values(test dataset))
#We make a contingency table of two variables.
#One variables has the values of dependent variable predicted by our classification model.
#Second variable will have the actual values of dependent variable of test dataset.
#table() command give us the count of values predicted correctly and wrongly with respect
#to the actual values of that variable. 

  
  
    
############## HOMEWORK ################  
  
  
  
#STEP 1
#A]
#Re-run the entire PE code
  
#B]
  table(goodDiamonds$cut)  
#OBSERVATION: IDEAL CUT DIAMONDS - 21551; PREMIUM CUT DIAMONDS - 13791
  
#STEP 2
#A]
#creating a random set of indices  
  trows <- nrow(goodDiamonds)
  RIndices <- sample(1:trows)
  summary(RIndices)  
  head(RIndices)  
 
#B]   
#create a training and testing dataset
  trainD <- goodDiamonds[RIndices[1 : round((2/3)*trows)] , ]
  testD  <- goodDiamonds[RIndices[(round((2/3)*trows) + 1) : trows] , ]  
  
#C] Check the dimensions for training data and test data. 
  dim(trainD)
  dim(testD)


#STEP 3      
#A]  
#Applying the SVM model on the training dataset
  svmOutput <- ksvm(cut~., data = trainD, kernel = "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)

#B] 
#Explanation of parameters.  
#cut~.
#specify the dependent variable.
# ~ implies that specify independent variables after this point.
# . implies that all the variables except dependent variables are selected as
#independent for classification.  
  
#data = goodDiamond:
#Input training dataset for the svm model.   
    
#Kernel = "rbfdot":
#1 Helps Project lower dimensional data into higher dimension.
#2 rbfdot refers to the radial basis function.
#3 radial basis means something like distance from origin to any point is radius of a circle.
#4 dot mean the dot product means dot product of vectors to calculate distance(suppose to be maximum).
#5 In short, kernel takes set of inputs from each row and calculated distance value based on combination of many variables in that row.
#6 The weightage of variables is adjusted by algorithm to get maximum distance seperation between spam and non-spam.
  
#kpar:
#1 automatic, designer dependent.
  
#C Value:
#known as "Cost of constraints".
#Large value: Small margin, Few mistakes, specific model(specific to train data), weird shape and slope.
#Small value: Large margin, more mistakes, generalizable model (applicable to all models), proper seperation.
  
#Cross and prob.model = TRUE
#Cross is nothing but k-fold cross validation.
#Prob.model says we use cross validation to generate probabilities for determining spam or no spam.
#Cross validation necessary because sometimes there is a risk of overfitting.
#model becomes too specific for a dataset. It cannot be generalized. So, cross is used.
#k-fold cross validation will divide the training data set into 'k' parts.
#Then it will fit the model on k-1 sets and validate the model on remaining set.
#It will repeat this process k times, each time the validation set will be diffrent.
  
  
#C] Echoing the model output in console 
  svmOutput

    
#STEP 4
#A]
#Validating the model against the test dataset  
  svmPred <- predict(svmOutput, testD)
  
#B]
#The object svmPred now contains a list of either "Premium" or "Ideal" cuts.  

#C]
#Reviewing the contents of svmPred  
  head(svmPred)
  str(svmPred)
  View(svmPred)

  
#STEP 5
#A]
#Creating a confusion matrix 
  cmat <- table(svmPred, testD$cut)

#B]
#Calculating the error percentage
  (cmat[2,1]+cmat[1,2])*100 / (cmat[1,1]+cmat[1,2]+cmat[2,1]+cmat[2,2]) 
  
  
  
#C] How good is the model?
#Another way to create confusion matrix
  library(caret)
  cMatrix <- confusionMatrix(svmPred, testD$cut)
  cMatrix
  
#THE MODEL WHICH WE HAE BUILT IS 91.69% ACCURATE.
#ALSO, THE NO INFORMATION RATE IS 60.84%, SO OUR ACCURACY IS BETTER THAN NO INFORMATION
#RATE.
#WE ARE 95% SURE THAT OUR ACCURACY WILL ALWAYS BE BETWEEN (0.9118, 0.9218) FOR TEST DATA SET.
  
  
  
#STEP 6
#WHEN BUILDING THE MACHINE LEARNING MODELS, IF WE RUN THE SAME MODEL OVER THE TRAINING 
#DATA, THERE IS A HIGH CHANCE OF WHAT WE CALL OVERFITTING.
#IT MEANS THAT THE MODEL WILL BECOME VERY SPECIFIC TO THE TRAINING DATA SET, AND WE WON'T
#BE ABLE TO GENERALIZE OUR MODEL
  
#HENCE, IT IS VERY IMPORTANT TO TEST OUR MODEL ON A DATA THAT OUR MODEL HAS NOT SEEN YET.
#SO, WE HAVE TO USE TEST DATA SET.
    