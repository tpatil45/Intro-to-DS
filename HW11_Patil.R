############################################################
#IST 687, HW11

#Student Name: Tejas Patil
#Homework Number: HW11
#Date Due: 11/21/2019

# Attribution statement: 
# 1. I did this homework by myself, with help from the book and the professor

#Running codes for clean test of Prep code
dev.off() #clear the graph window
rm(list = ls()) #clear all user objects from the enviornment 
cat('\014') #clear the console


#Install the "tm" package and library it
  #install.packages("tm")
  library(tm)

#Setting the working directory
  setwd("C:/Users/hp/Desktop/IST 687/Homework")  
  
#Reading the text file using scan function  
  charVector <- scan("speech.txt", character(0), sep = "\n")  
  
#Scanning the txt files containing positive and negative words
#Files contain characters and not integers, and are separated by the line
  posWords <- scan("Positive.txt", character(0), sep = "\n")
  negWords <- scan("Negative.txt", character(0), sep = "\n")

#Checking the unimportant data given at the beginning of the words files#
  head(posWords, 35)
  head(negWords, 35)
  
#Removing the useless rows of data
  posWords <- posWords[-(1:34)]
  negWords <- negWords[-(1:34)]
  
#Verifying the corrected data
  posWords[1]
  negWords[1]
  
#Examine charVector using the head() and summary() commands.
  head(charVector)
  summary(charVector)

#Head: Head returns the first six documents of the charVector.
#As we had specified, the data type was not integers, and it has returned us the textual data.
#Also, the values (here documents) are separated by "/n" which means new line.
  
#Summary: This function returns three values.
#First is the length of the vector, second and third values are class and mode respectively.
#Here, both class and mode are "character".
  

#TERM DOCUMENT MATRIX:  
#The term document matrix is a rectangular data structure.
#The rows contains the terms, here terms means "words".
#The columns contain documents, meaning that the specific word in a specific row has occurred how many times in that document, so basically, the count of the word in that document.
#They are also known as "document term matrix".

  
#creating all the documents from speech  
  words.vec <- VectorSource(charVector)
#Creating word corpus
  words.corpus <- Corpus(words.vec)
  words.corpus
  
#making all the selected terms in lower case
  words.corpus <- tm_map(words.corpus, content_transformer(tolower))
  
#Removing the punctuation from terms
  words.corpus <- tm_map(words.corpus, removePunctuation)
  
#Removing numerical data from the terms
  words.corpus <- tm_map(words.corpus, removeNumbers)
  
#Removing the stopwords from textual data, 
#stop words meaning small and useless words, the words which won't help in text mining
#e.g. the, a, an, etc.
  words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
  
#Creating a TermDoumentMatrix from the words.corpus variable.
  tdm <- TermDocumentMatrix(words.corpus)
  tdm
  
#creating a summary of the term document matrix
  inspect(tdm)

#Explanation of the output of the inspect() function  
    
#Inspect: This function tells us the information about our "term documention matrix".
# TermDocumentMatrix (terms: 1211, documents: 166)>>
#This tells us that there are 1211 words (rows) in the matrix and 166 documents (columns).
  
#Non-/sparse entries: 2574/198452
#This ratio has numerator as number of non zero cells in matrix.
#In the denominator, it has the total number of cells in matrix.
  
#Sparsity           : 99%
#This tells us the percentage of cells in matrix that has value as zero.
  
#Maximal term length: 18
#Length of the longest word (term).
  
#Weighting          : term frequency (tf)
#It tells us the what kind of values every cell contains, here term frequency means all the cells contain the count of terms in every document.
  
#Sample: It shows us the picture of a part of term documentation matrix. 
  
  
  
  
######################## HOMEWORK ##########################
  

#STEP 1
#A]
#Creating a named list of word counts by frequency
  
#To work on text data we will have to convert it to plain data matrix
  Matt <- as.matrix(tdm)
  wordCounts <- rowSums(Matt)
  wordCounts <- sort(wordCounts, decreasing = TRUE)    

#B]
#Explain the output of head() command
  head(wordCounts)
#head() FUNCTION HERE DISPLAYS THE FIRST SIX WORDS WHICH ARE USED MOST NUMBER OF TIMES
#IN THE SPEECH

#C]
#Count how many unique words are there in the speech(except stop words)
  length(wordCounts)
#THERE ARE 1211 UNIQUE WORDS IN SPEECH, AND WE USED THE LENGTH COMMAND TO GET THAT VALUE
  
#D]
#COUNT THE TOTAL NUMBER OF WORDS IN THE SPEECH
  sum(wordCounts)
#THERE ARE TOTAL OF 2762 WORDS IN SPEECH AND WE USE SUM COMMAND TO COUNT THEM
  
  

#STEP 2
#A]
#Match the words from the speech to the list of positive words
  matchedP <- match(names(wordCounts), posWords, nomatch = 0)
  View(matchedP)

#B]
#Match the words from the speech to the list of negative words
  matchedN <- match(names(wordCounts), negWords, nomatch = 0)
#Explanation of the code:
#WE MATCH ALL THE WORDS (names()) IN WORDCOUNTS TO THE WORDS IN NEGWORDS, AND 
#WORDS THAT DONT MATCH WILL BE TERMED AS "0"
  
#C]  
#IT THE RETURNS US THE INDEX NUMBER OF MATCHED WORD FROM POSWORDS, 
#THE LENGTH OF THAT VECTOR IS EQUAL TO THE LENGTH OF WORDCOUNTS,
# i.e. TOTAL NUMBER OF UNIQUE WORDS IN SPEECH

#THE WORDS THAT DONT MATCH WILL HAVE THE VALUE AS ZERO, 
#THE WORDS FROM WORDCOUNTS THAT DO MATCH WITH POSWORDS, WILL HAVE THE VALUE OF
#INDEX NUMBER OF THAT WORD IN POSWORDS
    
  
  
#STEP 3
#A]
#library ggplot2  
  library(ggplot2)

#Creating a dataframe with all the words, frequencies and the matched indices    
  Senti_Alys <- data.frame(Words = names(wordCounts), Frequency = wordCounts, Positive_Matched = matchedP,Negative_Matched = matchedN)
  View(Senti_Alys)
#Delete useless metadata  
  rownames(Senti_Alys) <- NULL
  
#Create a dataframe of all the positive matched words  
  Positive_Words <- Senti_Alys[Senti_Alys$Positive_Matched > 0, -which(colnames(Senti_Alys) == "Negative_Matched")]
#Create a dataframe of all the positive matched words
  Negative_Words <- Senti_Alys[Senti_Alys$Negative_Matched > 0, -which(colnames(Senti_Alys) == "Positive_Matched")]

#Plot a bar chart for all the positive matches  
  ggplot(data = Positive_Words,
    mapping = aes(reorder(Words, -Frequency), Frequency)) +
      geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90))


#B]
#Plot the bar chart for the top 20 negative matches
#Since the Negative words is already a sorted data by decreasing order, because we
#had sorted wordCounts, so we have top 20 values as the first 20 values,
#so we only reorder it by frequency   
  ggplot(data = Negative_Words[1:20,], mapping = aes(reorder(Words, -Frequency), Frequency)) +
    geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=90))
  

#C]    
#Creating a bar chart for positive matches with frequency at least = 2
  ggplot(data = Positive_Words[which(Positive_Words$Frequency > 1),],
    mapping = aes(reorder(Words, -Frequency), Frequency)) +
      geom_bar(stat = "identity") +
        theme(axis.text.x=element_text(angle=90))
  
#Creating a bar chart for negative matches with frequency at least = 2  
  ggplot(data = Negative_Words[which(Negative_Words$Frequency > 1),],
    mapping = aes(reorder(Words, -Frequency), Frequency)) +
      geom_bar(stat = "identity") +
        theme(axis.text.x=element_text(angle=90))


#D]  
#Count the total number of positive words in speech  
  Pcount <- sum(Positive_Words$Frequency)
#Count the total number of negative words in speech  
  Ncount <- sum(Negative_Words$Frequency)

#Count the total number of words in the speech    
  Tcount <- sum(wordCounts)

#Ratio of positive words to total words  
  Pratio <- Pcount/Tcount
  Pratio
#Ratio of negative words to total words  
  Nratio <- Ncount/Tcount
  Nratio  
  
#Conslusion: Since Pratio (216 words) is exactly double the Nratio (108 words),
#we can conclude that our speech was a positive speech.  