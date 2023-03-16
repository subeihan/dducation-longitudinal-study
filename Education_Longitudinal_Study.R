# Part I - Homework 1
# Section 1: Load Data Set and Preliminary Preparation of the Data
# Section 1-a: Load Data
loadData = function(csvfile){
  read.csv(csvfile, header=T, sep=',', stringsAsFactors=F)
}# Function to load data in csv format

rawData =loadData("els_02_12_byf3pststu_v1_0.csv") # Load data
dim(rawData) # Check the number of rows and columns of the data

# Section 1-b: Remove Identification Variables, Weight Variables and Questionnaire Variables
idValIdx = c(1:15) # Column indices for Identification Variables
wtValIdx = c(16:17,185:188,292,555:558,850:855,980:982,1060:1067) # Column indices for Weight Variables
qsValIdx = c(1235:4012) # Column indices for Questionnaire Variables
rmValIdx = c(idValIdx, wtValIdx, qsValIdx) # Combine column indices for all three types of variables
length(rmValIdx)
prepData = rawData[, -rmValIdx] # Remove the variables of the three types
dim(prepData) # Check the number of rows and remaining columns of the data


# Section 1-c: Remove imputation flag variables
library("stringr")
imputeColIdx = which(str_detect(colnames(prepData), regex("IM$")))
colnames(prepData)[imputeColIdx]
prepData = prepData[,-imputeColIdx]
dim(prepData) # Check the number of rows and columns of the data set


# Section 1-d: Check and remove suppressed variables; 
# According to the code book, for each variable the negative values indicates the following:
# -1 Don't know 
# -2 Refused
# -3 Item legitimate skip/NA 
# -4 Nonrespondent
# -5 Suppressed
# -6 Multiple response
# -7 Not administered; abbreviated interview or breakoff
# -8 Survey component legitimate skip/NA
# -9 Missing

searchSupprCols = function(data){
  colIdx = c()
  for (col in 1:ncol(data)){
    table = table(data[,col])
    if (dim(table) == 1 && names(table)[1] == "-5"){
      colIdx = c(colIdx, col)
    }
  }
  return(colIdx)
} # Function to find all suppressed columns

supprColIdx = searchSupprCols(prepData) # Find all suppressed variables
length(supprColIdx) # Check how many variables are suppressed
prepData = prepData[,-supprColIdx] # Remove suppressed variables
dim(prepData) # Check the number of rows and columns of the data set


fix(prepData)

# Section 1-e: Remove variables for which more than 20% of the rows have negative values (-1~-9)
# Function to count certain values for each column
valueCount = function(data, valueSeq){
  dataFrame = data.frame(valueSeq)
  colnames(dataFrame) = "Value"
  for (col in 1:ncol(data)){
    counts = c()
    for (val in valueSeq){
      currCount = 0
      for (row in 1:nrow(data)){
        if (data[row, col] == val){
          currCount = currCount + 1
        }
      }
      counts = c(counts, currCount)
    }
    
    currDataFrame = data.frame(counts)
    colnames(currDataFrame) = colnames(data)[col]
    dataFrame = cbind(dataFrame, currDataFrame)
  }
  return(dataFrame)
}

valueSeq = seq(from=-1,to=-9,by=-1) # A sequence of negative values
valCountDF = valueCount(prepData,valueSeq) # Create a data frame for the counts for negative values in each column
fix(valCountDF) # Review the data frame created
write.csv(valCountDF,"NegValCounts.csv") # Sava file for further review
ValCount = valCountDF[, 2:ncol(valCountDF)] # Remove the first column


# Drop columns in which more than 30% of all rows have negative values
treshhold = 0.3 * nrow(prepData)
rmColIdx = which(sapply(ValCount, sum) > treshhold) # Find columns in which more than threshold of all rows have negative values
length(rmColIdx) # Check the number of columns
prepData = prepData[, -rmColIdx] # Remove such columns
dim(prepData) # Check the number of rows and columns of the data set
write.csv(prepData,"prepData.csv")


# Section 2: EDA
# Section 2-a: Preliminary EDA on the Class variable
# Function to do Preliminary EDA on Target Class
edaTarget = function(edaData, targetName, description){
  dim(edaData)
  targetIdx = which(colnames(edaData)==targetName)
  
  # Check distribution of Target
  TBL = table(edaData[,targetIdx])
  print("Frequency Distribution Table:", quote = FALSE)
  print(TBL)
  print("Percentage Distribution Table:", quote = FALSE)
  print(round(TBL/sum(TBL),4))

  # Determine if binary or multi-class classification
  print(ifelse(length(TBL)==2, "Binary Classification", 
               paste("Multi-Class Classification with", length(TBL), "classes")))
  
  # View the distribution of the Target virtually
  hist(edaData[, targetIdx],
       main = "Histogram of Target",
       xlim = c(min(edaData[, targetIdx]) - 5, max(edaData[, targetIdx]) + 5),
       xlab = description,
       ylab = "Frequency",
       col = "grey")
}

# Class Variable: Highest level of education earned as of F3 (third follow-up)
# Variable name: F3ATTAINMENT
edaData = prepData # Copy data from preliminary preparation stage
edaTarget(edaData, "F3ATTAINMENT", "Highest Level of Education") # EDA on target


# Remove rows indicate nonrespondent or skip/NA
rmValSeq = c(-4, -8)
edaData = edaData[-which(edaData$F3ATTAINMENT %in% rmValSeq), ]
dim(edaData)
edaTarget(edaData, "F3ATTAINMENT", "Highest Level of Education")


# Deal with Imbalance
# Function to replace certain values in a column with new value
replaceVals = function(data, colIdx, oldValSeq, newVal){
  for (row in 1:nrow(data)){
    if (data[row, colIdx] %in% oldValSeq){
      data[row, colIdx] = newVal
    }
  }
  return(data)
}


# Group categories to deal with imbalance
table(edaData$F3ATTAINMENT) # Review distribution before grouping
class1Vals = c(1,2,3) # Values indicate no postsecondary credential
class0Vals = c(4,5,6,7,8,10) # Values indicate some postsecondary credential
targetIdx = which(colnames(edaData) == "F3ATTAINMENT")
edaData = replaceVals(edaData, targetIdx, class1Vals, 1) # Replace Values indicating no postsecondary credential with 1
edaData = replaceVals(edaData, targetIdx, class0Vals, 0) # Replace Values indicating some postsecondary credential with 0

edaTarget(edaData, "F3ATTAINMENT", "Highest Level of Education")
table(edaData$F3ATTAINMENT) # View distribution after grouping
dim(edaData)

# Remove other F3 Variables and beyond
#library("stringr")
F3Features = which(str_detect(colnames(edaData[,-targetIdx]), regex("^F3")))
colnames(edaData[,-targetIdx])[F3Features]
length(F3Features) # Check number of columns to be removed
edaData = cbind(edaData$F3ATTAINMENT, edaData[,-targetIdx][, -F3Features]) # Rearrange columns so that the class variable is in column 1
colnames(edaData)[1] = "F3ATTAINMENT" # Change the name of the first column back to "F3ATTAINMENT"
targetIdx = which(colnames(edaData) == "F3ATTAINMENT")
dim(edaData) # Check the number of rows and columns of the data set
write.csv(edaData, "beforeFeatures.csv")



# Section 2-b: The features
# Overview of the features
dim(edaData[,-1])

# Find and remove constant features
# Function to find and remove columns with constant values
findRmoveConstCol = function(data){
  newData = data
  count = 0
  colIdx = c()
  for (col in 1:ncol(data)){
    table = table(data[,col])
    if (dim(table) == 1){
      count = count + 1
      colIdx = c(colIdx, col)
    }
  }
  
  if (count == 0){
    print("There is no constant features")
  }
  else if (count == 1){
    print(paste("There is", count, "constant feature", "at column" ,colIdx[1], sep =" "))
  }
  else if (count > 1){
    print(paste("There are", count, "constant feature", sep =" " ))
  }
  
  if (count > 0){
     newData = data[, -colIdx]
     print("Constant features have been removed")
  }
  return(newData)
}

edaData = findRmoveConstCol(edaData) # Find and remove constant features in our data set
dim(edaData) # Check the number of rows and columns of the remaining data

# Check type of each feature
#types = sapply(edaData, typeof)
#table(types)
#which(types == "character")
# Function to replace certain values in a column with new values
#batchReplaceVals = function(data, colIdx, oldValSeq, newValSeq){
  #for (row in 1:nrow(data)){
    #data[row, colIdx] = newValSeq[which(oldValSeq == data[row, colIdx])]
  #}
  #return(data)
#}

# Check for Pair-Wise Collinearity
corMatrix = cor(edaData[,-1]) # create correlation matrix (column 1 is class variable)
print(ifelse(any(abs(corMatrix[corMatrix!=1])>0.75),
             "Correlated Features Exist", "No Correlated Features"))

#install.packages('caret')
if (!require(caret)) {install.packages("caret")}
library(caret)
correlatedCol = findCorrelation(corMatrix, cutoff = 0.75) + 1 # Find features to be removed to reduce pair-wise correlations
correlatedColNum = length(correlatedCol) # Check number of feature to be removed
print(paste("Number of correlated features =", correlatedColNum))

edaData = edaData[, -correlatedCol] # Remove correlated features
dim(edaData) # Check the number of remaining rows and columns of the data set



# Check for multicollinearity
# Fit a glm model on all features
glmModel = glm(F3ATTAINMENT~ ., data = edaData, family = "binomial")
#summary(glmModel)


library(car)
vifs = vif(glmModel) # Calculate VIFs of all the features

# Function to summarize the VIFs
vifSummary = function(vifs){
  min = min(vifs)
  max = max(vifs)
  low = floor(min)
  high = ceiling(max)
  print(paste("Minmum VIF: ", min), quote = F)
  print(paste("Maximum VIF: ", max), quote = F)
  counts = vector(mode = "integer", length = 6)
  for (vif in vifs){
    if (vif >= 0 && vif <= 1) counts[1] = counts[1] + 1
    else if(vif > 1 && vif <= 2) counts[2] = counts[2] + 1
    else if(vif > 2 && vif <= 3) counts[3] = counts[3] + 1
    else if(vif > 3 && vif <= 4) counts[4] = counts[4] + 1
    else if(vif > 4 && vif <= 5) counts[5] = counts[5] + 1
    else counts[6] = counts[6] + 1
  }
  
  for (i in 1:5){
    print(paste("Count of VIF in range (", i-1, ", ", i, "]: ", counts[i], sep =""), quote = F)
  }
  print(paste("Count of VIF > 5: ", counts[6], sep =" "), quote = F)
}

vifSummary(vifs) # Produce a summary of the VIFs we calculated on all the features
#which(vifs > 5)

# Function to determine whether multicollinearity exits and return a list of features that have higher VIF than the cutoff value.
multicollinearity = function(vifs, cutoff){
  multiCollinearityCols = c()
  
  for (i in 1:length(vifs)){
    if (vifs[i] > cutoff){
      multiCollinearityCols = c(multiCollinearityCols, i)
    }
  }
  
  if (length(multiCollinearityCols) > 0){
    print("Multicollinearity exits, a list of the affected columns is returned")
  }
  return(multiCollinearityCols)
}

multiCollinearityCols = multicollinearity(vifs, 5) # Detect if multicollinearity exits in our data set using cutoff value of 5
length(multiCollinearityCols) # Check number of features that have higher VIF than the cutoff value
colNames = names(vifs[multiCollinearityCols])
print(paste(colNames))
edaData = edaData[, -which(colnames(edaData) %in% colNames)] # Remove features that have higher VIF than the cutoff value
dim(edaData) # Check the number of remaining rows and columns of the data set

# Exam remaining features
featureNames = data.frame(colnames(edaData[, -1]))
write.csv(featureNames, "featureNames.csv")



# Determine Variable Importance
# Separate the data set into training data set and testing data set
data = edaData # copy data from EDA stage
set.seed(43)
trainDataIdx = sample(1:nrow(data), 0.7 * nrow(data), replace = FALSE) # Get indices for 70% of the total number of observations
trainData = data[trainDataIdx, ] # Define training data set
testData = data[-trainDataIdx, ] # Define testing data set
table(trainData$F3ATTAINMENT) # Frequency distribution table of the class variable in the training data set
table(trainData$F3ATTAINMENT)/sum(table(trainData$F3ATTAINMENT)) # Percentage distribution
table(testData$F3ATTAINMENT)  # Frequency distribution table of the class variable in the testing data set
table(testData$F3ATTAINMENT)/sum(table(testData$F3ATTAINMENT)) # Percentage distribution

# Fit a glm model with training data set
glmModel = glm(F3ATTAINMENT~ ., data = trainData, family = "binomial")
summary(glmModel)

# Calulate and Plot Variable Importance
if (!require(caret)) install.packages("caret")
library(caret)
valImportance = varImp(glmModel, scale = FALSE)
print(valImportance)
plot(valImportance, main = "Variable Importance")

ValImpSortedIdx = order(valImportance[,1], decreasing = TRUE) # Sort the variables by variable importance.
top10Features = row.names(valImportance)[ValImpSortedIdx[1:10]] # Get a list of names of the top 10 important features
top15Features = row.names(valImportance)[ValImpSortedIdx[1:15]] # Get a list of names of the top 15 important features
top20Features = row.names(valImportance)[ValImpSortedIdx[1:20]] # Get a list of names of the top 20 important features
top25Features = row.names(valImportance)[ValImpSortedIdx[1:25]] # Get a list of names of the top 25 important features
print(paste("Top 10 features are =", paste(top10Features, collapse=", ")))


# Section 3. Logistic Regression
# Section 3-a: Helper Functions
# Function to test Overfitting
isOverfitting = function(trCfm, tstCfm)
{
  # If the accuracy from training is much more than accuracy testing phase
  # then we conclude Model is overfitting 
  # unable to generalize
  trOverall = data.frame(trCfm$overall)
  tstOverall = data.frame(tstCfm$overall)
  trAcc = trOverall[which(row.names(trOverall) == "Accuracy"), 1]
  tstAcc = tstOverall[which(row.names(tstOverall) == "Accuracy"), 1]
  return(trAcc > 2 * tstAcc)
}

# Function to test Underfitting
isUnderfitting = function(trCfm)
{
  # If the accuracy from training is much more than accuracy testing phase
  # then we conclude Model is overfitting 
  # unable to generalize
  trOverall = data.frame(trCfm$overall)
  trAcc = trOverall[which(row.names(trOverall) == "Accuracy"), 1]
  return(0.5 > trAcc)
}

# Logistic regression model ROC
logisticROC = function(predResponse, actlClass, thresholds, title){
  TPRates = c()
  FPRates = c()
  
  for (threshold in thresholds){
    predClass = ifelse(predResponse > threshold, 1, 0)
    cfmTable = table(predClass, actlClass)
    
    if (dim(cfmTable)[1] == 2){
      TPRates = c(TPRates, cfmTable[1,1]/sum(cfmTable[,1]))
      FPRates = c(FPRates, cfmTable[1,2]/sum(cfmTable[,2]))
    }
    else if (as.numeric(row.names(cfmTable)) == 0) {
      TPRates = c(TPRates, 1)
      FPRates = c(FPRates, 1)
    }
    else if (as.numeric(row.names(cfmTable)) == 1){
      TPRates = c(TPRates, 0)
      FPRates = c(FPRates, 0)
    }
  }
  
  if(!require(pracma)) {install.packages('pracma')}
  library(pracma)
  auc = trapz(FPRates, TPRates)
  
  plot(FPRates, TPRates, 
       type = "l", 
       col = "red", 
       xlab = "False Positive Rate", 
       ylab = "True Positive Rate", 
       main = title)
  axis(1, seq(0.0, 1.0, 0.1))
  axis(2, seq(0.0, 1.0, 0.1))
  abline(h=seq(0.0, 1.0, 0.1), v=seq(0.0, 1.0, 0.1), col="gray", lty=3)
  legend(0.7, 0.3, sprintf("%3.3f",auc), lty=c(1,1), lwd=c(2.5,2.5), col="red", title = "AUC")
}

# Section 3-b: Benchmark: glm model with different number of features
# Function to run glm model on selected features
runAndEvalGlm = function(className, featureNames, trainData, testData, threshold){
  if (!require(caret)) {install.packages("caret")}
  library(caret)
  
  formulaStr = paste(className,"~", paste(featureNames, collapse="+"))
  glmModel = glm(formulaStr, data = trainData, family = "binomial")
  summary(glmModel)
  
  glmTrPred = predict(glmModel, trainData[, -which(colnames(trainData) == className)], type = "response")
  glmTrCfmTbl = table(ifelse(glmTrPred > threshold, 1, 0), 
                      trainData[, which(colnames(trainData) == className)])
  glmTrCfm = confusionMatrix(glmTrCfmTbl)
  print("glm Model Confusion Matrix and Statistics for Training Phase")
  print(glmTrCfm)
  
  glmTstPred = predict(glmModel, testData[, -which(colnames(testData) == className)], type = "response")
  glmTstCfmTbl = table(ifelse(glmTstPred > threshold, 1, 0), 
                       testData[, which(colnames(testData) == className)])
  glmTstCfm = confusionMatrix(glmTstCfmTbl)

  print("glm Model Confusion Matrix and Statistics for Generalization Phase")
  print(glmTstCfm)

  print(ifelse(isUnderfitting(glmTrCfm),"This model is deficient",
         paste("There is no underfitting. ", 
               "This model is an effective learner with accuracy of [", 
               glmTrCfm$overall['Accuracy'], "]",
               sep = "")))

  
  print(ifelse(isOverfitting(glmTrCfm, glmTstCfm), "The model is overfitting.",
         paste("There is no overfitting. This model is an effective learner with training accuracy of [",
               glmTrCfm$overall['Accuracy'], 
               "] and testing accuracy of [",
               glmTstCfm$overall['Accuracy'], "]",
               sep = "")))

  treshholds = seq(0, 1, by = 0.05)
  title = "glm Model ROC for Generalization Phase"
  logisticROC(glmTstPred, testData[, which(colnames(testData) == className)], treshholds, title)
  
}

className = "F3ATTAINMENT"
threshold = 0.5
runAndEvalGlm(className, top10Features, trainData, testData, threshold)
runAndEvalGlm(className, top15Features, trainData, testData, threshold)
runAndEvalGlm(className, top20Features, trainData, testData, threshold)
runAndEvalGlm(className, top25Features, trainData, testData, threshold)

# Section 3-c: Implementation of Logistic Regression with Gradient Descent
sigmoid = function(x) {
  sigmoid = 1/(1 + exp(-x))
  return(sigmoid)
}

logisticGD = function(className, featureNames, data, learnRate, numOfIters){
  dataY = data[, which(colnames(data) == className)]
  y = as.matrix(dataY)
  
  dataX = data[, which(colnames(data) %in% featureNames)]
  X = as.matrix(dataX)
  X = cbind(rep(1, nrow(X)), X)
  
  beta = matrix(rep(0, ncol(X)), nrow = ncol(X)) # initial beta
  
  for (i in 1:numOfIters){
    error = sigmoid(X %*% beta) - y
    delta = (t(X) %*% error) / length(y)
    beta = beta - learnRate * delta
  }
  ls = list(beta = beta)
  return(ls)
}


logisticPredict = function(logisticModel, dataX){
  X = as.matrix(dataX)
  X = cbind(rep(1, nrow(X)), X)
  beta = logisticModel$beta
  pPred = sigmoid(X %*% beta)

  return(pPred)
}


runAndEvalGD = function (className, featureNames, trainData, testData,
                           learnRate, numOfIters, threshold){
  if (!require(caret)) {install.packages("caret")}
  library(caret)
  
  gdModel = logisticGD(className, featureNames, trainData, learnRate, numOfIters)
  trDataX = trainData[, which(colnames(trainData) %in% featureNames)]
  gdTrPred = logisticPredict(gdModel, trDataX)
  gdTrCfmTbl = table(ifelse(gdTrPred > threshold, 1, 0), 
                     trainData[, which(colnames(trainData) == className)])
  gdTrCfm = confusionMatrix(gdTrCfmTbl)
  print("Logistic using GD Model Confusion Matrix and Statistics for Training Phase")
  print(gdTrCfm)
  
  tstDataX = testData[, which(colnames(testData) %in% featureNames)]
  gdTstPred = logisticPredict(gdModel, tstDataX)
  gdTstCfmTbl = table(ifelse(gdTstPred > threshold, 1, 0), 
                      testData[, which(colnames(testData) == className)])
  gdTstCfm = confusionMatrix(gdTstCfmTbl)
  
  print("Logistic using GD Model Confusion Matrix and Statistics for Generalization Phase")
  print(gdTstCfm)
  
  print(ifelse(isUnderfitting(gdTrCfm),"This model is deficient",
               paste("There is no underfitting. ", 
                     "This model is an effective learner with accuracy of [", 
                     gdTrCfm$overall['Accuracy'], "]", 
                     sep = "")))
  
  
  print(ifelse(isOverfitting(gdTrCfm, gdTstCfm), "The model is overfitting.",
               paste("There is no overfitting. This model is an effective learner with training accuracy of [",
                     gdTrCfm$overall['Accuracy'], 
                     "] and testing accuracy of [",
                     gdTstCfm$overall['Accuracy'], "]",
                     sep = "")))
  
  treshholds = seq(0, 1, by = 0.05)
  title = "Logistic using GD Model ROC for Generalization Phase"
  logisticROC(gdTstPred, testData[, which(colnames(testData) == className)], treshholds, title)

}


className = "F3ATTAINMENT"
learnRate = 0.00001
numOfIters = 30000
threshold = 0.5

runAndEvalGD(className, top10Features, trainData, testData, learnRate, numOfIters, threshold)
runAndEvalGD(className, top15Features, trainData, testData, learnRate, numOfIters, threshold)
runAndEvalGD(className, top20Features, trainData, testData, learnRate, numOfIters, threshold)
runAndEvalGD(className, top25Features, trainData, testData, learnRate, numOfIters, threshold)

# Section 3-d: Implementation of Logistic Regression with Newton-Raphson
logisticNR = function(className, featureNames, data, dif){
  #Calculate the matrix W with all diagonal values as p*(1-p) 
  computeWeights = function(p){
    W = matrix(0, length(p), length(p)) # make up a square matrix
    
    for (i in 1:length(p)){
      W[i,i] = p[i] * (1-p[i]) # set the diagonal elements
    }
    return(W)
  }
  
  dataY = data[, which(colnames(data) == className)]
  y = as.matrix(dataY)
  
  dataX = data[, which(colnames(data) %in% featureNames)]
  X = as.matrix(dataX)
  X = cbind(rep(1, nrow(X)), X)
  
  beta = matrix(rep(0, ncol(X)), nrow = ncol(X)) # initial beta
  
  delta = 1:(ncol(beta))
  diff = 10000
  
  while(diff > dif) {
    p = sigmoid(X %*% beta) # compute p
    W = computeWeights(p) # setup a diagonal matrix of p*(1-p)
   
    delta = solve(t(X) %*% W %*% X) %*% t(X) %*% (y - p)
    beta = beta + delta
    
    diff = sum(delta^2) # squared diff 
  }

  ls = list(beta = beta)
  return(ls)
}



runAndEvalNR = function (className, featureNames, trainData, testData,
                         dif, threshold){
  if (!require(caret)) {install.packages("caret")}
  library(caret)
  
  nrModel = logisticNR(className, featureNames, trainData, dif)
  trDataX = trainData[, which(colnames(trainData) %in% featureNames)]
  nrTrPred = logisticPredict(nrModel, trDataX)
  nrTrCfmTbl = table(ifelse(nrTrPred > threshold, 1, 0), 
                     trainData[, which(colnames(trainData) == className)])
  nrTrCfm = confusionMatrix(nrTrCfmTbl)
  print("Logistic using NR Model Confusion Matrix and Statistics for Training Phase")
  print(nrTrCfm)
  
  tstDataX = testData[, which(colnames(testData) %in% featureNames)]
  nrTstPred = logisticPredict(nrModel, tstDataX)
  nrTstCfmTbl = table(ifelse(nrTstPred > threshold, 1, 0), 
                      testData[, which(colnames(testData) == className)])
  nrTstCfm = confusionMatrix(nrTstCfmTbl)
  
  print("Logistic using NR Model Confusion Matrix and Statistics for Generalization Phase")
  print(nrTstCfm)
  
  print(ifelse(isUnderfitting(nrTrCfm),"This model is deficient",
               paste("There is no underfitting. ", 
                     "This model is an effective learner with accuracy of [", 
                     nrTrCfm$overall['Accuracy'],"]",
                     sep = "")))
  
  
  print(ifelse(isOverfitting(nrTrCfm, nrTstCfm), "The model is overfitting.",
               paste("There is no overfitting. This model is an effective learner with training accuracy of [",
                     nrTrCfm$overall['Accuracy'], 
                     "] and testing accuracy of [",
                     nrTstCfm$overall['Accuracy'],"]",
                     sep = "")))
  
  treshholds = seq(0, 1, by = 0.05)
  title = "Logistic using NR Model ROC for Generalization Phase"
  logisticROC(nrTstPred, testData[, which(colnames(testData) == className)], treshholds, title)
  
}

className = "F3ATTAINMENT"
dif = 0.000000001
threshold = 0.5

runAndEvalNR(className, top10Features, trainData, testData, dif, threshold)
runAndEvalNR(className, top15Features, trainData, testData, dif, threshold)
runAndEvalNR(className, top20Features, trainData, testData, dif, threshold)
runAndEvalNR(className, top25Features, trainData, testData, dif, threshold)

# Secition 4: NaiveBayes
# NaiveBayes ROC Evaluation
nbROC = function(predraw, actlClass, thresholds, title){
  TPRates = c()
  FPRates = c()
  
  for (threshold in thresholds){
    predClass = ifelse(predraw[, 2] > threshold, 1, 0)
    cfmTable = table(predClass, actlClass)
    
    if (dim(cfmTable)[1] == 2){
      TPRates = c(TPRates, cfmTable[1,1]/sum(cfmTable[,1]))
      FPRates = c(FPRates, cfmTable[1,2]/sum(cfmTable[,2]))
    }
    else if (as.numeric(row.names(cfmTable)) == 0) {
      TPRates = c(TPRates, 1)
      FPRates = c(FPRates, 1)
    }
    else if (as.numeric(row.names(cfmTable)) == 1){
      TPRates = c(TPRates, 0)
      FPRates = c(FPRates, 0)
    }
  }
  
  if(!require(pracma)) {install.packages('pracma')}
  library(pracma)
  auc = trapz(FPRates, TPRates)
  
  plot(FPRates, TPRates, 
       type = "l", 
       col = "red", 
       xlab = "False Positive Rate", 
       ylab = "True Positive Rate", 
       main = title)
  axis(1, seq(0.0, 1.0, 0.1))
  axis(2, seq(0.0, 1.0, 0.1))
  abline(h=seq(0.0, 1.0, 0.1), v=seq(0.0, 1.0, 0.1), col="gray", lty=3)
  legend(0.7, 0.3, sprintf("%3.3f",auc), lty=c(1,1), lwd=c(2.5,2.5), col="red", title = "AUC")
}


runAndEvalNB = function(className, featureNames, nbTrainData, nbTestData){
  if(!require(e1071)) {install.packages("e1071")}
  library(e1071)
  
  if (!require(caret)) {install.packages("caret")}
  library(caret)
  
  formulaStr = paste(className, "~", paste(featureNames, collapse="+"))
  formula = formula(formulaStr)
  
  nbModel = naiveBayes(formula = formula, data = nbTrainData)
  
  nbTrPred = predict(nbModel, nbTrainData[, -which(colnames(nbTrainData) == className)], type = "class")
  nbTrCfm = confusionMatrix(table(nbTrPred, nbTrainData[, which(colnames(nbTrainData) == className)]))
  print("NaiveBayes Model Confusion Matrix and Statistics for Training Phase")
  print(nbTrCfm)
  
  nbTstPred = predict(nbModel, nbTestData[, -which(colnames(nbTestData) == className)], type = "class")
  nbTstPredraw = predict(nbModel, nbTestData[, -which(colnames(nbTestData) == className)], type = "raw")
  nbTstCfm = confusionMatrix(table(nbTstPred, nbTestData[, which(colnames(nbTestData) == className)]))
  print("NaiveBayes Model Confusion Matrix and Statistics for Generalization Phase")
  print(nbTstCfm)
  
  print(ifelse(isUnderfitting(nbTrCfm),"This model is deficient",
               paste("There is no underfitting. ", 
                     "This model is an effective learner with accuracy of [", 
                     nbTrCfm$overall['Accuracy'], "]",
                     sep = "")))
  
  
  print(ifelse(isOverfitting(nbTrCfm, nbTstCfm), "The model is overfitting.",
               paste("There is no overfitting. This model is an effective learner with training accuracy of [",
                     nbTrCfm$overall['Accuracy'], 
                     "] and testing accuracy of [",
                     nbTstCfm$overall['Accuracy'], "]",
                     sep = "")))
  
  treshholds = seq(0, 1, by = 0.05)
  title = "NaiveBayes Model ROC for Generalization Phase"
  nbROC(nbTstPredraw, nbTestData[, which(colnames(nbTestData) == className)], treshholds, title)

}

# Prepare features for NB model
nbData = data # copy data
top20Idx = which((colnames(nbData) %in% top20Features)) # Get indices of the top 20 features
top20Types = sapply(nbData[, top20Idx], typeof) # Get the types of each feature
table(top20Types)
print(top20Types) 

if (!require("arules")) install.packages("arules")
library("arules")
nbData$BYTXRIRR = discretize(nbData$BYTXRIRR, method = "cluster", breaks = 4, labels = FALSE)
table(nbData$BYTXRIRR)
top20Types = sapply(nbData[, top20Idx], typeof) # Get the types of each feature
table(top20Types)
print(top20Types)

nbTrainDataIdx = trainDataIdx # Use the random indices we generated in previous section
nbTrainData = nbData[trainDataIdx, ]
nbTestData = nbData[-trainDataIdx, ]

className = "F3ATTAINMENT"
runAndEvalNB(className, top10Features, nbTrainData, nbTestData)
runAndEvalNB(className, top15Features, nbTrainData, nbTestData)
runAndEvalNB(className, top20Features, nbTrainData, nbTestData)
runAndEvalNB(className, top25Features, nbTrainData, nbTestData)

# Implementation of Naive Bayes
NBBinaryClassProb = function(nbTrainData, className, nbfeatureNames, nbFeatureVals){
  classIdx = which(colnames(nbTrainData) == className)
  featureInds = which(colnames(nbTrainData) %in% nbfeatureNames)
  
  totalCount = nrow(nbTrainData)
  dataClass0 = nbTrainData[which(nbTrainData[, classIdx] == 0), ]
  dataClass1 = nbTrainData[which(nbTrainData[, classIdx] == 1), ]
  class0Count = nrow(dataClass0)
  class1Count = nrow(dataClass1)
  
  class0Prob = class0Count/totalCount
  class1Prob = class1Count/totalCount

  classProbCalculator = function(featureIdx, featureVal){
      c0Prob = nrow(dataClass0[which(dataClass0[, featureIdx] == featureVal), ])/class0Count
      c1Prob = nrow(dataClass1[which(dataClass1[, featureIdx] == featureVal), ])/class1Count
      classProb = c(c0Prob, c1Prob)
      return(classProb)
  }
  
  probList = c()
  for (i in 1:length(featureInds)){
    probFeaturei =  classProbCalculator(featureInds[i], nbFeatureVals[i])
    #print(paste(featureInds[i], colnames(nbTrainData)[featureInds[i]], nbFeatureVals[i],probFeaturei))
    probList = c(probList, probFeaturei)
  }
  probList = c(probList, c(class0Prob, class1Prob))
  
  nrow = length(featureInds) + 1
  classProbMatrix = matrix(probList, nrow = nrow, ncol = 2, byrow = TRUE)
  
  
  predClassProbRaw = c()
  for (i in 1:2){
    currClassProbRaw = 1
    for (j in 1:nrow){
      currClassProbRaw = currClassProbRaw * classProbMatrix[j, i]
    }
    predClassProbRaw = c(predClassProbRaw, currClassProbRaw)
  }
  
  probSum = predClassProbRaw[1] + predClassProbRaw[2] 
  predClassProb = c(predClassProbRaw[1]/probSum, predClassProbRaw[2]/probSum)
  
  return(predClassProb)
}

NBBinaryPredictRaw = function(nbTrainData, className, nbFeatureNames, dataToPred){
  predFeatureInds = which(colnames(dataToPred) %in% nbFeatureNames)
  predictions = c()
  nrow = nrow(dataToPred)
  for (i in 1:nrow){
    nbFeatureVals = c()
    for (j in 1:length(predFeatureInds)){
      currFeatureVal = dataToPred[i, predFeatureInds[j]]
      nbFeatureVals = c(nbFeatureVals, currFeatureVal)
    }
    currPredraw = NBBinaryClassProb(nbTrainData, className, nbFeatureNames, nbFeatureVals)
    predictions = c(predictions, currPredraw)
  }
  predictionsMatrix = matrix(predictions, nrow = nrow, ncol = 2, byrow = TRUE)
  return(predictionsMatrix)
}

runAndEvalNBBinary = function(className, nbFeatureNames, nbTrainData, nbTestData, threshold){
  if (!require(caret)) {install.packages("caret")}
  library(caret)
  
  nbTrPredraw = NBBinaryPredictRaw(nbTrainData, className, nbFeatureNames, nbTrainData)
  nbTrPredClass = ifelse(nbTrPredraw[, 2] > threshold, 1, 0)
  nbTrCfm = confusionMatrix(table(nbTrPredClass, nbTrainData[, which(colnames(nbTrainData) == className)]))
  print("Naive Bayes Model Confusion Matrix and Statistics for Training Phase")
  print(nbTrCfm)
  
  nbTstPredraw = NBBinaryPredictRaw(nbTrainData, className, nbFeatureNames, nbTestData)
  nbTstPredClass = ifelse(nbTstPredraw[, 2] > threshold, 1, 0)
  nbTstCfm = confusionMatrix(table(nbTstPredClass, nbTestData[, which(colnames(nbTestData) == className)]))
  print("Naive Bayes Model Confusion Matrix and Statistics for Generalization Phase")
  print(nbTstCfm)
  
  print(ifelse(isUnderfitting(nbTrCfm),"This model is deficient",
               paste("There is no underfitting. ", 
                     "This model is an effective learner with accuracy of [", 
                     nbTrCfm$overall['Accuracy'], "]",
                     sep = "")))
  
  
  print(ifelse(isOverfitting(nbTrCfm, nbTstCfm), "The model is overfitting.",
               paste("There is no overfitting. This model is an effective learner with training accuracy of [",
                     nbTrCfm$overall['Accuracy'], 
                     "] and testing accuracy of [",
                     nbTstCfm$overall['Accuracy'], "]",
                     sep = "")))
  
  treshholds = seq(0, 1, by = 0.05)
  title = "Naive Bayes Model ROC for Generalization Phase"
  nbROC(nbTstPredraw, nbTestData[, which(colnames(nbTestData) == className)], treshholds, title)

}


className = "F3ATTAINMENT"
threshold = 0.5
runAndEvalNBBinary(className, top10Features, nbTrainData, nbTestData, threshold)
runAndEvalNBBinary(className, top15Features, nbTrainData, nbTestData, threshold)
runAndEvalNBBinary(className, top20Features, nbTrainData, nbTestData, threshold)

#Section 5: kNN classifier
# kNN ROC Evaluation
knnROC = function(predResponse, actlClass, thresholds, title){
  TPRates = c()
  FPRates = c()
  
  for (threshold in thresholds){
    predClass = ifelse(predResponse > threshold, 1, 0)
    cfmTable = table(predClass, actlClass)
    
    if (dim(cfmTable)[1] == 2){
      TPRates = c(TPRates, cfmTable[1,1]/sum(cfmTable[,1]))
      FPRates = c(FPRates, cfmTable[1,2]/sum(cfmTable[,2]))
    }
    else if (as.numeric(row.names(cfmTable)) == 0) {
      TPRates = c(TPRates, 1)
      FPRates = c(FPRates, 1)
    }
    else if (as.numeric(row.names(cfmTable)) == 1){
      TPRates = c(TPRates, 0)
      FPRates = c(FPRates, 0)
    }
  }
  
  if(!require(pracma)) {install.packages('pracma')}
  library(pracma)
  auc = trapz(FPRates, TPRates)
  
  plot(FPRates, TPRates, 
       type = "l", 
       col = "red", 
       xlab = "False Positive Rate", 
       ylab = "True Positive Rate", 
       main = title)
  axis(1, seq(0.0, 1.0, 0.1))
  axis(2, seq(0.0, 1.0, 0.1))
  abline(h=seq(0.0, 1.0, 0.1), v=seq(0.0, 1.0, 0.1), col="gray", lty=3)
  legend(0.7, 0.3, sprintf("%3.3f",auc), lty=c(1,1), lwd=c(2.5,2.5), col="red", title = "AUC")
}


runAndEvalkNN = function(className, FeatureNames, TrainData, TestData, kSeq){
  library(class)

  classIdx = which(colnames(TrainData) == className)
  featureIdx = which(colnames(TrainData) %in% FeatureNames)
  trainX = trainData[, featureIdx] # Features in the training data set
  testX = testData[, featureIdx] # Features in the testing data set
  
  maxAcc = 0 # Initial value of maximum testing accuracy
  maxAccK = 1 # Initial value of K when testing accuracy maximize
  knnTstPredClass = c()
  knnTstPredProb =c()
  for (i in kSeq){
    knnPred = knn(trainX, testX, trainData[, classIdx], k = i, prob = TRUE)
    knnPredProb = ifelse(knnPred == 1, attr(knnPred, "prob"), 1 - attr(knnPred, "prob"))
    table(knnPred, testData[, classIdx])
    testAcc = mean(knnPred == testData[, classIdx]) # calculate testing accuracy
    if (testAcc > maxAcc){
      maxAcc = testAcc
      maxAccK = i
      knnTstPredClass = knnPred
      knnTstPredProb = knnPredProb
    }
    print(paste("The testing accuracy when k = ", i, " is ", testAcc, sep=""))
  }
  print(paste("When k = ", maxAccK, ", ", 
              "we get the maximum test accuracy, which is ",
              maxAcc, ".", sep=""))
  
  kNNTstCfm = confusionMatrix(table(knnTstPredClass, testData[, which(colnames(testData) == className)]))
  print(paste("kNN Model (k = ", maxAccK, 
              ") Confusion Matrix and Statistics for Generalization Phase"),
             sep ="")
  print(kNNTstCfm)
  
  treshholds = seq(0, 1, by = 0.05)
  title = paste("kNN Model (k = ", maxAccK, ") ROC for Generalization Phase",
                sep ="")
  knnROC(knnTstPredProb, testData[, classIdx], treshholds, title)
}

className = "F3ATTAINMENT"
kSeq = seq(1, 49, 2) # A sequence of values of K
runAndEvalkNN(className, top10Features, trainData, testData, kSeq)
runAndEvalkNN(className, top15Features, trainData, testData, kSeq)
runAndEvalkNN(className, top20Features, trainData, testData, kSeq)


# Part II - Homework 2
# Fit a single tree
treeData = data # Copy data
className = "F3ATTAINMENT"
classIdx = which(colnames(treeData) == "F3ATTAINMENT")
treeData$F3ATTAINMENT = as.factor(treeData[, classIdx])
treeTrDataIdx = trainDataIdx
treeTrData = treeData[treeTrDataIdx, ]
treeTstData = treeData[-treeTrDataIdx, ]

library("tree")
sglTree = tree(F3ATTAINMENT~ ., treeTrData) # Fit a single decision tree
summary(sglTree) # Produce a summary of the tree
plot(sglTree) # Plot the tree
text(sglTree, pretty=0) # Add text
print(top10Features) # Compare with top10Features from previous section
print(top15Features) # Compare with top15Features from previous section

sglTreePred = predict(sglTree, treeTstData, type = "class") # Pridict on testing Data
sglTreeTestCfm = confusionMatrix(table(sglTreePred, treeTstData[, classIdx])) # Create confusion matrix
print(sglTreeTestCfm)

# Improve tree using bagging

if (!require("randomForest")) istall.package("randomForest")
library("randomForest")








# Random Forest

glmBagging = function(className, featureNames, data, trainDataIdx, threshold){
  if (!require(caret)) {install.packages("caret")}
  library(caret)
  
  formulaStr = paste(className,"~", paste(featureNames, collapse="+"))
  glmModel = glm(formulaStr, data = trainData, family = "binomial")
  summary(glmModel)
  
}



