# Loading libraries
library(readr)
library(caret)
library(doMC)
library(ggplot2)
library(arules)

registerDoMC(cores=4)
# Program starts 
SurveyComplete1 <- read.csv("/home/johannes/Documents/Ubiqum/Course2/task2/Data/Survey_Key_and_Complete_Responses.csv")
#str(SurveyComplete1)

# changing datatypes

SurveyComplete1$car <- factor(SurveyComplete1$car)

SurveyComplete1$zipcode <- factor(SurveyComplete1$zipcode)

SurveyComplete1$brand <- factor(SurveyComplete1$brand, 
                               levels = c(0,1), 
                               labels = c("Acer", "Sony"))

#SurveyComplete1$age <- discretize(SurveyComplete1$age,
                                 #method = "interval",
                                 #categories = 4,
                                 #labels = NULL,
                                 #ordered = F,
                                 #onlycuts = F)

#summary(SurveyComplete1)

#str(SurveyComplete1)

# sets random seed
set.seed(425)
# creates partition of the data 75% / 25%
inTrain <- createDataPartition(y=SurveyComplete1$brand,
                               p=.75,
                               list=FALSE)
#str(inTrain)

trainingset <- SurveyComplete1[ inTrain,]
testingset  <- SurveyComplete1[-inTrain,]

#nrow(trainingset)
#nrow(testingset)

ctrl <- trainControl(method = "repeatedcv", # specifies repeated K-fold cross-validation 
                     number = 10,
                     repeats = 3, # number of repititions
                     classProbs = TRUE, # incudes calculations of measures specific to two-class problems (area under ROC curve, sensitivity/specificity)
                     summaryFunction = twoClassSummary) # takes the observed and prdicted values to estimate measure of performance

# Accuracy and kappa
knnFit1 <- train(brand ~ 
                 +age
                 +salary
                 -elevel
                 -car
                 -zipcode
                 -credit, 
                 data = trainingset, 
                 method = "knn",
                 tuneLength = 40, # controls how many candidate sets of parameter values are evaluated
                 trControl = ctrl, 
                 metric = "ROC", # specifies the criterion that should be optimized - takes tuning parameters with best result
                 preProc = c("center","scale"))

knnFit1

plot(knnFit1)

predictors(knnFit1)

testPredknn1 <- predict(knnFit1, testingset) #type = "prob")

postResample(testPredknn1, testingset$brand)

#confusionMatrix(data = testPredknn1, testingset$brand)

plot(testPredknn1,testingset$brand)
