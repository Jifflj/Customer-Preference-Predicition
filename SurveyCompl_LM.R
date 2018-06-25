# Loading libraries
library(readr)
library(caret)


# Program starts 
SurveyComplete <- read.csv("/home/johannes/Documents/Ubiqum/Course2/task2/Data/Survey_Key_and_Complete_Responses.csv")
#str(SurveyComplete)

# changing datatypes

SurveyComplete$car <- factor(SurveyComplete$car)

SurveyComplete$zipcode <- factor(SurveyComplete$zipcode)

SurveyComplete$brand <- factor(SurveyComplete$brand, 
                               levels = c(0,1), 
                               labels = c("Acer", "Sony"))

summary(SurveyComplete)

str(SurveyComplete)

# sets random seed
set.seed(425)
# creates partition of the data 75% / 25%
inTrain <- createDataPartition(y=SurveyComplete$brand,
                               p=.75,
                               list=FALSE)
str(inTrain)

trainingset <- SurveyComplete[ inTrain,]
testingset  <- SurveyComplete[-inTrain,]

nrow(trainingset)
nrow(testingset)

ctrl <- trainControl(method = "repeatedcv", # specifies repeated K-fold cross-validation 
                     repeats = 3, # number of repititions
                     classProbs = TRUE, # incudes calculations of measures specific to two-class problems (area under ROC curve, sensitivity/specificity)
                     summaryFunction = twoClassSummary) # takes the observed and prdicted values to estimate measure of performance

# Accuracy and kappa
knnFit1 <- train(brand ~ ., 
                 data = trainingset, 
                 method = "knn",
                 tuneLength = 20, # controls how many candidate sets of parameter values are evaluated
                 trControl = ctrl, 
                 metric = "ROC", # specifies the criterion that should be optimized - takes tuning parameters with best result
                 preProc = c("center","scale"))

knnFit1

predictors(knnFit1)

testPredknn1 <- predict(knnFit1, testingset) #type = "prob")

postResample(testPredknn1, testingset$brand)

#confusionMatrix(data = testPredknn1, testingset$brand)

plot(testPredknn1,testingset$brand)
