# Loading libraries
library(readr)
library(caret)
library(doMC)
library(ggplot2)
library(arules)

registerDoMC(cores=4)

# Program starts 
SurveyComplete2 <- read.csv("/home/johannes/Documents/Ubiqum/Course2/task2/Data/Survey_Key_and_Complete_Responses.csv")

#str(SurveyComplete2)

# changing datatypes

SurveyComplete2$car <- factor(SurveyComplete2$car)

SurveyComplete2$zipcode <- factor(SurveyComplete2$zipcode)

SurveyComplete2$brand <- factor(SurveyComplete2$brand, 
                               levels = c(0,1), 
                               labels = c("Acer", "Sony"))

#SurveyComplete2$age <- discretize(SurveyComplete2$age,
                                 #method = "interval",
                                 #categories = 4,
                                 #labels = NULL,
                                 #ordered = F,
                                 #onlycuts = F)

#SurveyComplete2$salary <- discretize(SurveyComplete2$salary,
                              #method = "interval",
                              #categories = 4,
                              #labels = NULL,
                              #ordered = F,
                              #onlycuts = F)

#summary(SurveyComplete2)

#str(SurveyComplete2)

#table(SurveyComplete2$brand)

# sets random seed
set.seed(425)
# creates partition of the data 75% / 25%
inTrain <- createDataPartition(y=SurveyComplete2$brand,
                               p=.75,
                               list=FALSE)
#str(inTrain)

trainingset <- SurveyComplete2[ inTrain,]
testingset  <- SurveyComplete2[-inTrain,]

nrow(trainingset)
nrow(testingset)

ctrl <- trainControl(method = "repeatedcv", # specifies repeated K-fold cross-validation
                     number = 5,
                     repeats = 2, # number of repititions
                     classProbs = TRUE, # incudes calculations of measures specific to two-class problems (area under ROC curve, sensitivity/specificity)
                     summaryFunction = twoClassSummary) # takes the observed and prdicted values to estimate measure of performance

# Accuracy and kappa
rfFit1 <- train(brand ~ 
                +age
                +salary
                -elevel
                -car
                -zipcode
                -credit,
                data = trainingset, 
                method = "rf",
                tuneLength = 5, # controls how many candidate sets of parameter values are evaluated
                trControl = ctrl,
                metric = "ROC", # specifies the criterion that should be optimized - takes tuning parameters with best result
                preProc = c("center","scale"))

rfFit1

plot(rfFit1)

predictors(rfFit1)

testPredrf1 <- predict(rfFit1, testingset) #type = "prob")

postResample(testPredrf1, testingset$brand)

#confusionMatrix(data = testPredknn1, testingset$brand)

plot(testPredrf1,testingset$brand)

SurveyComplete2$age <- discretize(SurveyComplete2$age,
                                  method = "interval",
                                  categories = 4,
                                  labels = NULL,
                                  ordered = F,
                                  onlycuts = F)

SurveyComplete2$salary <- discretize(SurveyComplete2$salary,
                                     method = "interval",
                                     categories = 4,
                                     labels = NULL,
                                     ordered = F,
                                     onlycuts = F)

write.csv(SurveyComplete2, file = "SurveyComplete_discr.csv")
#predicting survey_incomplete
SurveyIncomplete2 <- read.csv("/home/johannes/Documents/Ubiqum/Course2/task2/Data/SurveyIncomplete.csv")
#str(SurveyIncomplete2)

SurveyIncomplete2$car <- factor(SurveyIncomplete2$car)

SurveyIncomplete2$zipcode <- factor(SurveyIncomplete2$zipcode)

SurveyIncomplete2$brand <- factor(SurveyIncomplete2$brand,
                                     levels = c(0,1), 
                                     labels = c("Acer", "Sony"))

#SurveyIncomplete2$age <- discretize(SurveyIncomplete2$age,
                                    #method = "interval",
                                    #categories = 4,
                                    #labels = NULL,
                                    #ordered = F,
                                    #onlycuts = F)

#SurveyIncomplete2$salary <- discretize(SurveyIncomplete2$salary,
                              #method = "interval",
                              #categories = 4,
                              #labels = NULL,
                              #ordered = F,
                              #onlycuts = F)

#str(SurveyIncomplete2)

predset <- SurveyIncomplete2
nrow(predset)

set.seed(425)

predrf1 <- predict(rfFit1, newdata = predset)

predrf1

predset$brand <- predrf1

predset

predset$age <- discretize(predset$age,
                                    method = "interval",
                                    categories = 4,
                                    labels = NULL,
                                    ordered = F,
                                    onlycuts = F)
                                    
predset$salary <- discretize(predset$salary,
                                    method = "interval",
                                    categories = 4,
                                    labels = NULL,
                                    ordered = F,
                                    onlycuts = F)
                                    


#write.csv(predset, file = "predrf2.csv")
