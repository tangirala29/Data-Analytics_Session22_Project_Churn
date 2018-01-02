getwd()

datafile<- read.csv(file.choose())
head(datafile)

datafile<-datafile[-c(19,20,21)]
head(datafile)
str(datafile)
# Data Split: Train and Test Data
library(caTools)
split1<-sample.split(datafile,SplitRatio = 0.8)
train_DF<- datafile[split1,]
str(train_DF)
test_DF<- datafile[!split1,]
head(test_DF)
str(test_DF)




###  Created training dataset and testing data set  ###

aggregate(datafile$Account.Length~datafile$Churn,FUN=mean)
aggregate(datafile$CustServ.Calls~datafile$Churn,FUN=mean)


### Hypothesis Testing ##


### Using T-test, 
### Null Hypo: There is no significant differenece between average number of customer churns and Account_length
### Altenate Hypo: There is a significant difference between average number of customer churns and Account_length

t.test(datafile$Account.Length~datafile$Churn)
t.test(datafile$CustServ.Calls~datafile$Churn)

### Therefore as the p-value is less than 0.05, we will reject null hypothesis and establish the fact that there is significant difference between average customer churns and customer service calls

### Chi-square test ###

### Null Hypo: There is no significant relationship between two categorical variables
### Alternative Hypo: There is a significant relationship between two categorical variables

chisq.test(table(datafile$Churn,datafile$Int.l.Plan))
chisq.test(table(datafile$Churn,datafile$VMail.Plan))

### As both the p-values are less than 0.05, There is a significant relationship between Churn Vs International plan and Vmail Plan

### Now we will go with logistic regression, Dependent Variable: Churn Vs All other variables 

head(datafile)
str(datafile)

datafile_glm<- glm(Churn~.,train_DF,family = "binomial")


## Interpretation of Logistic Regression 

## Independent variables with p-value <0.05 will be affecting the regression model relatively more than the ones with p-value>0.05

## Null Deviance is greater than Residual Deviance, which is better sign

## AIC value is also in control

## ANOVA Test 

anova(datafile_glm,test="Chisq")

TrainDF_glmpredict<- predict(datafile_glm,train_DF,type = "response")
summary(TrainDF_glmpredict)
TestDF_glmpredict<- predict(datafile_glm,test_DF,type = "response")

head(TrainDF_glmpredict)
head(TestDF_glmpredict)
nrow(TrainDF_glmpredict)

plot(test_DF$Churn~TestDF_glmpredict)

## checking with the threshold value of 0.50

table(Actual=train_DF$Churn,Predicted=TrainDF_glmpredict>0.5)
table_testDF<- table(Actual=test_DF$Churn,Predicted=TestDF_glmpredict>0.5)
table_testDF

outcome1=floor(TestDF_glmpredict+0.50)
table(outcome1)
## Finding the accuracy for both train_predict and Test_predict

accuracy_TrainDF= (2164+77)/(2164+59+77+291)
accuracy_TrainDF
accuracy_testDF=(607+27)/(607+20+27+88)
607+20+27+88
accuracy_testDF
## our accuracy = 86.4917% (train_DF)
## our accuracy = 85.444% (test_DF)

confusionMatrix(test_DF$Churn,TestDF_glmpredict,threshold = 0.5)
## Decision Tree Modeling
library(rpart)
library(rpart.plot)

datafile_Decisiontree<- rpart(Churn~.,data = train_DF,method = "class")
summary(datafile_Decisiontree)
plot(datafile_Decisiontree,cex=0.5)
text(datafile_Decisiontree,cex=0.5)
head(datafile_Decisiontree)



rpart.plot(datafile_Decisiontree,cex=0.6)

## predict Decision Tree Model

DecisionTree_predict_TrainDF<-predict(datafile_Decisiontree,train_DF,type =  "class")
summary(DecisionTree_predict_TrainDF)
head(DecisionTree_predict_TrainDF)
length(train_DF$Churn)
length(datafile_Decisiontree)
table(Actual=train_DF$Churn,Predicted=DecisionTree_predict_TrainDF>0.50)

## Confusion Matrix for Decision Tree Model

confusionMatrix(DecisionTree_predict_TrainDF,train_DF$Churn)

DecisionTree_Predict_TestDF<- predict(datafile_Decisiontree,test_DF,type = "class")

confusionMatrix(DecisionTree_Predict_TestDF,test_DF$Churn)


## Hence, Accuracy of Decision Tree Model on Train DF = 95.72%, Sensitivity= 0.9879, specificity= 0.7717

## Hence, Accuracy of Decision Tree Model on Test DF = 93.13%, Sensitivity= 0.9777, specificity= 0.6783


##Building ROC-Curve and finding AUC (Area Under Curve)

install.packages("InformationValue")
library(InformationValue)
install.packages("pROC")
library(pROC)

rpartpredict_TestDF<- predict(datafile_Decisiontree,test_DF,type = "vector")
head(rpartpredict_TestDF)

# Finding AUC
AUC_TestDF<-auc(test_DF$Churn,rpartpredict_TestDF)
AUC_TestDF
#AUC is 82.8%

plot(roc(test_DF$Churn,rpartpredict_TestDF))

## plotting is also done for ROC

## Random Forest- ALGORITHM for classification Model

library(randomForest)
head(train_DF)

train_DF$Int.l.Plan<- as.factor(train_DF$Int.l.Plan)
train_DF$VMail.Plan<- as.factor(train_DF$VMail.Plan)

str(train_DF)
Datafile_RF_TrainDF<-randomForest(Churn~.,data = train_DF,ntrees=500,do.trace=100)
print(Datafile_RF_TrainDF)

Datafile_RF_TrainDF$predicted
Datafile_RF_TrainDF$importance

# Predicting Random Forest Model on TRain DF
RFpredict_TrainDF<- predict(Datafile_RF_TrainDF,train_DF,type = "class")
table(Actual=train_DF$Churn,Predicted=RFpredict_TrainDF>0.5)

accuracy_RF_TrainDF<- (2223+360)/(2223+0+8+360)
accuracy_RF_TrainDF

# Accuracy of Random FOrest Model on Train DF = 99.69124%

RFpredict_TestDF<- predict(Datafile_RF_TrainDF,test_DF,type = "class")
table(Actual=test_DF$Churn,Predicted= RFpredict_TestDF>0.5)
RFpredict_TestDF$predicted

accuracy_RF_TestDF<- (620+78)/(620+7+78+37)
accuracy_RF_TestDF

# Accuracy of Random Forest Model on Test DF = 94.07008%

confusionMatrix(RFpredict_TestDF>0.50,test_DF$Churn)
getTree(Datafile_RF_TrainDF,k=40,labelVar = TRUE)


## Neural Networks- ALGORITHM for Classification Model
library(nnet)
head(train_DF)

datafile_nnet= nnet(Churn~.,data=train_DF,size=5,maxit=1000)
print(datafile_nnet)
(telecom_nnet)
summary(datafile_nnet)
TrainDF_nnetpredict=predict(datafile_nnet, type = "raw")
head(TrainDF_nnetpredict)
library(caret)

confusionMatrix(TrainDF_nnetpredict,train_DF$Churn)

telecomnnet= nnet(Churn~.,data=teledata1,size=3,maxit=1000)
print(telecomnnet)
summary(telecomnnet)
nnetpredict=predict(telecomnnet,type="class")
head(nnetpredict)
confusionMatrix(nnetpredict,teledata1$Churn)

## Thus by confusion matrix, the accuracy of the nnet model is given as 85.26

head(train_DF)
library(neuralnet)
Datafile_neuralnet=neuralnet(Churn~Account.Length+VMail.Message+Day.Mins+Eve.Mins+Night.Mins+Intl.Mins+ CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge,data=train_DF,hidden=3)
plot(Datafile_neuralnet)

Datafile_TestDF_neuralnet=neuralnet(Churn~Account.Length+VMail.Message+Day.Mins+Eve.Mins+Night.Mins+Intl.Mins+ CustServ.Calls+Int.l.Plan+VMail.Plan+Day.Calls+Day.Charge+Eve.Calls+Eve.Charge+Night.Calls+Night.Charge+Intl.Calls+Intl.Charge,data=test_DF,hidden=3)
plot(Datafile_TestDF_neuralnet)

length(Datafile_neuralnet)
confusionMatrix(Datafile_neuralnet,train_DF$Churn)

## Starting the server 127.0.0.1 for integrating to Tableau
library(Rserve)
Rserve()
