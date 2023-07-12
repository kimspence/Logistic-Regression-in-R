
install.packages("caret")
install.packages("ROCR")

library(tidyverse)
library(caret)
library(ROCR)

setwd("C:/Users/kspen/OneDrive/Documents")

insdf <- read.csv("insurance.csv")
view(insdf)

sum(is.na(insdf))

insdf$CLAIM <- factor(insdf$CLAIM, levels=c(0,1), labels=c("No", "Yes"))
insdf$KIDSDRIV <- factor(insdf$KIDSDRIV, levels=c(0,1), labels=c("No", "Yes"))
insdf$HOMEKIDS <- factor(insdf$HOMEKIDS, levels=c(0,1), labels=c("No", "Yes"))
insdf$HOMEOWN <- factor(insdf$HOMEOWN, levels=c(0,1), labels=c("No", "Yes"))
insdf$MSTATUS <- factor(insdf$MSTATUS, levels=c(0,1), labels=c("No", "Yes"))
insdf$GENDER <- factor(insdf$GENDER, levels=c(0,1), labels=c("No", "Yes"))
insdf$EDUCATION <- factor(insdf$EDUCATION, levels=c(0,1), labels=c("No", "Yes"))
insdf$CAR_USE <- factor(insdf$CAR_USE, levels=c(0,1), labels=c("No", "Yes"))
insdf$RED_CAR <- factor(insdf$RED_CAR, levels=c(0,1), labels=c("No", "Yes"))
insdf$CLM_BEF <- factor(insdf$CLM_BEF, levels=c(0,1), labels=c("No", "Yes"))
insdf$REVOKED <- factor(insdf$REVOKED, levels=c(0,1), labels=c("No", "Yes"))
insdf$MVR_PTS <- factor(insdf$MVR_PTS, levels=c(0,1), labels=c("No", "Yes"))
insdf$URBANICITY <- factor(insdf$URBANICITY, levels=c(0,1), labels=c("No", "Yes"))

#Generate summary statistics for the variables in the insurance.csv dataset.
summary(insdf)

#Partition the dataset into training, validation, and test set,60%, 20%, 20% split.
set.seed(42)
Samples <- sample(seq(1,3), size=nrow(insdf), replace=TRUE, prob=c(0.6, 0.2, 0.2))
Train <- insdf[Samples==1,]
Validate <- insdf[Samples==2,]
Test <- insdf[Samples==3,]
summary(Train)
summary(Validate)
summary(Test)

#Conduct a logistic regression analysis using the training dataframe with CLAIM as the 
#outcome variable and all the other variables in the dataset as predictor variables.
options(scipen=999)
lrtrain <- glm(CLAIM ~ ., data=Train,
               family=binomial(link="logit"))
summary(lrtrain)
exp(coef(lrtrain))

#create a confusion matrix to assess the accuracy of the logistic regression model 
#and obtain probability of a claim for each observation in the validation set
lrprobs <- predict(lrtrain, newdata=Validate, type="response")
Validate <- cbind(Validate, Probabilities=lrprobs)
lrclass <- as.factor(ifelse(lrprobs>0.5,"Yes", "No"))
confusionMatrix(lrclass, Validate$CLAIM, positive="Yes")

#create an ROC curve plot and calculate the AUC.
predROC <- prediction(lrprobs, Validate$CLAIM)
perfROC <- performance(predROC, "tpr", "fpr")
plot(perfROC)
abline(a=0, b=1)
performance(predROC, measure="auc")@y.values[[1]]

#create a new training subset using the oversampling method.
xsdf <- Train[c(-1)]
view(xsdf)
oversample <- upSample(x=xsdf, y=Train$CLAIM, yname="CLAIM")
table(oversample$CLAIM)

#Conduct a logistic regression analysis using the new oversampled training subset with
#CLAIM as the outcome variable and all the other variables in the dataset as predictor 
#variables.
set.seed(42)
lrOver <- glm(CLAIM ~ ., data=oversample,
              family=binomial(link="logit"))
summary(lrOver)

#create a confusion matrix to assess the accuracy of the logistic regression model
lrprobsO <- predict(lrOver, newdata=Validate, type="response")
Validate <- cbind(Validate, Probabilities=lrprobsO)
lrclassO <- as.factor(ifelse(lrprobsO>0.5,"Yes", "No"))
Validate <- cbind(Validate, PredClass=lrclassO)
confusionMatrix(lrclassO, Validate$CLAIM, positive="Yes")

#create an ROC curve plot and calculate the AUC.
predROCO <- prediction(lrprobsO, Validate$CLAIM)
perfROCO <- performance(predROCO, "tpr", "fpr")
plot(perfROCO)
abline(a=0, b=1)
performance(predROCO, measure="auc")@y.values[[1]]

#create a confusion matrix to assess the accuracy of the logistic regression model 
#on the test dataframe.
lrprobstest <- predict(lrOver, newdata=Test, type="response")
lrclasstest <- as.factor(ifelse(lrprobstest>0.5,"Yes", "No"))
confusionMatrix(lrclasstest, Test$CLAIM, positive="Yes")

#create an ROC curve plot and calculate the AUC.
predROCtest <- prediction(lrprobstest, Test$CLAIM)
perfROCtest <- performance(predROCtest, "tpr", "fpr")
plot(perfROCtest)
abline(a=0, b=1)
performance(predROCtest, measure="auc")@y.values[[1]]

#predict probability scores for insurance claims for ten new customers
new_customers <- read.csv("insurance_predictions.csv")
view(new_customers)

new_customers$CLAIM <- factor(new_customers$CLAIM, levels=c(0,1), labels=c("No", "Yes"))
new_customers$KIDSDRIV <- factor(new_customers$KIDSDRIV, levels=c(0,1), labels=c("No", "Yes"))
new_customers$HOMEKIDS <- factor(new_customers$HOMEKIDS, levels=c(0,1), labels=c("No", "Yes"))
new_customers$HOMEOWN <- factor(new_customers$HOMEOWN, levels=c(0,1), labels=c("No", "Yes"))
new_customers$MSTATUS <- factor(new_customers$MSTATUS, levels=c(0,1), labels=c("No", "Yes"))
new_customers$GENDER <- factor(new_customers$GENDER, levels=c(0,1), labels=c("No", "Yes"))
new_customers$EDUCATION <- factor(new_customers$EDUCATION, levels=c(0,1), labels=c("No", "Yes"))
new_customers$CAR_USE <- factor(new_customers$CAR_USE, levels=c(0,1), labels=c("No", "Yes"))
new_customers$RED_CAR <- factor(new_customers$RED_CAR, levels=c(0,1), labels=c("No", "Yes"))
new_customers$CLM_BEF <- factor(new_customers$CLM_BEF, levels=c(0,1), labels=c("No", "Yes"))
new_customers$REVOKED <- factor(new_customers$REVOKED, levels=c(0,1), labels=c("No", "Yes"))
new_customers$MVR_PTS <- factor(new_customers$MVR_PTS, levels=c(0,1), labels=c("No", "Yes"))
new_customers$URBANICITY <- factor(new_customers$URBANICITY, levels=c(0,1), labels=c("No", "Yes"))

lrprobsnew <- predict(lrOver, newdata=new_customers, type="response")
new_customers <- cbind(new_customers, Probabilities=lrprobsnew)
view(new_customers)