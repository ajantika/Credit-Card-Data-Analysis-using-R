# Credit-Card-Data-Analysis-using-R

#Loading the dataset

#The dataset has been taken from AER package and can be obtained using: library(AER) data("CreditCard") head(CreditCard) str(CreditCard) summary(CreditCard)

hist(CreditCard$age)

library(ggplot2)

#Since age has few outliers, we will clean the data.

Cleaning the dataset
creditcard_sub<-subset(CreditCard, age>18)

summary(creditcard_sub)

hist(creditcard_sub$age)

Split the data into testing and training
library(caTools) #Library to split the data

split=sample.split(creditcard_sub,SplitRatio=0.7)

split

training<- subset(creditcard_sub, split="TRUE")

training

testing<- subset(creditcard_sub, split="FALSE")

Applying logistic Regression
#FULL MODEL ANOVA TEST

model<-glm(card ~.,family=binomial(link="logit"),data=training)

summary(model) anova(model, test="Chisq")

#*** means 99% confident

#Null Deviance shows how well the response variable is predicted by a model that includes #only the intercept (beta0)

#Residual Deviance shows how well the response variable is predicted with inclusion of independant variables

#Null Deviance must be greater than Residual Deviance #We remove the Insignificant variables

#Insignificant variables were removed.

#Model has all the significant variables.

model2<-glm(card ~ age + income + reports + dependents + months + owner + selfemp, family= "binomial", data=training) summary(model2) coef(model2)

###Prediction of model

res<-predict(model2, training , type="response")

head(res, 20)

#comparing with the training data

head(training,20)

#After predicting the values for the test dataset, we found that our model is predicting correct data.

MODEL ACCURACY
tab1 <- table(predicted=predic1, actual=training$card)

tab1

#Assuming Threshold to be 0.5.

#Confusion matrix says: For actual card= ‘yes’ , #model predicted FALSE for 7 times that means an #applicant cannot get the credit card. This is the error. #For actual card=‘no’, the model always predicted TRUE value. #Accuracy: (31+308)/(31+308+7+58)= 0.839 #Our model is very much accurate that is around 83% accurate

Goodness of fit:
with(model1, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail=F))

#Since p value is small, our model is significant

ROC Curve
library(ROCR)

#Receiver operating characteristic curve(ROC Curve), used to measure accuracy of the model ##As we assumed threshold to be 0.5, we predicted our model to be 83% accurate. #Accuracy is measured by area under the curve

ROCRPred = prediction(res, training$card)

ROCRPerf <- performance(ROCRPred, "tpr", "fpr")

plot(ROCRPerf)

abline(a=0,b=1)

plot(ROCRPerf, colorize = TRUE, print.cutoffs.at=seq(0.1, by=0.1))
