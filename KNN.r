#library(sjPlot) # to check k in knn method
library(caret)
#library(class)
#install.packages("sjmisc")
set.seed(323)
###
h1=read.csv("~/Desktop/College course material/Fall_2017/ML/Project2/churn_dataset.csv", header=TRUE)
h=na.omit(h1)
#Bar plot of the Churn varialbe
barplot(height =cbind(sum(h$Churn=="Yes"), sum(h$Churn=="No")), axisnames=TRUE, main="Chart1", names.arg = c("Yes", "No"), col=c("red", "blue"), xlab = "Churn", beside=TRUE)
#Creating Training and Test data sets
training=sample(nrow(h), 0.75*nrow(h), replace=FALSE)
trainingH=h[training,]
testDS=h[-training,]
##checking for k in knn
#sjc.elbow() #
#using knn3 of caret
model <- knn3(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV
              +StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges,data = trainingH,k=2)
predTrain<-predict(model,trainingH,type="class") 
conftrain1 <- confusionMatrix(trainingH$Churn,predTrain)
conftrain1 #my accuracy is around 86% here
#model on test dataset
predTest <- predict(model,testDS,type="class")
conftest1 <- confusionMatrix(testDS$Churn,predTest)
conftest1 #accuracy is around 72%
#USing K = 3
model2 <- knn3(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV
              +StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges,data = trainingH,k=3)
predTrain2<-predict(model2,trainingH,type="class") 
conftrain2 <- confusionMatrix(trainingH$Churn,predTrain2)
conftrain2 #my accuracy is around 86% here
#model on test dataset
predTest2 <- predict(model2,testDS,type="class")
conftest2 <- confusionMatrix(testDS$Churn,predTest2)
conftest2 #accuracy is around 75%
#############K=5
model3 <- knn3(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV
               +StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges,data = trainingH,k=5)
predTrain3<-predict(model3,trainingH,type="class") 
conftrain3 <- confusionMatrix(trainingH$Churn,predTrain3)
conftrain3 #my accuracy is around 82% here
#model on test dataset
predTest3 <- predict(model3,testDS,type="class")
conftest3 <- confusionMatrix(testDS$Churn,predTest3)
conftest3 #accuracy is around 76%
####k=7
model4 <- knn3(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV
               +StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges,data = trainingH,k=7)
predTrain4<-predict(model4,trainingH,type="class") 
conftrain4 <- confusionMatrix(trainingH$Churn,predTrain4)
conftrain4 #my accuracy is around 82% here
#model on test dataset
predTest4 <- predict(model4,testDS,type="class")
conftest4 <- confusionMatrix(testDS$Churn,predTest4)
conftest4 #accuracy is around 76%