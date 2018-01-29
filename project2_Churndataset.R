#####RUNNING MODELS ON THE NEW DATA (MISSING VALUES REMOVED)################

library(rpart.plot)
library(caret)
library(e1071) #SVM
library(rpart)
library(pROC)
library(ada) # Boosting
set.seed(323)
#Reading and viewing the data 
h1=read.csv("C:/AML - BUAN 6341", header=TRUE)
h=na.omit(h1)
#Bar plot of the Churn varialbe
barplot(height =cbind(sum(h$Churn=="Yes"), sum(h$Churn=="No")), axisnames=TRUE, main="Chart1", names.arg = c("Yes", "No"), col=c("red", "blue"), xlab = "Churn", beside=TRUE)
table(h$Churn)

#Creating Training and Test data sets
training=sample(nrow(h), 0.75*nrow(h), replace=FALSE)
trainingH=h[training,]
testDS=h[-training,]

#creating single tree model and testing the training data (not the test data)
Htreemodel=rpart(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV
                 +StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges, data=trainingH)
printcp(Htreemodel)# will be used in pruning 
plotcp(Htreemodel) #visualize cross-validation results
#Plotting tree
#plot(Htreemodel,uniform=TRUE,main="Classification Tree")
#text(Htreemodel, use.n=TRUE, all=TRUE, cex=.7)
# or use
prp(Htreemodel, box.palette = "Reds", tweak = 1.1)
prediction=predict(Htreemodel, trainingH, type=c("class"))
cmatrix=confusionMatrix(trainingH$Churn, prediction)
print(cmatrix) #Accuracy details without Pruning
print(cmatrix$table)
#test data
prediction5=predict(Htreemodel,testDS, type=c("class"))
cmatrix5=confusionMatrix(testDS$Churn, prediction5)
print(cmatrix5) #Accuracy details without Pruning
print(cmatrix5$table)
#Pruning the tree
pfit<- prune(Htreemodel, 
     cp= Htreemodel$cptable[which.min(Htreemodel$cptable[,"xerror"]),"CP"])
# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Classification Tree - After Pruning")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
#or plotting by prp function
prp(pfit, box.palette = "Reds", tweak = 1.1)
#Prediction after pruning
prediction13=predict(pfit, trainingH, type=c("class"))
cmatrix13=confusionMatrix(trainingH$Churn, prediction13)
print(cmatrix13)
prediction12=predict(pfit, testDS, type=c("class"))
cmatrix12=confusionMatrix(testDS$Churn, prediction12)
print(cmatrix12)
#testing the test data
prediction1=predict(Htreemodel, testDS, type=c("class"))
cmatrix1=confusionMatrix(testDS$Churn, prediction1)
print(cmatrix1)
##
##GINI Index
Htreemodel1=rpart(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV
                 +StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges, data=trainingH,parms = list(split = "gini"))
printcp(Htreemodel1)# No changes in CP hence Gini index will be same
prediction12=predict(Htreemodel1, testDS, type=c("class"))
cmatrix12=confusionMatrix(testDS$Churn, prediction12)
print(cmatrix1)
#Boosting 
adaModel <- ada(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+
                  OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges
                +TotalCharges, data=trainingH,control=rpart.control(maxdepth=30,cp=0.010000,minsplit=20,xval=10),iter=50)

predictions6<-predict(adaModel,trainingH)
cmatrix6<-confusionMatrix(trainingH$Churn,predictions6)
print(cmatrix6$table) #Please uncomment this to see table.
print(cmatrix6)
predictions7<-predict(adaModel,testDS)
cmatrix7<-confusionMatrix(testDS$Churn,predictions7)
print(cmatrix7)

#SVM linear kernel
svm_model_init <- svm(Churn~ ., data=trainingH, method="C-classification", kernel="linear")
#Tuning and finind the best gamma and cost value to use further
obj <- tune(svm, Churn~., data = trainingH, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
            tunecontrol = tune.control(sampling = "fix")
)
obj
#Changing SVM function accordingly
svm_model<- svm(Churn~ ., data=trainingH, method="C-classification", kernel="linear",gamma =.5,
                cost=4)
#Prediction - Training
pred_train_svm <-predict(svm_model,trainingH)
mean(pred_train_svm==trainingH$Churn)
#Test data set Prediction
pred_test_svm <-predict(svm_model,testDS)
mean(pred_test_svm==testDS$Churn)
#SVM radial kernel - default parameters
#svm_model2 <- svm(Churn~ ., data=trainingH, method="C-classification", kernel="radial")
svm_model2 <- svm(Churn~ ., data=trainingH, method="C-classification", kernel="radial",gamma =.5,
                  cost=4)
#Prediction - Training
pred_train_svm2 <-predict(svm_model2,trainingH)
mean(pred_train_svm2==trainingH$Churn)
#Test data set Prediction
pred_test_svm2 <-predict(svm_model2,testDS)
mean(pred_test_svm2==testDS$Churn)
#Both predicted value is close to each other unlike linear but much lower hence not apt
# SVM polynomial
svm_model3 <- svm(Churn~ ., data=trainingH, method="C-classification", kernel="polynomial",gamma =.5,
                  cost=4)
svm_model3
#Prediction - Training
pred_train_svm3 <-predict(svm_model3,trainingH)
mean(pred_train_svm3==trainingH$Churn)
#Test data set Prediction
pred_test_svm3 <-predict(svm_model3,testDS)
mean(pred_test_svm3==testDS$Churn)
#Training for polynomial came as 1 afrer tuning.
#######

######
rm(list = ls())
###second code
library(rpart)
library(pROC)
library(ROCR)
library(ada) # Boosting
library(rpart.plot)
library(caret)
library(adabag)
set.seed(323)
#Reading and viewing the data 

cardata <- read.csv("C:/AML - BUAN 6341", sep = ',', header = FALSE)
View(cardata)
nrow(cardata)
#Creating Training and Test data sets
training1=sample(nrow(cardata), 0.75*nrow(cardata), replace=FALSE)
trainingH1=cardata[training1,]

testDS1=cardata[-training1,]
#dtree_fit<- rpart(Rating~BuyingPrice+Doors+Maintenance+PersonCapacity+LuggageArea+Safety, method="class",data=trainingH1)
dtree_fit <- train(V7 ~., data = trainingH1, method = "rpart",
                   parms = list(split = "information"),
                   tuneLength = 10)
#final cp value used is .127
#Visualization of tree
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1)
#training dataset
training_pred1 = predict(dtree_fit, newdata =trainingH1)
cmatrix11=confusionMatrix(trainingH1$V7, training_pred1)
print(cmatrix11) #Accuracy details without Pruning
print(cmatrix11$table)
#Model prediction
test_pred1 <- predict(dtree_fit, newdata = testDS1)
#confusion matrix
conf1<-confusionMatrix(test_pred1,testDS1$V7)
print(conf1$table) #accuracy .872
#Gini Index
dtree_fit_gnew <- train(V7 ~., data = trainingH1, method = "rpart",
                        parms = list(split = "gini"),
                        tuneLength = 10)
dtree_fit_gnew
#decision tree GINI Index
prp(dtree_fit_gnew$finalModel, box.palette = "Reds", tweak = 1)
#model prediction _ GINI INDEX
test_pred2 <- predict(dtree_fit_gnew, newdata = testDS1)
#confusion matrix
conf2<-confusionMatrix(test_pred2,testDS1$V7)
print(conf2$table) # accuracy and details .86
##
#SVM linear kernel
svm_model_init <- svm(V7~ ., data=trainingH1, method="C-classification", kernel="linear")
#Tuning and finind the best gamma and cost value to use further
obj <- tune(svm, V7~., data = trainingH1, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
            tunecontrol = tune.control(sampling = "fix")
)
obj
#Changing SVM function accordingly
svm_model<- svm(V7~ ., data=trainingH1, method="C-classification", kernel="linear",gamma =.5,
                cost=4)
#Prediction - Training
pred_train_svm <-predict(svm_model,trainingH1)
mean(pred_train_svm==trainingH1$V7)
#Test data set Prediction
pred_test_svm <-predict(svm_model,testDS1)
mean(pred_test_svm==testDS1$V7)
#SVM radial kernel - default parameters
#svm_model2 <- svm(Churn~ ., data=trainingH, method="C-classification", kernel="radial")
svm_model21 <- svm(V7~ ., data=trainingH1, method="C-classification", kernel="radial",gamma =.5,
                   cost=4)
#Prediction - Training
pred_train_svm21 <-predict(svm_model21,trainingH1)
mean(pred_train_svm21==trainingH1$V7)
#Test data set Prediction
pred_test_svm21 <-predict(svm_model21,testDS1)
mean(pred_test_svm21==testDS1$V7)
#Both predicted value is close to each other unlike linear but much lower hence not apt
# SVM polynomial
svm_model3 <- svm(V7~ ., data=trainingH1, method="C-classification", kernel="polynomial",gamma =.5,
                  cost=4)
#Prediction - Training
pred_train_svm3 <-predict(svm_model3,trainingH1)
mean(pred_train_svm3==trainingH1$V7)
#Test data set Prediction
pred_test_svm3 <-predict(svm_model3,testDS1)
mean(pred_test_svm3==testDS1$V7)
#Training for polynomial came as 1 afrer tuning.
#Boosting $$$
adaModel1 <- boosting(V7~., data=trainingH1, boos=TRUE, 
                      mfinal=6)
#
predictions8<-predict(adaModel1,trainingH1)
predictions8 #99.07%
predictions9 <- predict(adaModel1, newdata = testDS1)
predictions9# Accuracy.9652 

