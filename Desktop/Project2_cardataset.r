library(rpart)
library(pROC)
library(ROCR)
library(ada) # Boosting
library(rpart.plot)
library(caret)
library(adabag)
set.seed(323)
#Reading and viewing the data 

cardata <- read.csv("~/Desktop/College course material/Fall_2017/ML/Project2/car.data.csv", sep = ',', header = FALSE)
View(cardata)
nrow(cardata)
#Creating Training and Test data sets
training1=sample(nrow(cardata), 0.75*nrow(cardata), replace=FALSE)
trainingH1=cardata[training1,]

testDS1=cardata[-training1,]
#training decision tree on training data
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
#Tree Pruning
#pfit2<- prune(dtree_fit, 
#             cp= dtree_fit$cptable[which.min(dtree_fit$cptable[,"xerror"]),"CP"])
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
