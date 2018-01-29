install.packages("C50") #churn dataset is present here
install.packages("caret") #knn3
library(caret)
library(C50)
set.seed(323)
data(churn)
churnTest= na.omit(churnTest)
churnTrain= na.omit(churnTrain)
#Normalizing data 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

num.vars <- sapply(churnTest, is.numeric)

#dataprocessing
train <- churnTrain[,-c(1,3,4,5)]
test <- churnTest[,-c(1,3,4,5)]
#train$churn <- as.numeric(train$churn)
str(train)
#PCA
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(train[, 1:15], method=c("center", "scale", "pca"))
print(preprocessParams$mean)
preprocessParams$dim
transformed <- predict(preprocessParams, train)
summary(transformed)
#forward step model
min.model1 = lm(churn ~ 1, data=train)
biggest1 <- formula(lm(churn~.,train))
fwd.model1 = step(min.model1, direction='forward', scope=biggest1)
plot(fwd.model1)
##
#ICA
preprocessParams <- preProcess(train[,1:15], method=c("center", "scale", "ica"), n.comp=5)
print(preprocessParams$mean)
transformed <- predict(preprocessParams, train[,1:15])
summary(transformed)
plot(transformed,col=rainbow(3))
#Random Projection
