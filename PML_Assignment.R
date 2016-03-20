packages <- c("ggplot2", "caret", "data.table", "randomForest", "stats", "scales", "gridExtra")
sapply(packages, require, character.only = TRUE, quietly = TRUE, warn = FALSE)

#extracting data

raw_train_dat <- read.csv('Machine Learning/pml-training.csv', na.strings=c("#DIV/0!", "", "NA"), strip.white=T, header=TRUE)
raw_test_dat <- read.csv('Machine Learning/pml-testing.csv', na.strings=c("#DIV/0!", "", "NA"), strip.white=T, header=TRUE)


#cleaning data
dim(raw_train_dat)
#[1] 19622   160

dataset_row_nb<-dim(raw_train_dat)[1]

col_name_80p_na<-(colSums(is.na(raw_train_dat)) > 0.9*dataset_row_nb)

raw_train_dat<-raw_train_dat[, !(col_name_80p_na)]
raw_test_dat<-raw_test_dat[, !(col_name_80p_na)]


#removing book keeping columns
raw_train_dat<-raw_train_dat[, -c(1:7)]
raw_test_dat<-raw_test_dat[, -c(1:7)]

#Partitioning the dataset
set.seed(222) #sets the seed
inTrain <- createDataPartition(y=raw_train_dat$classe,p=0.6, list=FALSE)
training <- raw_train_dat[inTrain,]
testing <- raw_train_dat[-inTrain,]


#the dataset is quite large and laptop is old => better to try to get the most importatnt variable at the first shot


col_classe_nb<-dim(training)[2]

#preObj <- preProcess(training[,-col_classe_nb],method=c("center","scale"))
#trainPC<-predict(preObj,training)

preProc <- preProcess(training[,-col_classe_nb],method="pca",thresh=0.8)
trainPC<-predict(preProc,training)
testPC<-predict(preProc,testing)

ctrl <- trainControl(allowParallel=T, method="cv", number=4) ## do 4-fold cross validation with parallel processing
mod_rpart <- train(training$classe ~ ., data=trainPC, trControl=ctrl, method="rpart")
mod_rf <- train(training$classe ~ ., data=trainPC, trControl=ctrl, method="rf")
mod_lda <- train(training$classe ~ ., data=trainPC, trControl=ctrl, method="lda")


resamps <- resamples(list(rpart = mod_rpart, rf = mod_rf, lda = mod_lda))

summary(resamps)$statistics$Accuracy
#Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
#rpart 0.4039  0.4054 0.4086 0.4081  0.4113 0.4113    0
#rf    0.9274  0.9337 0.9359 0.9348  0.9370 0.9399    0
#lda   0.4597  0.4623 0.4655 0.4657  0.4689 0.4720    0



train_pred_rpart<-predict(mod_rpart,newdata=trainPC)
train_acc_rpart<-confusionMatrix(train_pred_rpart,training$classe)$overall[1]
train_acc_rpart
#Accuracy 
#0.3538553

train_pred_rf<-predict(mod_rf,newdata=trainPC)
train_acc_rf<-confusionMatrix(train_pred_rf,training$classe)$overall[1]
train_acc_rf
#Accuracy 
#1 

train_pred_lda<-predict(mod_lda,newdata=trainPC)
train_acc_lda<-confusionMatrix(train_pred_lda,training$classe)$overall[1]
train_acc_lda
#Accuracy 
#0.4725713


#So it seems that random forest is the most efficient model

lets test it on testing dataset

test_pred_rpart<-predict(mod_rpart,newdata=testPC)
test_acc_rpart<-confusionMatrix(test_pred_rpart,testing$classe)$overall[1]
test_acc_rpart
#Accuracy 
#0.3459087 

test_pred_rf<-predict(mod_rf,newdata=testPC)
test_acc_rf<-confusionMatrix(test_pred_rf,testing$classe)$overall[1]
test_acc_rf
#Accuracy 
#0.9567933

test_pred_lda<-predict(mod_lda,newdata=testPC)
test_acc_lda<-confusionMatrix(test_pred_lda,testing$classe)$overall[1]
test_acc_lda
#Accuracy 
#0.4725713

=> even on the test dataset the random forest is fitting very well

#Data prediction on raw_test_dat dataset
Lets see what it gives for the raw_test_data

raw_test_dat_PC<-predict(preProc,raw_test_dat)

raw_test_pred_rf<-predict(mod_rf,newdata=raw_test_dat_PC)
raw_test_acc_rf<-confusionMatrix(raw_test_pred_rf,testing$classe)$overall[1]
test_acc_rf
