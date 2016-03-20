## Prediction Tree

par(mar=c(1,1,1,1), mfrow = c(1, 2)); set.seed(1234);


data(iris)
inTrain <- createDataPartition(y=iris$Species,p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
modFit <- train(Species ~ .,method="rpart",data=training)
print(modFit$finalModel)

gives
      n= 105 
      node), split, n, loss, yval, (yprob)
      * denotes terminal node
      1) root 105 70 setosa (0.33333333 0.33333333 0.33333333)  
      2) Petal.Length< 2.45 35  0 setosa (1.00000000 0.00000000 0.00000000) *
      3) Petal.Length>=2.45 70 35 versicolor (0.00000000 0.50000000 0.50000000)  
      6) Petal.Width< 1.65 34  1 versicolor (0.00000000 0.97058824 0.02941176) *
      7) Petal.Width>=1.65 36  2 virginica (0.00000000 0.05555556 0.94444444) *
      
library(rattle)
fancyRpartPlot(modFit$finalModel)
predict(modFit,data=testing)


## Bagging
bootstrap aggregating

# load relevant package and data
library(party); data(ozone,package="ElemStatLearn")
# reorder rows based on ozone variable
ozone <- ozone[order(ozone$ozone),]
# extract predictors
predictors <- data.frame(ozone=ozone$ozone)
# extract outcome
temperature <- ozone$temperature
# run bagging algorithm
treebag <- bag(predictors, temperature, B = 10,
               # custom bagging function
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))
# plot data points
plot(ozone$ozone,temperature,col='lightgrey',pch=19)
# plot the first fit
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
# plot the aggregated predictions
points(ozone$ozone,predict(treebag,predictors),pch=19,col="blue")



## Random Forest
one of most commonly used algorythm with boosting
# load data
data(iris)
# create train/test data sets
inTrain <- createDataPartition(y=iris$Species,p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
# apply random forest
modFit <- train(Species~ .,data=training,method="rf",prox=TRUE)
# return the second tree (first 6 rows)
head(getTree(modFit$finalModel,k=2))
# compute cluster centers
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
# convert irisP to data frame and add Species column
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)
# plot data points
p <- qplot(Petal.Width, Petal.Length, col=Species,data=training)
# add the cluster centers
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP)
# predict outcome for test data set using the random forest model
pred <- predict(modFit,testing)
# logic value for whether or not the rf algorithm predicted correctly
testing$predRight <- pred==testing$Species
# tabulate results
table(pred,testing$Species)
# plot data points with the incorrect classification highlighted
qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,main="newdata Predictions")



## Boosting

differente method of boosting in caret package
options for method for boosting

gbm - boosting with trees
mboost - model based boosting
ada - statistical boosting based on additive logistic regression
gamBoost for boosting generalized additive models

load data
data(Wage)
# remove log wage variable (we are trying to predict wage)
Wage <- subset(Wage,select=-c(logwage))
# create train/test data sets
inTrain <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
# run the gbm model
modFit <- train(wage ~ ., method="gbm",data=training,verbose=FALSE)
# print model summary
print(modFit)



## Model Based Prediction
# Linear Discriminant Analysis
R Commands
lda<-train(outcome ~ predictors, data=training, method="lda")
      => constructs a linear discriminant analysis model
      on the predictors with the provided training data
predict(lda, test)
      => applies the LDA model to test data
      and return the prediction results in data frame

example: caret package
data(iris)
inTrain <- createDataPartition(y=iris$Species,p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
# run the linear discriminant analysis on training data
lda <- train(Species ~ .,data=training,method="lda")
# predict test outcomes using LDA model
pred.lda <- predict(lda,testing)
# print results
pred.lda



# Naive Bayes
R Commands

nb <- train(outcome ~ predictors, data=training, method="nb")
      => constructs a naive Bayes model on the predictors
      with the provided training data
predict(nb, test)
      => applies the naive Bayes model to test data
      and return the prediction results in data frame
example: caret package
# using the same data from iris, run naive Bayes on training data
nb <- train(Species ~ ., data=training,method="nb")
# predict test outcomes using naive Bayes model
pred.nb <- predict(nb,testing)
# print results
pred.nb