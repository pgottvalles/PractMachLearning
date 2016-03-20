##caret example
#visualization
=============
featurePlot(x = iris[, 1:4],
            y = iris$Species,
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))




library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

set.seed(32343)
modelFit <- train(type ~.,data=training, method="glm")
modelFit

modelFit$finalModel

predictions <- predict(modelFit,newdata=testing)
predictions

confusionMatrix(predictions,testing$type)

##CROSS VALIDATION
#with k-folds

folds <- createFolds(y=spam$type,k=10,list=TRUE,returnTrain=TRUE) ##returns the training set

folds <- createFolds(y=spam$type,k=10,list=TRUE,returnTrain=FALSE) ##returns the testing set

#with resampling
set.seed(32323)
folds <- createResample(y=spam$type,times=10,list=TRUE)
#NB: resampling with replacement of the value


#Time slicing
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme,initialWindow=20,horizon=10)

#I want a set of 20 time points and I want to predict the next 10 points



## PLot Predictors

library(ISLR); library(ggplot2);
data(Wage)
inTrain <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]


featurePlot(x=training[,c("age","education","jobclass")], y = training$wage,plot="pairs")


g<-qplot(age, wage, color=education, data=training)
g<- g+geom_smooth(method='lm',formula=y~x)
g


# creates a new factor variable by cutting the specified variable into n groups 
#(3 in this case) based on percentiles
#grid.arrange can display several chart side by side
library(Hmisc);library(gridExtra);
cutWage <- cut2(training$wage,g=3)
p1 <- qplot(cutWage,age, data=training,fill=cutWage,geom=c("boxplot"))
p2 <- qplot(cutWage,age, data=training,fill=cutWage,geom=c("boxplot","jitter"))
grid.arrange(p1,p2,ncol=2)

# tabulates the cut factor variable vs another variable in the dataset
# converts a table to a proportion table 

t <- table(cutWage,training$jobclass)
prop.table(t,1)


# produces density plot for  given numeric and factor variables
qplot(wage,colour=education,data=training,geom="density")

or

cutAge<-cut2(training$age,g=5)
qplot(wage,colour=cutAge,data=training,geom="density")


##Preprocessing

# centering
#     subtracting the observations of a particular variable by its mean
# scaling
#     dividing the observations of a particular variable by its standard deviation
# normalizing
#     centering and scaling the variable --> effectively converting each observation to the number of standard deviations away from the mean


# preprocessing can be directly specified in the train function
train(y~x, data=training, preProcess=c("center", "scale"))

#you can store the result of the preProcess function as an object
# and apply it to the train and test sets using the predict function
preObj <- preProcess(training[,-58],method=c("center","scale"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
testCapAveS <- predict(preObj,testing[,-58])$capitalAve

# applies BoxCox transformations to continuous data 
# to help normalize the variables through maximum likelihood
qqnorm(processedVar) : can be used to produce the Q-Q plot
which compares the theoretical quantiles with the sample quantiles to see the normality of the data 

note it assumes continuous values and DOES NOT deal with repeated values

preObj <- preProcess(training[,-58],method=c("BoxCox"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)

# impute/estimate the missing data using k nearest neighbors (knn) 
most prediction algorithms are not build to handle missing data
=>  takes the k nearest neighbors from the missing value
    and averages the value to impute the missing observations

training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj,training[,-58])$capAve
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)
quantile(capAve - capAveTruth)


## Covariate Creation

[level 1]: construct covariate (usable metric, feature) from raw data depends heavily on application
NB: needed to have science/domain knowledgw
Google "feature extraction for [data type]" for more guidance * 
      
[level 2]: construct new covariates from extracted covariate
- generally transformations of features you extract from raw data
- used more for methods like regression and support vector machines (SVM),
whose accuracy depend more on the distribution of input variables
- best approach is through exploratory analysis (tables/plots)
- should only be performed on the train dataset
-new covariates should be added to data frames under recognizable names so they can be used later

# Dummy Variables
convert factor variables to indicator/dummy variable
=> qualitative become quantitative
dummies <- dummyVars(wage ~ jobclass,data=training)
head(predict(dummies,newdata=training))


# Removing Zero Covariates
nearZeroVar(training, saveMetrics=TRUE)

freqRatio
      ratio of frequencies for the most common value over second most common value
percentUnique
      % unique data points out of total number of data points
zeroVar
      TRUE/FALSE indicating whether the predictor has only one distinct value
nzv
      TRUE/FALSE indicating whether the predictor is a near zero variance predictor
**Note*: when nzv = TRUE, those variables should be thrown out *
      

      
# Creating Splines (Polynomial Functions)
ns() and poly() can also be used to generate polynomials
gam() function can also be used and it allows for smoothing of multiple variables with different values for each variable
**Note*: the same polynomial operations must be performed on the test sets using the predict function *
      
library(splines)
bsBasis <- bs(training$age,df=3) # create polynomial function
lm1 <- lm(wage ~ bsBasis,data=training) # fit the outcome on the three polynomial terms
plot(training$age,training$wage,pch=19,cex=0.5) # plot all age vs wage data
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5) # plot the fitted polynomial function
# predict on test values
head(predict(bsBasis,age=testing$age))



## Preprocessing with ""Principal Component Analysis"" (PCA)
ideally we want to capture the most variation with the least amount of variables 
weighted combination of predictors may improve fit
combination needs to capture the most information


2 ways:
      - SVD (https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Dimensionality_Reduction/Singular_Value_Decomposition)
      - PCA

#prcomp Function
pr<-prcomp(data)
=>performs PCA on all variables and returns a prcomp object that contains information about standard deviations and rotations 

often times, it is useful to take the log transformation of the variables and adding 1 before performing PCA
- helps to reduce skewness or strange distribution in data
- log(0) = - infinity, so we add 1 to account for zero values
- makes data more Gaussian

plot(pr)
plots the percent variation explained by the first 10 principal components (PC) 

data(spam)
prComp <- prcomp(log10(spam[,-58]+1)) # perform PCA on dataset
head(prComp$rotation[, 1:5], 5) # print out the eigenvector/rotations first 5 rows and PCs
typeColor <- ((spam$type=="spam")*1 + 1) # create new variable that marks spam as 2 and nospam as 1
# plot the first two principal components
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")


# with Caret package: perform PCA with preProcess
pp<-preProcess(log10(training[,-58]+1),method="pca",pcaComp=2,thresh=0.8))
      pcaComp=2 = specifies the number of principal components to compute (2 in this case)
      thresh=0.8 = threshold for variation captured by principal components 

predict(pp, training) = computes new variables for the PCs (2 in this case) for the training data set 
      the results from predict can then be used as data for the prediction model
      NB: the same PCA must be performed on the test set 
      

inTrain <- createDataPartition(y=spam$type,p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(training$type ~ .,method="glm",data=trainPC)

testPC <- predict(preProc,log10(testing[,-58]+1))
# compare results
confusionMatrix(testing$type,predict(modelFit,testPC))


# All that can be done directly in the train method
train(outcome ~ ., method="glm", preProcess="pca", data=training)
modelFit <- train(training$type ~ .,method="glm",preProcess="pca",data=training)


## Predicting with Regression


data(faithful)
inTrain <- createDataPartition(y=faithful$waiting, p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
lm1 <- lm(eruptions ~ waiting,data=trainFaith)
summary(lm1)
newdata <- data.frame(waiting=80)
predict(lm1,newdata)
par(mfrow=c(1,2))

# plot train data with the regression line
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",
     ylab="Duration", main = "Train")
lines(trainFaith$waiting,predict(lm1),lwd=3)
# plot test data with the regression line
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",
     ylab="Duration", main = "Test")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

# Calculate RMSE on training and test sets
c(trainRMSE = sqrt(sum((lm1$fitted-trainFaith$eruptions)^2)),
  testRMSE = sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2)))

pi<-predict(lm, newdata=test, interval="prediction") = returns 3 columns for fit (predicted value, same as before), lwr (lower bound of prediction interval), and upr (upper bound of prediction interval)
matlines(x, pi, type="l") = plots three lines, one for the linear fit and two for upper/lower prediction interval bounds

# prediction interval
pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
# plot data points (eruptions, waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
# plot fit line and prediction interval
matlines(testFaith$waiting,pred1,type="l",,col=c(1,2,2),lty = c(1,1,1), lwd=3)


# with Caret
lm <- train(y ~ x, method="lm", data=train)
=> run linear model on the training data --> identical to lm function

summary(lm$finalModel)
=> returns summary of the linear regression model, which will include coefficients, standard errors, $t$ statistics, and p values --> identical to summary(lm) for a lm object


train(y ~ ., method="lm", data=train)
=> run linear model on all predictors in training data

multiple predictors (dummy/indicator variables) are created for factor variables

plot(lm$finalModel) = construct 4 diagnostic plots for evaluating the model

**Note*: more information on these plots can be found at ?plot.lm *
      Residual vs Fitted
Normal Q-Q
Scale-Location
Residual vs Leverage
