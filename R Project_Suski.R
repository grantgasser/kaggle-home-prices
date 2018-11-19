###################### 
# Final Project
# Grant Gasser & Matt Suski
# Fall 2018
#####################

library(GGally)

list.files(path = "../project/") 

train = read.csv('../project/train.csv', header=T)
test = read.csv('../project/test.csv', header=T)
print('Dimensions of the test and train data: ')
dim(test)
dim(train)
num_train = dim(train)[1]
num_test = dim(test)[1]

#Take a look at X
head(train)[1:10]

#Remove ID from training and test data
ids = test$Id #test Ids stored for submission file later
train = train[,-1]
test = test[,-1]

#Set Y to SalePrice and delete the column
Y = train$SalePrice
train$SalePrice = NULL

#Combine train and test data to examine NAs
all_data = rbind(train, test)
dim(all_data)
head(all_data)[1:10]

#Which columns have NA and how many do they have?
NAcol = which(colSums(is.na(all_data)) > 0)
sort(colSums(sapply(all_data[NAcol], is.na)), decreasing = TRUE)

#Remove columns with NA
all_data = all_data[,-NAcol]
head(all_data)[1:45] #can see Alley was removed

str(all_data)

################ NUMERIC
all_data.numeric = all_data[,which(sapply(all_data, is.numeric))]

train.numeric = all_data.numeric[1:num_train,]

library(corrplot)

M <- cor(train.numeric)
corrplot(M, method = "circle", type="lower")

#library(car)
#vif(train.numeric)
########################

table(train$LotShape)

train = all_data[1:num_train,]
test = all_data[(num_train+1):(num_train+num_test),]
fit <- lm(Y ~ ., data=train)

summary(fit)

#Look at residuals v. fitted, can see issue with linearity more clearly here
plot(fit, which = 1)

#Does not look like constant variance or normal

#Look at normality, does not seem to fit, note 826, 524, 1183
plot(fit, which=2)

#Try looking at distribution of Y
hist(Y)

#Look at log transform
hist(log(Y))

#Try fitting with tranformed Y
fit <- lm(log(Y) ~ ., data=train)

#summary(fit)

#Look at residuals v. fitted, can see issue with linearity more clearly here
plot(fit, which = 1)
plot(fit, which = 2)

#Keep an eye on 826, 633, 524
#Also 121, 251, 272, 326, 584, 1004, 1231, 1271, 1276, 1299, 1322 not on plot

########## Suski Lasso

### install dummy packages to transform dataset into dummy variables
install.packages("dummy")
install.packages("dummies")
library(dummy)
library(dummies)

#NOTE: dummy == 1-Hot encoding

# Make new dataset with all factor variables and YrSold as dummy variables
dummy <- dummy.data.frame(train,names = "YrSold", dummy.classes = "factor")

# figure out what is going on 
help(dummy.data.frame)

# check dimensions of new dataset
dim(dummy) 

# Fit a model matrix with predictor variables and the dummy set
newX = model.matrix(log(Y)~.,data=dummy)

# fit the lasso model using cv.glmnet
fit.lasso = cv.glmnet(x=newX,y=log(Y),alpha=1,nfolds=5)

# visualize the lasso model
plot(fit.lasso)

# view coefficients lasso model 1SD away from min used 
coef(fit.lasso, s='lambda.1se', exact=T)
lambda_se <- fit.lasso$lambda.1se

### Check normality

# Get prediction
y.hat=predict(fit.lasso, newX, s=lambda_se)

# Define residuals
res=as.numeric(log(Y)-y.hat)

# plot residuals
qqPlot(res)

# test residuals for significance
shapiro.test(res)
# Data is not normally distributed

### Get MSE of the data
MSE = sum((res^2)/(nrow(train)-33))
MSE
# Model is similar to the one by res subsets


### Make predictions for test set

#First make same adjustments to test set w/ dummy vars
# Make new dataset with all factor variables and YrSold as dummy variables
dummy.test <- dummy.data.frame(test, names = "YrSold", dummy.classes = "factor")

# check dimensions of new dataset
dim(dummy.test) 

# Fit a model matrix with predictor variables and the dummy set
#newX.test = model.matrix(log(Y)~.,data=dummy.test)


#dummy.test <- data.matrix(dummy.test)
require(methods)
test.predictions.lasso <- predict(fit.lasso, data.matrix(dummy.test), s=lambda_se)

#Error: https://stackoverflow.com/questions/35737485/how-to-predict-in-glmnet-when-response-outcome-is-unknown


# Check MSE with test data
### Not working don't know why
#test = all_data[(num_train+1):(num_test+num_train),]
#Y.test = test$
#dim(test)
#dummy.test <- dummy.data.frame(test,names = "YrSold",dummy.classes = "factor")
#X.test <- model.matrix(log(Y)~.,data=dummy.test)

#head(test)
#head(train)
#y.hat.test = predict(fit.lasso,dummy.test,type="response")
#res.test = as.numeric(log(Y))
