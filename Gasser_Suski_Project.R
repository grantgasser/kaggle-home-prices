###################### 
# Final Project
# Grant Gasser & Matt Suski
# Fall 2018
#####################

library(GGally)

list.files(path = "../Project/")

train = read.csv('../Project/train.csv', header=T)
test = read.csv('../Project/test.csv', header=T)
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

################ LOOK AT NUMERICAL VARIABLES CORRELATION WITH Y 
all_data.numeric = all_data[,which(sapply(all_data, is.numeric))]

train.numeric = all_data.numeric[1:num_train,]

#add SalePrice back in to see correlation of each variable with Y
train.numeric = cbind(train.numeric, Y)

library(corrplot)

M <- cor(train.numeric)
corrplot(M, method = "circle", type="lower")

#Numerical variables to use: OverallQual, YearBuilt, YearRemodAdd, GrLiving, FullBath, TotRmsAbvGrd, Fireplaces

########################

#Re-set train and test data
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

head(Y)

#Try fitting with tranformed Y
fit <- lm(log(Y) ~ ., data=train)

plot(fit, which = 1)
plot(fit, which = 2)

#Keep an eye on 826, 633, 524
#Also 121, 251, 272, 326, 584, 1004, 1231, 1271, 1276, 1299, 1322 not on plot


############### REGSUBSETS

#Numerical variables to use: OverallQual, YearBuilt, YearRemodAdd, GrLivArea, FullBath, TotRmsAbvGrd, Fireplaces
#Categorical variables: ExterQual, Neighborhood, SaleCondition, HouseStyle, MoSold

#Now use regsubsets with the BIC/SBC criterion to get a final model
fit.test <- lm(log(Y) ~ OverallQual + YearBuilt + YearRemodAdd + GrLivArea + FullBath + TotRmsAbvGrd + Fireplaces
               + ExterQual + Neighborhood + SaleCondition + HouseStyle, data=train)
#Note how lm created dummy variables for us
summary(fit.test)


library(leaps)
regsubsets.out <- regsubsets(log(Y) ~ OverallQual + YearBuilt + YearRemodAdd + GrLivArea + FullBath + TotRmsAbvGrd + Fireplaces
                             + ExterQual + Neighborhood + SaleCondition + HouseStyle,
                             data = train,
                             nbest = 1,
                             method = "exhaustive", really.big=T)
#mat = summary(regsubsets.out)
#cbind(mat$outmat, mat$bic)[order(mat$bic),]
plot(regsubsets.out, scale = "bic", main = expression(BIC))

#TotRmsAbvGrd, FullBath, SaleCondition, ExterQual seem to be uneccessary, keep the rest of the variables.
fit.new <- lm(log(Y) ~ OverallQual + YearBuilt + YearRemodAdd + GrLivArea + Fireplaces + Neighborhood + HouseStyle, data=train)
summary(fit.new)

plot(fit.new, which=1)

#Note: 524 and 1299 overestimated by the model to large degree

library(car)
vif(fit.new)

#Neighborhood has high VIF, remove
fit.new <- lm(log(Y) ~ OverallQual + YearBuilt + YearRemodAdd + GrLivArea + Fireplaces + HouseStyle, data=train)
vif(fit.new) #Low VIFs, seem to have eliminated multicollinearity

#Tried Log transform to GrLivingArea
#fit.new <- lm(log(Y) ~ OverallQual + YearBuilt + YearRemodAdd + log(GrLivArea) + Fireplaces + HouseStyle, data=train)
#vif(fit.new) #Low VIFs, good model

#### Begin outlier analysis ####

#Get prediction
yhat <- predict(fit.new, train)

plot(fit.new, which=1)

#As we saw with the initial fit, 1299 and 524 are outliers

#Outliers wrp to Y: 1299, rstudent = -12.96, 524 rstudent = -8.7
outlierTest(fit.new)

dat.tmp = data.frame(fitted.values = fit.new$fitted.values, residuals = rstudent(fit.new))

n = nrow(dat.tmp)
p = 6

#Outliers wrp to X: Lots of outliers wrp to X! (see 524, 1299)
hii <- hatvalues(fit.new)
names(which(hii>2*p/n))

#Influential Points: lots, notably 524, 1299
influ <- dffits(fit.new)
names(which(abs(influ) >  2*sqrt(p/n)))

#Cook's Distance: 1299 by far the most influential, then 524, then 329
plot(fit.new, which = 4)

#Mean of a couple variables
mean(Y)
mean(OverallQual)
mean(GrLivArea)

#Take a look at 1299 
Y[1299] #160000, which is 20K less than the mean, that is we the model overestimated
train[1299,]

#OverallQual=10, GRLivArea=5642
#So these are quite higher, would expect a high SalePrice, but SalePrice is actually

#tranformed response for this observation, residual is about -2 which is large
log(Y[1299]) - yhat[1299]

#Look at 524 (was overestimated)
train[524,]
Y[524]

#$184K, Sold for about the mean despite OverallQual = 10 and super high GrLivArea of 4676, has fireplace, leading model to overestimate

#Note: both of these have SaleCondition=partial, might explain why they sold for less than expected

#SaleCondition=Partial means home was not completed when last assessed (associated with New Homes), per data information

#Look at influence on Betas, can see 1299 and 524 having large effect on betas
dfbetasPlots(fit.new)
which(dfbetas(fit.new)[,2] > 1)

qqPlot(rstudent(fit.new)) #still over estimating a large amount of homes

#Many of the overestimated home prices were homes with not Normal SaleCondition and/or sold in 07-08
overestimated <- which(rstudent(fit.new) < (-3))
table(train[overestimated,]$YrSold)
table(train[overestimated,]$SaleCondition)

#WHY DOES MODEL OVERESTIMATE THESE? Hypothesis: 07-08 housing crisis. Many large, nice houses went unfinished and 
#sold below typical market value

#Note: the houses that are vastly overestimated were all sold around 2007 and 2008, YrSold not specific enough
shapiro.test(rstudent(fit.new))

#Test independence of residuals
library(lmtest)
dwtest(fit.new) 
bgtest(fit.new)

#Both tests: Not enough evidence to say the residuals are not independent

#Test whether residuals have constant variance
bptest(fit.new) 
#Lots of evidence to suggest the residuals do not have constant variance, no surprise

#Remember:
plot(fit.new, which=1)

#.0275 compared to 0.0226 MSE from Lasso (done later)
res <- log(Y) - yhat
mse.1 <- sum((res)^2/(n-p))
mse.1

#Make predictions on test data based on model found by regsubsets
test.predictions <- predict(fit.new, test)
test.predictions <- exp(test.predictions) #undo log transform for submission

#Prepare for submission on Kaggle
submission <- cbind(ids, test.predictions)
colnames(submission) <- c("Id", "SalePrice")

#Output to csv file
write.csv(submission, file="../Project/output.csv", row.names=FALSE)

#Results: RMSE = 0.16589, 3175th place out of 4313 (26th percentile)

###### Try dropping top 3 influential
train <- train[-c(1299,524,329),]
Y <- Y[-c(1299,524,329)]

fit.drop <- lm(log(Y) ~ OverallQual + YearBuilt + YearRemodAdd + GrLivArea + Fireplaces + HouseStyle, data=train)
test.predictions.drop <- predict(fit.drop, test)
test.predictions.drop <- exp(test.predictions.drop)

#Prepare for submission on Kaggle
submission <- cbind(ids, test.predictions.drop)
colnames(submission) <- c("Id", "SalePrice")

#Output to csv file
write.csv(submission, file="../Project/output2.csv", row.names=FALSE)

#Results: RMSE = 0.16472, 3157th place out of 4313 (27th percentile)
######


####################

########## Lasso

### install dummy packages to transform dataset into dummy variables since glmnet does not do this for us
#install.packages("dummy")
#install.packages("dummies")
library(dummy)
library(dummies)

#NOTE: dummy == 1-Hot encoding

# Make new dataset with all factor variables and YrSold as dummy variables
dummy <- dummy.data.frame(train, names = "YrSold", dummy.classes = "factor")
dummy.test <- dummy.data.frame(test, names = "YrSold", dummy.classes = "factor")

#dummy 
#help(dummy.data.frame)

# check dimensions of new dataset
dim(dummy) 

# Fit a model matrix with predictor variables and the dummy set
newX = model.matrix(log(Y)~.,data=dummy)
test = data.frame(dummy.test)

# fit the lasso model using cv.glmnet
fit.lasso = cv.glmnet(x=newX,y=log(Y),alpha=1,nfolds=5)

# visualize the lasso model
plot(fit.lasso)

# view coefficients lasso model 1SD away from min used 
coef(fit.lasso, s='lambda.1se', exact=T)
lambda_se <- fit.lasso$lambda.1se

#Keep: SaleConditionPartial, ScreenPorch, WoodDeckSF, PavedDriveY, Fireplaces, GrLivArea, X1stFlrSF,
#HeatingQCTA, CentralAirY, FoundationPConc, FoundationSlab, ExterQualTA, ExterCondFa, etc.
keep.vars <- which(abs(coef(fit.lasso, s="lambda.1se", exact=T)) > 0) - 1
keep.vars <- keep.vars[-1] #get rid of intercept
newX <- newX[, keep.vars]

#Fit Lasso again!
fit.lasso.2 <- cv.glmnet(x=newX,y=log(Y),alpha=1,nfolds=5)

# visualize the lasso model
plot(fit.lasso.2)

# view coefficients lasso model 1SD away from min used 
coef(fit.lasso.2, s='lambda.1se', exact=T)
lambda_se <- fit.lasso.2$lambda.1se

keep.vars <- which(abs(coef(fit.lasso.2, s="lambda.1se", exact=T)) > 0) - 1
keep.vars <- keep.vars[-1] #get rid of intercept
newX <- newX[, keep.vars]

#Fit Lasso a third time
fit.lasso.3 <- cv.glmnet(x=newX,y=log(Y),alpha=1,nfolds=5)

# visualize the lasso model
plot(fit.lasso.3)

# view coefficients lasso model 1SD away from min used 
coef(fit.lasso.3, s='lambda.min', exact=T)
lambda_min <- fit.lasso.3$lambda.min

#25-40 variables left
final.vars <- which(abs(coef(fit.lasso.3, s="lambda.min", exact=T)) > 0) - 1
newX[, final.vars]

newX = data.frame(newX)
#Run regsubsets
regsubsets.out <- regsubsets(log(Y) ~ .,
                             data = newX,
                             nbest = 2,
                             method = "exhaustive", really.big=T)

#mat = summary(regsubsets.out)
#cbind(mat$outmat, mat$bic)[order(mat$bic),]
plot(regsubsets.out, scale = "bic", main = expression(BIC))


#Best model according to BIC includes SaleCondition, BldgtTypeTwnns, OverallQual, OverallCond, YearBuilt
#X1stFlrSF, GrLivArea, Fireplaces
#attach(newX)
fit.final <- lm(log(Y) ~ OverallQual + SaleConditionPartial + SaleConditionNormal + BldgTypeTwnhs + YearBuilt + X1stFlrSF + GrLivArea + Fireplaces, data=newX)

### Check normality

# Get prediction on training set
y.hat=predict(fit.final, newX)
res = as.numeric(log(Y) - y.hat)

########################################### TODO

#Transform test data to same dummy format of train data
test = all_data[(num_train+1):(num_test+num_train),]
dim(test)
dummy.test <- dummy.data.frame(test,names = c("YrSold", "SaleCondition", "BldgType"),dummy.classes = "factor")

#Make predictions on test data based on "Relaxed Lasso"
test.predictions <- predict(fit.final, dummy.test)
test.predictions <- exp(test.predictions) #undo log transform for submission

#Prepare for submission on Kaggle
submission <- cbind(ids, test.predictions)
colnames(submission) <- c("Id", "SalePrice")

#Output to csv file
write.csv(submission, file="../Project/output_lasso.csv", row.names=FALSE)


####Model selection with Lasso moved us up on leaderboard 112 places, RMSE = .16044

# plot residuals
qqPlot(fit.final)

# test residuals for significance, residuals not normal
shapiro.test(res)

#Test independence of residuals
library(lmtest)
dwtest(fit.final) 
bgtest(fit.final)

#Both tests: Not enough evidence to say the residuals are not independent

#Test whether residuals have constant variance
bptest(fit.new) 
#Lots of evidence to suggest the residuals do not have constant variance, no surprise

#Remember:
plot(fit.new, which=1)

### Get MSE of the data
MSE = sum((res^2)/(nrow(train)-33))
MSE

#A bit better than regsubsets only method
mse.1

################################################