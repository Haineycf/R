############## Decision Trees #####################################
#####http://machinelearningmastery.com/non-linear-regression-in-r-with-decision-trees/


### CART: Classification and Regression Trees#############3333

# load the package
library(rpart)
# load data
data(longley)
# fit model
fit <- rpart(Employed~., data=longley, control=rpart.control(minsplit=5))
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, longley[,1:6])
# summarize accuracy
rmse <- mean((longley$Employed - predictions)^2)
print(rmse)


###############3 Conditional Decision Trees

ditional Decision Trees in RR

# load the package
library(party)
# load data
data(longley)
# fit model
fit <- ctree(Employed~., data=longley, controls=ctree_control(minsplit=2,minbucket=2,testtype="Univariate"))
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, longley[,1:6])
# summarize accuracy
rmse <- mean((longley$Employed - predictions)^2)
print(rmse)


###################Conditional Decission Trees

# load the package
library(party)
# load data
data(longley)
# fit model
fit <- ctree(Employed~., data=longley, controls=ctree_control(minsplit=2,minbucket=2,testtype="Univariate"))
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, longley[,1:6])
# summarize accuracy
rmse <- mean((longley$Employed - predictions)^2)
print(rmse)


####################################MOdel Trees####################33

# load the package
library(RWeka)
# load data
data(longley)
# fit model
fit <- M5P(Employed~., data=longley)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, longley[,1:6])
# summarize accuracy
rmse <- mean((longley$Employed - predictions)^2)
print(rmse)


####################### Rule System ####################################

library(RWeka)
# load data
data(longley)
# fit model
fit <- M5Rules(Employed~., data=longley)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, longley[,1:6])
# summarize accuracy
rmse <- mean((longley$Employed - predictions)^2)
print(rmse)

################ Bagging Cart: Bootstrapped Aggregation

library(ipred)
# load data
data(longley)
# fit model
fit <- bagging(Employed~., data=longley, control=rpart.control(minsplit=5))
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, longley[,1:6])
# summarize accuracy
rmse <- mean((longley$Employed - predictions)^2)
print(rmse)

################### Random Forester ########################333
# load the package
library(randomForest)
# load data
data(longley)
# fit model
fit <- randomForest(Employed~., data=longley)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, longley[,1:6])
# summarize accuracy
rmse <- mean((longley$Employed - predictions)^2)
print(rmse)


############# GBM Gradient Boosted Machine #####################
# load the package
library(gbm)
# load data
data(longley)
# fit model
fit <- gbm(Employed~., data=longley, distribution="gaussian")
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, longley)
# summarize accuracy
rmse <- mean((longley$Employed - predictions)^2)
print(rmse)


######################## Cubist ####################################
library(Cubist)
# load data
data(longley)
# fit model
fit <- cubist(longley[,1:6], longley[,7])
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, longley[,1:6])
# summarize accuracy
rmse <- mean((longley$Employed - predictions)^2)
print(rmse)

