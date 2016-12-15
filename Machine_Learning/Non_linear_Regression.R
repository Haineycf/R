############### Non-Linear Regression #################33

#### http://machinelearningmastery.com/non-linear-regression-in-r/

### MARS: Multivariate Adaptive Regression Splines ##############33

# load the package
library(earth)
# load data
data(longley)
# fit model
fit <- earth(Employed~., longley)
# summarize the fit
summary(fit)
# summarize the importance of input variables
evimp(fit)
# make predictions
predictions <- predict(fit, longley)
# summarize accuracy
rmse <- mean((longley$Employed - predictions)^2)
print(rmse)


################ Support Vector Machine ################################
# load the package
library(kernlab)
# load data
data(longley)
# fit model
fit <- ksvm(Employed~., longley)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, longley)
# summarize accuracy
rmse <- mean((longley$Employed - predictions)^2)
print(rmse)


######################## k-Nearest Neighbors ##############################
# load the package
library(caret)
# load data
data(longley)
# fit model
fit <- knnreg(longley[,1:6], longley[,7], k=3)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, longley[,1:6])
# summarize accuracy
rmse <- mean((longley$Employed - predictions)^2)
print(rmse)


################ Neural Networds #########################################

library(nnet)
# load data
data(longley)
x <- longley[,1:6]
y <- longley[,7]
# fit model
fit <- nnet(Employed~., longley, size=12, maxit=500, linout=T, decay=0.01)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, x, type="raw")
# summarize accuracy
rmse <- mean((y - predictions)^2)
print(rmse)

