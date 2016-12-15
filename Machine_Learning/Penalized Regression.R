###Penalized Regression in R

####### from http://machinelearningmastery.com/penalized-regression-in-r/

######## Ridge Regression


library(glmnet)
# load data
data(longley)
x <- as.matrix(longley[,1:6])
y <- as.matrix(longley[,7])
# fit model
fit <- glmnet(x, y, family="gaussian", alpha=0, lambda=0.001)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, x, type="link")
# summarize accuracy
rmse <- mean((y - predictions)^2)
print(rmse)



################ LASSO Least Absolute Shrinkage and Selection Operator#########3

# load the package
library(lars)
# load data
data(longley)
x <- as.matrix(longley[,1:6])
y <- as.matrix(longley[,7])
# fit model
fit <- lars(x, y, type="lasso")
# summarize the fit
summary(fit)
# select a step with a minimum error
best_step <- fit$df[which.min(fit$RSS)]
# make predictions
predictions <- predict(fit, x, s=best_step, type="fit")$fit
# summarize accuracy
rmse <- mean((y - predictions)^2)
print(rmse)

####################  Elastic Net ####################################


library(glmnet)
# load data
data(longley)
x <- as.matrix(longley[,1:6])
y <- as.matrix(longley[,7])
# fit model
fit <- glmnet(x, y, family="gaussian", alpha=0.5, lambda=0.001)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, x, type="link")
# summarize accuracy
rmse <- mean((y - predictions)^2)
print(rmse)

