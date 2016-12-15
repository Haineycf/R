###Non-Linear Classification in R

######  http://machinelearningmastery.com/non-linear-classification-in-r/


##### MDA: Mixture Discriminant Analysis ######################

# load the package
library(mda)
data(iris)
# fit model
fit <- mda(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])
# summarize accuracy
table(predictions, iris$Species)

############# QDA: Quadratic Discriminant Analysis #####################

# load the package
library(MASS)
data(iris)
# fit model
fit <- qda(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])$class
# summarize accuracy
table(predictions, iris$Species)

############# RDA: Regularized Discriminant Analysis ######################


# load the package
library(klaR)
data(iris)
# fit model
fit <- rda(Species~., data=iris, gamma=0.05, lambda=0.01)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])$class
# summarize accuracy
table(predictions, iris$Species)

################# Neural Networks #####################
# load the package
library(nnet)
data(iris)
# fit model
fit <- nnet(Species~., data=iris, size=4, decay=0.0001, maxit=500)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4], type="class")
# summarize accuracy
table(predictions, iris$Species)

#### FDA: Flexible Discriminant Analysis##################

# load the package
library(mda)
data(iris)
# fit model
fit <- fda(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])
# summarize accuracy
table(predictions, iris$Species)


#################### SVM:Support Vector Machine #################################

# load the package
library(kernlab)
data(iris)
# fit model
fit <- ksvm(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4], type="response")
# summarize accuracy
table(predictions, iris$Species)


### k-NN: k-Nearest Neighbors###############3

# load the package
library(caret)
data(iris)
# fit model
fit <- knn3(Species~., data=iris, k=5)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4], type="class")
# summarize accuracy
table(predictions, iris$Species)


###Naive Bayes############################
# load the package
library(e1071)
data(iris)
# fit model
fit <- naiveBayes(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])
# summarize accuracy
table(predictions, iris$Species)