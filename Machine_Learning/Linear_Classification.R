#######3 Linear Classification ################3

### http://machinelearningmastery.com/linear-classification-in-r/

##########3 Logistic Regression #############3

Logistic Regression in RR

# load the package
library(VGAM)
# load data
data(iris)
# fit model
fit <- vglm(Species~., family=multinomial, data=iris)
# summarize the fit
summary(fit)
# make predictions
probabilities <- predict(fit, iris[,1:4], type="response")
predictions <- apply(probabilities, 1, which.max)
predictions[which(predictions=="1")] <- levels(iris$Species)[1]
predictions[which(predictions=="2")] <- levels(iris$Species)[2]
predictions[which(predictions=="3")] <- levels(iris$Species)[3]
# summarize accuracy
table(predictions, iris$Species)

############# LDA: Linear Discriminant Analysis ###########################333

# load the package
library(MASS)
data(iris)
# fit model
fit <- lda(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])$class
# summarize accuracy
table(predictions, iris$Species)


################# Partial Least Squares ###########################
# load the package
library(caret)

data(iris)
x <- iris[,1:4]
y <- iris[,5]
# fit model
fit <- plsda(x, y, probMethod="Bayes")
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])
# summarize accuracy
table(predictions, iris$Species)


