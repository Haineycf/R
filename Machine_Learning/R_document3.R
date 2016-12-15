setwd("C:/Users/Haineycf/Documents/GitHub")

# read the sms data into the sms data frame
dat <- read.csv("whitewines.csv")
head(dat)
str(dat)

cor1 <-round(cor(dat), digits = 2)

fix(cor1)

cor(dat[c("quality","alcohol","density")])
library(psych)
pairs.panels(dat[c("quality","alcohol","density")])

# confirming the regression line using the lm function 1 variable
model <- summary(lm(quality ~ ., data = dat))
model

model$r.squared
model1 <- summary(lm(quality ~ fixed.acidity + volatile.acidity + residual.sugar + free.sulfur.dioxide + density + pH + sulphates + alcohol, data = dat ))
model2 <- summary(lm(quality ~ alcohol, data = dat))
model3 <- summary(lm(quality ~ density, data = dat))
model4 <- summary(lm(quality ~ residual.sugar, data = dat))

r_square <- c(model$r.squared, model1$r.squared, model2$r.squared,model3$r.squared,model4$r.squared)
r_square
library(lattice)
wireframe(quality ~ density + alcohol, data=dat)

set.seed(1234)
#subset data for validation
library(caret)
# Create a building data set and validation set
inBuild <- createDataPartition(y=dat$quality, p=0.7, list=FALSE)
validation <- dat[-inBuild,]; buildData <- dat[inBuild,]
inTrain <- createDataPartition(y=buildData$quality, p=0.7, list=FALSE)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,]
train1 <- training

library(rpart)
m.rpart <- rpart(quality ~ ., data = training)

# get basic information about the tree
m.rpart

summary(m.rpart)

# use the rpart.plot package to create a visualization
library(rpart.plot)

# a basic decision tree diagram
rpart.plot(m.rpart, digits = 3)

# a few adjustments to the diagram
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

## Step 4: Evaluate model performance ----

# generate predictions for the testing dataset
p.rpart <- predict(m.rpart, testing)

# compare the distribution of predicted values vs. actual values
summary(p.rpart)
summary(testing$quality)

# compare the correlation
cor(p.rpart, testing$quality)

# function to calculate the mean absolute error
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

# mean absolute error between predicted and actual values
MAE(p.rpart, testing$quality)

# mean absolute error between actual values and mean value
mae_am <-mean(training$quality) # result = 5.87
MAE(mae_am, testing$quality)

## Step 5: Improving model performance ----
# train a M5' Model Tree
library(RWeka)
m.m5p <- M5P(quality ~ ., data = training)

# display the tree
m.m5p

# get a summary of the model's performance
summary(m.m5p)

# generate predictions for the model
p.m5p <- predict(m.m5p, testing)

# summary statistics about the predictions
summary(p.m5p)

# correlation between the predicted and true values
cor(p.m5p, testing$quality)

# mean absolute error of predicted and true values
# (uses a custom function defined above)
MAE(testing$quality, p.m5p)

##### Chapter 7: Neural Networks and Support Vector Machines -------------------

getwd()
setwd("C:/Users/Haineycf/Documents/GitHub/Machine Learning with R, Second Edition_Code/Chapter 07")

##### Part 1: Neural Networks -------------------
## Example: Modeling the Strength of Concrete  ----

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
dat_norm <- as.data.frame(lapply(dat, normalize))

# confirm that the range is now between zero and one
summary(dat_norm$alcohol)

# compared to the original minimum and maximum
summary(dat$alcohol)

# create training and test data
# use training and testing

## Step 3: Training a model on the data ----
# train the neuralnet model
library(neuralnet)
head(training)
# simple ANN with only a single hidden neuron
set.seed(12345) # to guarantee repeatable results
dat_model <- neuralnet(formula = quality ~fixed.acidity + 
          volatile.acidity + citric.acid + residual.sugar+ chlorides + 
            free.sulfur.dioxide +  total.sulfur.dioxide + density +pH + sulphates + alcohol,
          data = training)
# visualize the network topology
plot(dat_model)

## Step 4: Evaluating model performance ----
# obtain model results
model_results <- compute(dat_model, testing)
# obtain predicted strength values
predicted_strength <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_strength, concrete_testing$quality)

## Step 5: Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
set.seed(12345) # to guarantee repeatable results
dat_model2 <- neuralnet(quality ~ density + alcohol +
                               volatile.acidity + free.sulfur.dioxide,
                             data = training, hidden = 5)

# plot the network
plot(dat_model2)

# evaluate the results as we did before
model_results2 <- compute(dat_model2, testing[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, testing$quality)


