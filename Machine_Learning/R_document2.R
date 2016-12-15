##### Chapter 4: Classification using Naive Bayes --------------------
getwd()
setwd("C:/Users/Haineycf/Documents/GitHub")

## Example: Filtering spam SMS messages ----
## Step 2: Exploring and preparing the data ---- 

# read the sms data into the sms data frame
dat <- read.csv("whitewines.csv")

# examine the structure of the sms data
str(dat)
summary(dat)
# convert spam/ham to factor.
dat$quality <- factor(dat$quality)

# examine the type variable more carefully
str(dat$quality)
table(dat$quality)


set.seed(123)
dat_sample <- sample(4898, 4000)

str(dat_sample)

# split the data frames
dat_train <- dat[dat_sample,1:12 ]
dat_test  <- dat[-dat_sample,1:12 ]

library(C50)
dat_model <- C5.0(dat_train[-12], dat_train$quality)

# display simple facts about the tree
dat_model
# display detailed information about the tree
summary(dat_model)

## Step 4: Evaluating model performance ----
# create a factor vector of predictions on test data
dat_pred <- predict(dat_model, dat_test)

library(gmodels)
CrossTable(dat_test$quality, dat_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual quality', 'predicted quality'))




## Step 5: Improving model performance ----

## Boosting the accuracy of decision trees
# boosted decision tree with 10 trials
dat_boost10 <- C5.0(dat_train[-12], dat_train$quality,
                       trials = 10)
dat_boost10
summary(dat_boost10)

dat_boost_pred10 <- predict(dat_boost10, dat_test)
CrossTable(dat_test$quality, dat_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual quality', 'predicted quality'))

## Making some mistakes more costly than others

# create dimensions for a cost matrix
matrix_dimensions <- list(c(1,2,3,4,5,6,7), c(1,2,3,4,5,6,7))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions

# build the matrix
error_cost <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), nrow = 7, dimnames = matrix_dimensions)
error_cost

# apply the cost matrix to the tree
dat_cost <- C5.0(dat_train[-12], dat_train$quality,
                    costs = error_cost)
credit_cost_pred <- predict(dat_cost, dat_test)

CrossTable(dat_test$quality, dat_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual quality', 'predicted qualtiy'))

#### Part 2: Rule Learners -------------------

## Example: Identifying Poisonous Mushrooms ----
## Step 2: Exploring and preparing the data ---- 
# examine the structure of the data frame
str(dat)

dat <- dat[1:12]


## Step 3: Training a model on the data ----
library(RWeka)
head(dat)
# train OneR() on the data
dat_1R <- OneR(quality ~ ., data = dat)

## Step 4: Evaluating model performance ----
dat_1R
summary(dat_1R)

## Step 5: Improving model performance ----
dat_JRip <- JRip(quality ~ ., data = dat)
dat_JRip
summary(dat_JRip)

# Rule Learner Using C5.0 Decision Trees (not in text)
library(C50)
dat_c5rules <- C5.0(quality ~ free.sulfur.dioxide + alcohol, data = dat, rules = TRUE)
summary(dat_c5rules)





