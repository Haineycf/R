

getwd()
setwd("C:/Users/Haineycf/Documents/GitHub")

dat <- read.csv("whitewines.csv")
dat$quality <- as.factor(dat$quality)
head(dat)
str(dat)
x1 <- dat[1]
x2 <- dat[2]
x3 <- dat[3]
x4 <- dat[4]
x5 <- dat[5]
x6 <- dat[6]
x7 <- dat[7]
x8 <- dat[8]

table(dat$quality)



y <- dat[12]  # Check to see if this is the variable you want*************
y

# summarize three numeric features
summary(dat[1:12])

summary(dat)
colnames(dat)

library(psych)
pairs.panels(dat)

pairs.panels(dat[c(1:5)])

### normalize the function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
dat_n <- as.data.frame(lapply(dat[1:11], normalize))
dat_n$quality <-dat$quality
summary(dat_n[1:12])

#########################Machine Learning #############################################

# Crosstab of conservative by model
dat$y2 <-
  dat$quality %in% c("8", "9", "10", "11")


library(gmodels)
CrossTable(x = dat$volatile.acidit, y = dat$quality) ### pick your variables *******


dat_n_train <- dat_n[1:4000, ]
dat_n_test <- dat_n[4001:4898, ]

# create labels for training and test data ==> change this to caret package

dat_n_train_labels <- dat_n[1:4000, 12]
dat_n_test_labels <- dat_n[4001:4898, 12]

library(class)

#choose a k ~ (n)^(1/2) best if its prime, but odd will work
dat_n_test_pred <- knn(train = dat_n_train, test = dat_n_test,
                      cl = dat_n_train_labels, k = 3)

## Step 4: Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = dat_n_test_labels, y = dat_n_test_pred,
           prop.chisq = FALSE)


## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
dat_z <- as.data.frame(scale(dat[1:11]))

# confirm that the transformation was applied correctly
summary(dat_z$fixed.acidity)
hist(dat_z$fixed.acidity)
plot(dat_z$fixed.acidity, dat_n$fixed.acidity)
# create training and test datasets
dat_z_train <- dat_z[1:4000, ]
dat_z_test <- dat_z[4001:4898, ]

# re-classify test cases
dat_test_pred <- knn(train = dat_z_train, test = dat_z_test,
                      cl = dat_n_train_labels, k = 3)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = dat_n_test_labels, y = dat_test_pred,
           prop.chisq = FALSE)

# try several different values of k
