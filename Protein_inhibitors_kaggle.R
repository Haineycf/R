### new kaggle search for protein inhibors for cancer


### started 12/19/2016

dat <- read.csv("./protein_inhibitors/train_test/cdk2_test.csv")
head(dat)

library(randomForest)
dim(dat)