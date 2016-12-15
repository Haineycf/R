##########3https://ocw.mit.edu/courses/sloan-school-of-management/15-097-prediction-machine-learning-and-statistics-spring-2012/lecture-notes/MIT15_097S12_lec02.pdf


############## Apriori ################################
getwd()
library(arules)
dataset <- read.csv("./Github/mushroom.csv", header = TRUE)

mushroom_rules <- apriori(as.matrix(dataset), parameter = list(supp = 0.8, conf = 0.9))

summary(mushroom_rules)

inspect(mushroom_rules)
##################33 GLM #####################333

glm_mushrooms <- glm(dataset$class.p ~ ., data = dataset, family = binomial(link = "logit"))
summary(glm_mushrooms)

y <- dataset$class.e
x1 <- dataset$cap.shape.b
x2 <- dataset$cap.shape.f
glm_mod <-glm(y ??? x1+x2, family=binomial(link="logit"), data=as.data.frame(cbind(y,x1,x2)))

#################### k- Means #######################

kmeans_model <- kmeans(x=y, centers=2)
kmeans_model

###################### k-Nearest Neighbor Clustering ########################3

library(class)

knn_model <- knn(train=X_train, test=X_test, cl=as.factor(labels), k=K)


##########################Naives Bayes #################333

library(e1071)

nB_model <- naiveBayes(y ??? x1 + x2, data=as.data.frame(cbind(y,x1,x2)))
nB_model

###########################Decission Tree ###################################################

library(rpart)
cart_model <- rpart(y ??? x1 + x2, data=as.data.frame(cbind(y,x1,x2)), method="class")
summary(cart_model)

# use the rpart.plot package to create a visualization
library(rpart.plot)

# a basic decision tree diagram
rpart.plot(cart_model, digits = 3)


####################### AdaBoost ####################3

library(rpart)
library(ada)
boost_model <- ada(x=x1, y=labels)


#########################support vector Machine##############################333

svm_model <- svm(x=X, y=as.factor(labels), kernel ="radial", cost=C)
summary(svm_model)

