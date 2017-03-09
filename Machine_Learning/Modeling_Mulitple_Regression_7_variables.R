###Linear Regression
###First Run with multiple options

library(car)

#Identify the data
dat <- as.data.frame(state.x77)
summary(dat)
names <-colnames(state.x77)
y <-dat$Murder
y_name <-names[5]
x1 <-dat$Population
x1_nam <-names[1]
x2 <-dat$Income
x2_name <-names[2]
x3 <-dat$Illiteracy
x3_name <-names[3]
x4 <-dat$`Life Exp`
x4_name <-names[4]
x5 <-dat$`HS Grad`
x5_name <-names[6]
x6 <-dat$Frost
x6_name <-names[7]
x7 <-dat$Area
x7_name <-names[8]

df <- cbind.data.frame(y,x1,x2,x3,x4,x5,x6,x7)

rownames(df)<-row.names(state.x77)

##Basic Data
scatterplotMatrix(df, spread=FALSE, smoother.args=list(lty=2),
                  main="Scatter Plot Matrix")
library(corrplot)
M <- cor(df)
corrplot.mixed(M, order="hclust")

## identify highest correlation with Y
Y <-M[2:7,"y"]
sort(Y)
Y.max <-which.max(Y)
Y.min <-which.min(Y)

pos.cor <-colnames(Y.max)
Y.max[1]


###########Create Models####################################################
fit <- lm(y ~., data = df)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))



###nested models
fit1 <- lm(y ~ x1+x2+x3+x4+x5+x6, data = df)
fit2 <- lm(y ~ x3 + x4, data = df)

confint(fit) # if it contains zero, its unrelated
summary(fit)


library(effects)



par(mfrow=c(2,2))
plot(fit)
plot(fit1)
plot(fit2)
par(mfrow=c(1,1))

#####################################################################
############ Multiple Linear Regression##############################
#####################################################################

confint(fit) # if it contains zero, its unrelated
summary(fit)

## an interactive graph, click on dot to identify ones want then esc!!
qqPlot(fit, labels=row.names(df), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")


### Graphing interaction
library(effects)
fit_int <-lm(y ~ x1:x4, data=df)
# graph the average weight 3.2 and 1 sd above and below
m1 <-mean(df$x4, na.rm = FALSE)
sd1 <-mean(df$x4, na.rm = FALSE) + sd(df$x4, na.rm = FALSE)
sd2 <-mean(df$x4, na.rm = FALSE) - sd(df$x4, na.rm = FALSE)

plot(effect("x1:x4", fit_int,, list(wt=c(sd1,m1,sd2))), multiline=TRUE)
par(mfrow=c(1,1))

# simple regression diagnostics

###simple diagnostics:
##Upper Right:Normality-when normaly distributed mean should be zero, p
# a probability plot of the standardizedresiduals against the values that would be expected under normality
# if met all points should be on the straight line
## independence: 
## linearity - upper left, 
## Homoscedasticity - lower left
# you want a random band around a horiszontal line
## Residuals vs Leverage gives insight into outliers
fit_int <- lm(Murder ~ Illiteracy, data=states)
par(mfrow=c(2,2))
plot(fit_int)
par(mfrow=c(1,1))

### droping point in data !!!be cautions!!

# Assessing normality

qqPlot(fit, labels=row.names(df), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")


# Listing 8.6 - Function for plotting studentized residuals
residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

residplot(fit)

#test weather dependent variables are indepenedent
durbinWatsonTest(fit)

# Assessing linearity
library(car)
crPlots(fit)

summary(fit)
# Listing 8.7 - Assessing homoscedasticity
ncvTest(fit)
spreadLevelPlot(fit)
# if suggested power transformation is close to 1, its good

# Listing 8.8 - Global test of linear model assumptions
library(gvlma)
gvmodel <- gvlma(fit) 
summary(gvmodel)
gvmodel1 <- gvlma(fit1) 
summary(gvmodel1)

gvmodel2 <- gvlma(fit2) 
summary(gvmodel2)

vif(fit)

#outlier test
outlierTest(fit)

### identify points!!!
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}

hat.plot(fit)

cutoff <- 4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which = 4, cook.levels = cutoff)
abline(h=cutoff, lty=2, col="red")

### identify markes
avPlots(fit,ask=FALSE, id.method="identify")

influencePlot(fit, id.method="identify", main="Incluence Plot",
              sub="Circel size is proportional to Cook's distacne")


summary(powerTransform(df$y))

## this will tell you if you need to have a power to your inputs (lambda)
## if the p-value is small, don't worry, it doesn't really need it
boxTidwell(y ~ x1 + x4, data =df)

anova(fit1, fit2)
AIC(fit,fit1,fit2) #smaller number better

# Listing 8.13 - Backward stepwise selection
library(MASS)
stepAIC(fit, direction="backward")


# Listing 8.14 - All subsets regression
library(leaps)
leaps <-regsubsets(y ~ x1 + x2 + x3 + x4 + x6 + x5 + x6 + x7, data=df,
                   nbest=4)
plot(leaps, scale="adjr2") ## to read black means it's part of the equation

### these graphs, you want closeto red line furthest away?
subsets(leaps, statistic="cp",
        main="Cp Plot for All Subsets Regression")
abline(1,1,lty=2,col="red")


library(bootstrap)
shrinkage <- function(fit,k=10){
  require(bootstrap)
  
  # define functions 
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
  
  # matrix of predictors
  x <- fit$model[,2:ncol(fit$model)]
  # vector of predicted values
  y <- fit$model[,1]
  
  results <- crossval(x,y,theta.fit,theta.predict,ngroup=k)
  r2 <- cor(y, fit$fitted.values)**2 # raw R2 
  r2cv <- cor(y,results$cv.fit)**2 # cross-validated R2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2-r2cv, "\n")
}

# using it
shrinkage(fit)
fit2 <- lm(Murder~Population+Illiteracy,data=states)
shrinkage(fit2)

#  Calculating standardized regression coefficients
zdf <- as.data.frame(scale(df))
zfit <- lm(y~ x1 + x2 + x3 + x4 + x5 + x6 + x6 + x7, data=zdf)
coef(zfit)

# Listing 8.16 rlweights function for clculating relative importance of predictors
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}

# Listing 8.17 - Applying the relweights function
relweights(fit1, col="blue")

scatterplot(y ~ x3, data=df,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Title",
            xlab=x3_name,
            ylab=y_name)

scatterplot(y ~ x4, data=df,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Title",
            xlab=x4_name,
            ylab=y_name)

plot(df$x3,df$y,
     main="Title",
     xlab= x3_name,
     ylab= y_name)
lines(df$x3,fitted(fit))
