###Linear Regression
###First Run with multiple options

dat <- as.data.frame(state.x77)
head(dat)
y <-dat$Murder
x1 <-dat$Population
x2 <-dat$Income
x3 <-dat$Illiteracy
x4 <-dat$`Life Exp`
x5 <-dat$`HS Grad`
x6 <-dat$Frost
x7 <-dat$Area
df <- cbind.data.frame(y,x1,x2,x3,x4,x5,x6,x7)
head(df)
fit1 <-lm(y ~.,data=df)
sfit <-summary(fit1)
sfit
fit1$coefficients




fit <- lm(weight ~ height, data=women)
summary(fit)
women$weight
fitted(fit)
residuals(fit)
plot(women$height,women$weight,
     main="Women Age 30-39", 
     xlab="Height (in inches)", 
     ylab="Weight (in pounds)")
# add the line of best fit
abline(fit)

fit2 <- lm(weight ~ height + I(height^2), data=women)
summary(fit2)
plot(women$height,women$weight,
     main="Women Age 30-39",
     xlab="Height (in inches)",
     ylab="Weight (in lbs)")
lines(women$height,fitted(fit2))


# Enhanced scatterplot for women data
library(car)
scatterplot(weight ~ height, data=women,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Women Age 30-39",
            xlab="Height (inches)",
            ylab="Weight (lbs.)")

fit3 <- lm(weight ~ height + I(height^2) + I(height^3), data = women)
plot(women$height,women$weight,
     main="Women Age 30-39",
     xlab="Height (in inches)",
     ylab="Weight (in lbs)")
lines(women$height,fitted(fit3))


#####################################################################
############ Multiple Linear Regression##############################
#####################################################################

# Listing 8.3 - Examining bivariate relationships
states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                     "Illiteracy", "Income", "Frost")])
cor(states)
library(car)
scatterplotMatrix(states, spread=FALSE, smoother.args=list(lty=2),
                  main="Scatter Plot Matrix")


# Listing 8.4 - Multiple linear regression
states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fit)
plot(states$Illiteracy,states$Murder)
plot(states$Income,states$Murder)

confint(fit) # if it contains zero, its unrelated



# Listing 8.5 - Mutiple linear regression with a significant interaction term
fit <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(fit)
plot(fit)
library(effects)
# graph the average weight 3.2 and 1 sd above and below
m1 <-mean(mtcars$wt, na.rm = FALSE)
sd1 <-mean(mtcars$wt, na.rm = FALSE) + sd(mtcars$wt, na.rm = FALSE)
sd2 <-mean(mtcars$wt, na.rm = FALSE) - sd(mtcars$wt, na.rm = FALSE)

plot(effect("hp:wt", fit,, list(wt=c(sd1,m1,sd2))), multiline=TRUE)

# simple regression diagnostics
fit <- lm(weight ~ height, data=women)
par(mfrow=c(2,2))
###simple diagnostics:
##Upper Right:Normality-when normaly distributed mean should be zero, p
# a probability plot of the standardizedresiduals against the values that would be expected under normality
# if met all points should be on the straight line
## independence: 
## linearity - upper left, 
## Homoscedasticity - lower left
# you want a random band around a horiszontal line
## Residuals vs Leverage gives insight into outliers
fit2 <- lm(weight ~ height + I(height^2), data = women)
par(mfrow=c(2,2))
plot(fit2)
### droping point in data !!!be cautions!!
newfit <-lm(weight ~ height + I(height^2), data=women[-c(13,15),])
par(opar)
par(mfrow=c(2,2))
plot(newfit)

par(opar)


# basic regression diagnostics for states data
opar <- par(no.readonly=TRUE)
fit <- lm(weight ~ height, data=women)
par(mfrow=c(2,2))
plot(fit)
par(opar)

fit2 <- lm(weight ~ height + I(height^2), data=women)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
plot(fit2)
par(opar)

par(mfrow=c(1,1))

# Assessing normality
library(car)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)

## an interactive graph, click on dot to identify ones want then esc!!

qqPlot(fit, labels=row.names(states), id.method="identify",
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


# Listing 8.7 - Assessing homoscedasticity
library(car)
ncvTest(fit)
spreadLevelPlot(fit)
# if suggested power transformation is close to 1, its good

# Listing 8.8 - Global test of linear model assumptions
library(gvlma)
gvmodel <- gvlma(fit) 
summary(gvmodel)

gvmodel2 <- gvlma(fit2) 
summary(gvmodel2)






