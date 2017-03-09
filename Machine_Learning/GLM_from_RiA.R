#----------------------------------------------#
# R in Action (2nd ed): Chapter 13             #
# Generalized linear models                    #
# requires packages AER, robust, gcc           #
# install.packages(c("AER", "robust", "gcc"))  #
#----------------------------------------------#


## Logistic Regression

# get summary statistics
data(Affairs, package="AER")
summary(Affairs)
table(Affairs$affairs)

# create binary outcome variable
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair, 
                           levels=c(0,1),
                           labels=c("No","Yes"))
table(Affairs$ynaffair)

# fit full model
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + 
                  religiousness + education + occupation +rating,
                data=Affairs,family=binomial())
summary(fit.full)

# fit reduced model
fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness + 
                     rating, data=Affairs, family=binomial())
summary(fit.reduced)

# compare models
anova(fit.reduced, fit.full, test="Chisq")

# interpret coefficients
coef(fit.reduced)
exp(coef(fit.reduced))

# calculate probability of extramariatal affair by marital ratings
testdata <- data.frame(rating = c(1, 2, 3, 4, 5),
                       age = mean(Affairs$age),
                       yearsmarried = mean(Affairs$yearsmarried),
                       religiousness = mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

# calculate probabilites of extramariatal affair by age
testdata <- data.frame(rating = mean(Affairs$rating),
                       age = seq(17, 57, 10), 
                       yearsmarried = mean(Affairs$yearsmarried),
                       religiousness = mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

# evaluate overdispersion
fit <- glm(ynaffair ~ age + yearsmarried + religiousness +
             rating, family = binomial(), data = Affairs)
fit.od <- glm(ynaffair ~ age + yearsmarried + religiousness +
                rating, family = quasibinomial(), data = Affairs)

deviance(fit.reduced)/df.residual(fit.reduced) ## if >1, evidence of overdispersion
## if close to 1, don't worry about it.

### if the p-value is >0.05, it is no significan,saying dispersion not a problem
pchisq(summary(fit.od)$dispersion * fit$df.residual,  
       fit$df.residual, lower = F)

### other glm to try, good for outliers and such
library(robust)

gfit <-glmRob(ynaffair ~ age + yearsmarried + religiousness +
                rating, family = binomial(), data = Affairs)
summary(gfit)

### multinomial logistic regression
library(mlogit)
mlogit()

###ordinal logistic regression, poor/good/excellent
library(rms)
lrm()

## Poisson Regression

# look at dataset
data(breslow.dat, package="robust")
names(breslow.dat)
summary(breslow.dat[c(6, 7, 8, 10)])

# plot distribution of post-treatment seizure counts
opar <- par(no.readonly=TRUE)
par(mfrow=c(1, 2))
attach(breslow.dat)
hist(sumY, breaks=20, xlab="Seizure Count", 
     main="Distribution of Seizures")
boxplot(sumY ~ Trt, xlab="Treatment", main="Group Comparisons")
par(opar)

# fit regression
fit <- glm(sumY ~ Base + Age + Trt, data=breslow.dat, family=poisson())
summary(fit)

# interpret model parameters
coef(fit)       ## interprit with log
exp(coef(fit))  ## interprit without log

# evaluate overdispersion
deviance(fit)/df.residual(fit) ## if larger than 1 need to run another type of model
library(qcc)
qcc.overdispersion.test(breslow.dat$sumY, type="poisson")## if have p <0.05, have overdispersion


# fit model with quasipoisson, because the poisson has too much dispersion
fit.od <- glm(sumY ~ Base + Age + Trt, data=breslow.dat,
              family=quasipoisson())
summary(fit.od)

