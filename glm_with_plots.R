############    General Linear Model #####################
### glm(formula, family, data, weights, subset, ...)######

## this looks at if women want more children and if they use contraception

cuse <- read.table("http://data.princeton.edu/wws509/datasets/cuse.dat", 
                        header=TRUE)
colnames(cuse)
### additive model
attach(cuse)
lrfit <- glm( cbind(using, notUsing) ~ 
                    age + education + wantsMore , family = binomial)

lrfit
summary(lrfit)
plot(lrfit)
anova(lrfit)
summary(anova(lrfit))

