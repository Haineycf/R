#### Distributions###########3

norm_vec <- rnorm(n=10, mean=5, sd=2)
exp_vec <- rexp(n=100, rate=3)
pois_vec <- rpois(n=50, lambda=6)
unif_vec <- runif(n=20, min=1, max=9)
bin_vec <- rbinom(n=20, size=1000, prob=0.7)

#Suppose you have a vector v of numbers. To randomly sample, say, 25 of the numbers, use the sample function:
v_sample <-sample(v, size=25, replace=FALSE)


plot(norm_vec, norm_vec, main="Scatterplot", xlab="Age", ylab="Number of Nodes", pch=20)

hist(norm_vec, main="Histogram", xlab="Year", ylab="Count")

boxplot(norm_vec, main="Boxplot", xlab="Age")










