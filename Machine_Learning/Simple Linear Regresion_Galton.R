### lets use different types of data to determine the relationship



# load necessary packages/install if needed
library(ggplot2)
library(UsingR)

#first up is linear relationship
set.seed(1234)

x <- seq(100:200)
y <- 2*x
plot(x,y)
dat <- cbind.data.frame(y,x)

g_fit<-lm(y ~ x, data=dat)
summary(g_fit)
par(mfrow=c(2,2))
plot(g_fit)

# calculate residual
e <- resid(g_fit)
# calculate predicted values
yhat <- predict(g_fit)
# create 1 x 2 panel plot
par(mfrow=c(1, 2))
# plot residuals on regression line

library(ggplot2)
library(grid)
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# plot data + regression
g <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
  geom_smooth(method = "lm", colour = "black") +
  geom_point(size = 3, colour = "black", alpha = 0.4) +
  geom_point(size = 2, colour = "red", alpha = 0.4)+
  ggtitle("Residual on Regression Line")
# plot residuals
f <- ggplot(data.frame(x = x, y = resid(lm(y ~ x))), aes(x = x, y = y))+
  geom_hline(yintercept = 0, size = 2)+
  geom_point(size = 3, colour = "black", alpha = 0.4)+
  geom_point(size = 2, colour = "red", alpha = 0.4)+
  xlab("X") + ylab("Residual")+ ggtitle("Residual vs X")
multiplot(g, f, cols = 2)

######################################## put a little sin on it
y  <- 2*x + sin(x) 

par(mfrow=c(1, 1))
plot(x,y)

g_fit<-lm(y ~ x, data=dat)
summary(g_fit)
par(mfrow=c(2,2))
plot(g_fit)


#########################################################
z <- runif(101, 0.1, 7.5)
y <-x^2 + sin(x) + 10*z^2

par(mfrow=c(1, 1))
plot(x,y)

g_fit<-lm(y ~ x, data=dat)
summary(g_fit)
par(mfrow=c(2,2))
plot(g_fit)

x <- runif(100, 0.0001, 100)
z <- runif(100, 0.0001, 100)
y <- x + z
dat<- cbind.data.frame(x,y,z)
plot(x,y)
fit1 <-lm(y~x, data = dat)
summary(fit1)

fit2 <-lm(y~., data=dat)
summary(fit2)
par(mfrow=c(2,2))
plot(fit1)
plot(fit2)

fn <- function(x) x * 100/max(x, na.rm = TRUE)
df_scale <- data.frame(lapply(dat, fn))
fit3 <-lm(y~., data=df_scale)
summary(fit3)
plot(fit3)


x <- runif(100, 0.0001, 100)
z <- runif(100, 0.0001, 100)
y <- x * z
dat<- cbind.data.frame(x,y,z)

cor(df_scale)
fit4 <-lm(y ~.,data=dat)
summary(fit4)
par(mfrow=c(1,3))
plot(x,y)
plot(z,y)
plot(x,z)
par(mfrow=c(2,2))
plot(fit4)