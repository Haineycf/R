### combining the red and white wine data

getwd()
setwd("C:/Users/Haineycf/Documents/GitHub")
doc1 <- read.csv("whitewines.csv")
doc2 <- read.csv("redwines.csv")
mainmain <- "wine"
x_lab <-"xlabel"
y_lab <-"ylabel"
doc1$type <-"white"
doc2$type <-"red"

head(doc1)
colnames(doc1)
colnames(doc2)

dat <-rbind(doc1,doc2)

library(vcd)
counts <- table(dat$type)
counts


# Listing 6.1 - Simple bar plot
# vertical barplot
barplot(counts, 
        main=mainmain,
        xlab=x_lab, ylab="Frequency")
# horizontal bar plot   
barplot(counts, 
        main=mainmain, 
        xlab="Frequency", ylab=x_lab, 
        horiz=TRUE)
head(dat)
# Listing 6.3 - Bar plot for sorted mean values
dat$type<-as.factor(dat$type)

means <- aggregate(dat$quality, by=list(dat$type), FUN=mean)
means

means <- means[order(means$x),]  
means

barplot(means$x, names.arg=means$Group.1) 
title(mainmain)  

# Listing 6.4 - Fitting labels in bar plots
par(las=2)                # set label text perpendicular to the axis
par(mar=c(5,8,4,2))       # increase the y-axis margin
counts <- table(dat$quality) # get the data for the bars
counts


# produce the graph
barplot(counts, 
        main=mainmain, horiz=TRUE, cex.names=0.8,
        names.arg=c("3","4","5","6","7","8","9")
)

# Listing 6.5 - Pie charts
par(mfrow=c(2,2))                             
slices <- counts 
lbls <- c("3","4","5","6","7","8","9")

pie(slices, labels = lbls, 
    main="Simple Pie Chart")

pct <- round(slices/sum(slices)*100)                      
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart with Percentages")

library(plotrix)                                               
pie3D(slices, labels=lbls,explode=0.1,
      main="3D Pie Chart ")

mytable <- table(dat$quality)                                   
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, 
    main="Pie Chart from a dataframe\n (with sample sizes)")

# Fan plots
library(plotrix)

fan.plot(slices, labels = lbls, main="Fan Plot")

# Listing 6.6 - Histograms
# simple histogram                                                        1
hist(mtcars$mpg)

# colored histogram with specified number of bins        
hist(dat$quality, 
     breaks=12, 
     col="red", 
     xlab=x_lab, 
     main=mainmain)

# colored histogram with rug plot, frame, and specified number of bins 
hist(dat$quality, 
     freq=FALSE, 
     breaks=12, 
     col="red", 
     xlab=x_lab, 
     main=mainmain)  
rug(jitter(dat$quality)) 
lines(density(dat$quality), col="blue", lwd=2)

# histogram with superimposed normal curve (Thanks to Peter Dalgaard)  
x <- dat$quality 
h<-hist(x, 
        breaks=12, 
        col="red", 
        xlab=x_lab, 
        main=mainmain) 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
box()

# Listing 6.7 - Kernel density plot
d <- density(dat$quality) # returns the density data  
plot(d) # plots the results 


plot(d, main=mainmain)       
polygon(d, col="red", border="blue")                     
rug(dat$quality, col="brown") 

library(sm)
# create value labels 
type_f <- factor(dat$type, levels= c("red","white"),                               
                labels = c("red","white")) 

# plot densities 
sm.density.compare(dat$alcohol, dat$type, xlab=x_lab)                
title(main=mainmain)
# add legend via mouse click
colfill<-c(2:(2+length(levels(type_f)))) 
cat("Use mouse to place legend...","\n\n")
legend(locator(1), levels(type_f), fill=colfill) 


# parallel box plots
boxplot(alcohol~quality,data=dat,
        main=mainmain, 
        xlab=x_lab, 
        ylab=y_lab)

boxplot(alcohol~quality,data=dat, 
        notch=TRUE, 
        varwidth=TRUE,
        col="red",
        main=mainmain, 
        xlab=x_lab, 
        ylab=y_lab)

# Listing 6.9 - Box plots for two crossed factors
# create a factor for number of cylinders
dat$type.f <- factor(dat$quality,
                       levels=c("red","white"),
                       labels=c("red","white"))

# create a factor for transmission type
dat$quality.f <- factor(dat$quality, 
                      levels=c(3,4,5,6,7,8,9), 
                      labels=c("3","4","5","6","7","8","9"))

# Listing 6.10 - Violin plots

library(vioplot)
x1 <- dat$alcohol[dat$quality==3] 
x2 <- dat$alcohol[dat$quality==4] 
x3 <- dat$alcohol[dat$quality==5] 
x4 <- dat$alcohol[dat$quality==6] 
x5 <- dat$alcohol[dat$quality==7] 
x6 <- dat$alcohol[dat$quality==8] 
x7 <- dat$alcohol[dat$quality==9] 
x5

vioplot(x1, x2, x3, x4,x5,
        names=c("5","6","7","8","9"), 
        col="gold")
title(mainmain)

# dot chart
dotchart(dat$alcohol,labels=row.names(dat),cex=.7,
         main=mainmain, 
         xlab=x_lab)

# Listing 6.11 - Dot plot grouped, sorted, and colored
x <- dat[order(dat$alcohol),]                      
x$quality <- factor(x$quality)                                 
x$color[x$quality==3] <- "red"                              
x$color[x$quality==4] <- "blue"
x$color[x$quality==5] <- "darkgreen" 
dotchart(x$alcohol,
         labels = row.names(x),                               
         cex=.7, 
         pch=19,                                              
         groups = x$quality,                                       
         gcolor = "black",
         color = x$color,
         main = mainmain,
         xlab = x_lab)


