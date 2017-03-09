columnmean <- function(y){
     nc <- ncol(y) #nc = number of columns
     means <- numeric(nc)
     for(i in 1:nc) {
         means[i] <- mean(y[,i])
       }
     means
   }