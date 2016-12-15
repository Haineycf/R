columnmean_no_NA <- function(y,removeNa = TRUE){
     nc <- ncol(y) #nc = number of columns
     means <- numeric(nc)
     for(i in 1:nc) {
         means[i] <- mean(y[,i],na.rm = removeNa)
       }
     means
   }