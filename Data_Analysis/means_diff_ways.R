DT <-fread("idaho_housing.csv")

a.0 <- mean(DT[DT$SEX==1,]$pwgtp15); 
a.1 <-mean(DT[DT$SEX==2,]$pwgtp15)
b <- tapply(DT$pwgtp15,DT$SEX,mean)
c <- mean(DT$pwgtp15,by=DT$SEX)
d <- DT[,mean(pwgtp15),by=SEX]
e <-sapply(split(DT$pwgtp15,DT$SEX),mean)
a.0
a.1
b
c
d
e
