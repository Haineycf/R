#### looping through columns in a dataframe
#### rember if its not linear, it will not be very accurate
#### so check to make sure its linear response, otherwise, go to glm



dat_y<-(dat[,c(2:1130)])
dat_x<-(dat[,c(1)])
models <- list()
#
for(i in names(dat_y)){
  y <- dat_y[i]
  model[[i]] = lm( y~dat_x )
}


for(i in names(dat_y)){
  x <- dat_x[i]
  model[[i]] = lm(dat_y~x )
}




