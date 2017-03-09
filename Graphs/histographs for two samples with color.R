set.seed(42)
p1 <- hist(rnorm(500,4))                     # centered at 4
p2 <- hist(rnorm(500,6))                     # centered at 6
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,10))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,10), add=T)  # second