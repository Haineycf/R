library(ggplot2)
p <- ggplot(data = diamonds)
p <- p + aes(x= depth, y = price)
p <- p + geom_point()
p <- p + geom_density2d()

p