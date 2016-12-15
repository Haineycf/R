library(doBy)
summaryBy(mpg + wt ~ cyl + vs + gear, data = mtcars, 
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )
# produces mpg.m wt.m mpg.s wt.s for each 
# combination of the levels of cyl and vs
head(mtcars)