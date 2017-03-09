# count the number of times a variable is seen .N is the equation and x is the columnhead it looks at
library(data.table)
set.seed(123);
DT <- data.table(x=sample(letters[1:3], 1E5, TRUE))
DT[,.N, by=x]

#order by a variable x starting w/a
library(data.table)
DTkey <-data.table(x=rep(c("a","b","c"), each =100), y=rnorm(300))
setkey(DTkey, x)
DTkey['a']


#Join
DT1 <- data.table(x=c('a','a','b', 'dt1'), y = 1:4)
DT2 <- data.table(x=c('a','b','dt2'),z=5:7)
setkey (DT1, x); setkey(DT2,x)
merge(DT1,DT2)

#Fast reading
big_df <- data.frame(x=rnorm(1E6), y=rnorm(1e6))
file <-tempfile()
write.table(big_df, file=file, row.names=FALSE, col.names = TRUE, sep="\t", quote=FALSE)
system.time(fread(file))
#or
system.time(read.table(file, header= TRUE, sep="\t"))