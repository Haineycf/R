### open SPSS file

library(foreign)

db = file.choose()  ## pop up window to locate file

dataset = read.spss(db, to.data.frame=TRUE)

head(dataset)

colnames(dataset)