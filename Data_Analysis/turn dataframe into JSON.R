#Turn data frames to JSON
library(jsonlite)
myjson <-toJSON(iris, pretty = TRUE)
cat(myjson)

# to convert back
#iris2 <-fromJSON(myjson)
#head(iris2)