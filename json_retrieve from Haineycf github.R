library(jsonlite)
jsonData <- fromJSON("https://api.github.com/users/Haineycf")
names(jsonData)
names(jsonData$login)
