# download a file from a url paste and reading file


if (!file.exists("data1")) {dir.create("data1")}

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./excel1/cameras.xlsx", method = "curl")
dateDownloaded <- date()

library(xlsx)
library(rJava)
cameraData <-read.xlsx("./data1/cameras.xlsx",sheetIndex =1, header = TRUE)

head(cameraData)