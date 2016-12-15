# download a file from a url paste and reading file

if (!file.exists("data")) {
  dir.create("data")
}

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./data/cameras.csv", method = "curl")
dateDownloaded <- date()


#option 1
#cameraData <-read.table("./data/cameras.csv")


#option 2
#cameraData <-read.table("./data/cameras.csv", sep = ",", header = TRUE)

#option 3
cameraData <-read.csv(".data/cameras.csv")

head(cameraData)