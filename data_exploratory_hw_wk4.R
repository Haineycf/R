#this script will produce graphs for the Coursera Exploratory Data Analysis Course, Homework week4
# work out of the documents directory

# create file if it does not exist
if (!file.exists("new_data")) {
  dir.create("new_data")
}
#Download url
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileUrl, destfile = "./new_data/cloudfront.zip", method = "curl")
dateDownloaded <- date()

#unzip and read file
ss <-unzip("./new_data/cloudfront.zip")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#head(NEI)
#head(SCC)
write.table(NEI,"./cloudfront/NEI.txt" )
write.table(SCC,"./cloudfront/SCC.csv")
nrow(NEI)
head(NEI)

#tells the number of unique items in a column
rapply(NEI,function(x)length(unique(x)))
unique(NEI$type)

#make columns factors
ppm <- transform(NEI, year = factor(year))
ppm <- transform(NEI, SCC = factor(SCC))
ppm <- transform(NEI, Pollutant = factor(Pollutant))
ppm <- transform(NEI, type = factor(type))

#sum the emision by year
byYear_sum <- tapply(NEI$Emissions, NEI$year, FUN=sum)
byYear_sum

#plot1###
mainmain = "total emissions from PM2.5 in the  United States \n from 1999 to 2008 has decreased"
barplot(byYear_sum, main = mainmain ,cex.main= 1.4)

## subset for zipcode 24510
Baltimore <- NEI[NEI$fips == "24510",]
#head(Baltimore)

## sum by year for zipcode 24510
Baltimore_byYear_sum <- tapply(Baltimore$Emissions, Baltimore$year, FUN=sum)


#plot2
mainmain = "total emissions from PM2.5 decreased \n in the Baltimore City, Maryland"
barplot(Baltimore_byYear_sum, main = mainmain, cex.main = 1.4)


### sum columns by type and year using plyr
library(plyr)
groupColumns = c("year","type")
dataColumns = c("Emissions")
res = ddply(Baltimore, groupColumns, function(x) colSums(x[dataColumns]))
class(res$year)
res <- transform(res, year = factor(year))

# plot 3 ******
library(ggplot2)
gg <- qplot(year, Emissions, data = res, facets = . ~ type)
gg + ggtitle("four sources for emissions from 1999-2008 for Baltimore City") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

## find coal and combustion names
coal <-SCC[grep("Coal", SCC$SCC.Level.Four), ]
cc <-coal[grep("Combustion", coal$SCC.Level.One),]

toMatch <- cc$SCC

#obtain data that has coal and combustion from scc file and sum
coalComb <- NEI[NEI$SCC %in% toMatch,]
byYear_sumCoalCom <- tapply(coalComb$Emissions, coalComb$year, FUN=sum)
byYear_sumCoalCom

###plot 4
coalTitle = " emissions from coal combustion-related \n sources in the US from 1999-2008"
barplot(byYear_sumCoalCom,  main = coalTitle)


## find emission from vehicle and sum
mv_list <- SCC[grep("Vehicle", SCC$Short.Name), ]
toMatch2 <- mv_list$SCC
mv_NEI <- Baltimore[Baltimore$SCC %in% toMatch2,]
Baltimore_mv <- tapply(mv_NEI$Emissions, mv_NEI$year, FUN=sum)


#plot 5 ******
baltTitle = "Emissions from motor vehicle sources changed \n from 1999-2008 in Baltimore City"
barplot(Baltimore_mv, main = baltTitle)


## find LA data and compareto Baltimore
LA <- NEI[NEI$fips == "06037",]
mv_NEI_LA <- LA[LA$SCC %in% toMatch2,]
LA_mv <- tapply(mv_NEI_LA$Emissions, mv_NEI_LA$year, FUN=sum)
#barplot(LA_mv)
la_ba <- rbind(LA_mv, Baltimore_mv)
colnames(la_ba)[1] <- c("1999")
#plot(la_ba, type = "s")

#plot 6 ***
mainmain = "Comparison of Emissions From Motor Vehicle Sources \n In Baltimore, MD   Los Angeles County, CA"
par(mfrow = c(1, 2))
  barplot(LA_mv, main = "Los Angeles", ylim=c(0,1500))
  barplot(Baltimore_mv, main = "Baltimore", ylim=c(0,1500))
  mtext(mainmain, outer = TRUE, cex = 1.0)

