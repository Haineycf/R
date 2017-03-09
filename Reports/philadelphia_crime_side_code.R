#getwd()

library(data.table)
dat <- fread("C:/Users/Haineycf/Documents/Crime_Philadelphia/crime.csv")
#summary(dat)

### make column for month and another for year
dat$Dispatch_Date <- as.Date(dat$Dispatch_Date)
dat$year <- format(as.Date(dat$Dispatch_Date, format="%m/%d/%Y"),"%Y")
dat$Month <- format(as.Date(dat$Dispatch_Date, format="%m/%d/%Y"),"%m")
dat$day <- weekdays(as.Date(dat$Dispatch_Date))

dat$crime_type[dat$Text_General_Code == "Homicide - Gross Negligence" |
                 dat$Text_General_Code =="Rape" |
                 dat$Text_General_Code =="Robbery No Firearm" |
                 dat$Text_General_Code =="Aggravated Assault Firearm" |
                 dat$Text_General_Code =="Homicide - Justifiable" |
                 dat$Text_General_Code =="Aggravated Assault No Firearm" |
                 dat$Text_General_Code =="Robbery Firearm" ] <-"Violent_crime"


dat$crime_type[dat$Text_General_Code == "Burglary Residential" |
                 dat$Text_General_Code =="Burglary Non-Residential" |
                 dat$Text_General_Code =="Thefts" |
                 dat$Text_General_Code =="Theft from Vehicle" |
                 dat$Text_General_Code =="Motor Vehicle Theft" |
                 dat$Text_General_Code =="Arson" ] <-"Property"


dat$crime_type[dat$Text_General_Code == "Receiving Stolen Property"  |
                 dat$Text_General_Code =="Embezzlement" |
                 dat$Text_General_Code =="Fraud" |
                 dat$Text_General_Code =="Weapon Violations" |
                 dat$Text_General_Code =="Forgery and Counterfeiting" |
                 dat$Text_General_Code =="Narcotic / Drug Law Violations" |
                 dat$Text_General_Code =="Offenses Against Family and Children" |
                 dat$Text_General_Code =="DRIVING UNDER THE INFLUENCE"  |
                 dat$Text_General_Code =="Vandalism/Criminal Mischief" ] <-"PartII"

dat$crime_type[dat$Text_General_Code == "Disorderly Conduct"  |
                 dat$Text_General_Code =="All Other Offenses" |
                 dat$Text_General_Code =="Vagrancy/Loitering" |
                 dat$Text_General_Code =="Other Sex Offenses (Not Commercialized)" |
                 dat$Text_General_Code =="Liquor Law Violations" |
                 dat$Text_General_Code =="Gambling Violations" |
                 dat$Text_General_Code =="Prostitution and Commercialized Vice"  |
                 dat$Text_General_Code =="Public Drunkenness" ] <-"Quality_of_Life"


crime_name <-unique(dat$Text_General_Code)
min(dat$Dispatch_Date)
max(dat$Dispatch_Date)

crime_occurance <-table(dat$Text_General_Code)
head(crime_occurance)
df <-as.data.frame(crime_occurance)
df <-df[order(df$Freq, decreasing = TRUE),]

library(pander)
pander(df)


############## check to see if a particular crime is increaseing or decreasing

crime_by_year <- aggregate(Month ~Text_General_Code + year, data = dat, FUN = length)

colnames(crime_by_year)
plot(crime_by_year$year,crime_by_year$Month, 
     col=factor(crime_by_year$Text_General_Code))

############## crime by month
crime_by_month <- aggregate(year ~Text_General_Code + Month,
                            data = dat, FUN = length)

plot(crime_by_month$Month,crime_by_month$year, 
     col=factor(crime_by_month$Text_General_Code))

############## crime by day
crime_by_day <- aggregate(year ~Text_General_Code + day,
                          data = dat, FUN = length)
crime_by_day$day_num[crime_by_day$day == "Sunday"] <-1
crime_by_day$day_num[crime_by_day$day == "Monday"] <-2
crime_by_day$day_num[crime_by_day$day == "Tuesday"] <-3
crime_by_day$day_num[crime_by_day$day == "Wednesday"] <-4
crime_by_day$day_num[crime_by_day$day == "Thursday"] <-5
crime_by_day$day_num[crime_by_day$day == "Friday"] <-6
crime_by_day$day_num[crime_by_day$day == "Saturday"] <-7


head(crime_by_day)
hist(crime_by_day$year, col = factor(crime_by_day$day))
# Add extra space to right of plot area; change clipping to figure
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

plot(crime_by_day$day_num,crime_by_day$year, 
     col=factor(crime_by_day$Text_General_Code),
     xlab = "day of week \n Sunday =1",
     ylab = "count")
# Add legend to top right, outside plot region
legend("topright",inset=c(-0.3,0),
       legend = unique(crime_by_day$Text_General_Code),
       ncol = 1,
       fill = factor(crime_by_day$Text_General_Code),
       cex = 0.28, 
       text.font = 0.5)


library(plyr)
counts <- ddply(dat, .(dat$Dc_Dist, dat$Text_General_Code), nrow)
names(counts) <- c("Dc_Dist", "Text_General_Code", "Freq")
counts<-counts[order(counts$Freq, decreasing = TRUE),]
head(counts)

year_count <- ddply(dat, .(dat$year), nrow)
year_count <- year_count[ which(year_count$`dat$year` <2016), ]
plot(year_count)

month_count <- ddply(dat, .(dat$Month), nrow)
plot(month_count)


####new crime table
counts2 <- ddply(dat, .(dat$Dc_Dist, dat$UCR_General, dat$Month, dat$year), nrow)
names(counts2) <- c("Dc_Dist", "Code","Month","year", "Freq")
counts2<-counts2[order(counts2$Freq, decreasing = TRUE),]
head(counts2)

theft <- counts2[ which(counts2$Code == 600), ]
head(theft)

fit <- lm(Freq ~., data=theft)
summary(fit)

plot(theft$Month,theft$Freq)
plot(theft$year,theft$Freq)
plot(theft$Dc_Dist,theft$Freq)

plot(as.factor(dat$Text_General_Code), horiz = TRUE)

dotchart(theft$Dc_Dist, groups = dat$year)

################map
library(ggmap)
#library(qmap)
philadelphia <-"philadelphia"
qmap(philadelphia, zoom =12)

set.seed(500)
df <- round(data.frame(
  x = jitter(rep(-95.36, 50), amount = .3),
  y = jitter(rep( 29.76, 50), amount = .3)
), digits = 2)
map <- get_googlemap('houston', markers = df, path = df, scale = 2)
ggmap(map, extent = 'device')

library(ggmap)
#library(qmap)

dat1 <-dat[ which(dat$crime_type == "Violent_crime"|
                    dat$crime_type =="Quality_of_Life"), ]
dat1 <-dat1[ which(dat1$year==2015), ]
lengths(dat1)

theme_set(theme_bw(16))
PhiladelphiaMap <- qmap("philadelphia", zoom = 12, color = "bw", legend = "bottomright")
PhiladelphiaMap +
  geom_point(aes(x = Lon, y = Lat, colour = crime_type, size = crime_type),
             data = dat1)


##########################################################
philadelphia <- get_map("philadelphia", zoom = 12)
PhiladelphiaMap <- ggmap("philadelphia", extent = "device", legend = "bottomright")
PhiladelphiaMap +
  stat_density2d(
    aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..),
    size = 2, bins = 4, data = dat1,
    geom = "polygon"
  )
overlay <- stat_density2d(
  aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..),
  bins = 4, geom = "polygon",
  data = dat1
)



