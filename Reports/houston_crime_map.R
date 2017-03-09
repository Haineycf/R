## from https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
library(ggmap)
str(crime)


 # find a reasonable spatial extent
 qmap('houston', zoom = 13)

 # only violent crimes
 violent_crimes <- subset(crime,
                     offense != "auto theft" & offense != "theft" & offense != "burglary")
head(violent_crimes)
# order violent crimes
 violent_crimes$offense <- factor(violent_crimes$offense,
                         levels = c("robbery", "aggravated assault", "rape", "murder"))

 # restrict to downtown
 violent_crimes <- subset(violent_crimes,
               + -95.39681 <= lon & lon <= -95.34188 &
                 + 29.73631 <= lat & lat <= 29.78400)
lengths(violent_crimes)
head(violent_crimes)
tb <-as.data.frame(tb)
tb <-subset(tb, Freq >0)
 unique(violent_crimes$lat)
 theme_set(theme_bw(16))
 HoustonMap <- qmap("houston", zoom = 14, color = "bw", legend = "topleft")
 HoustonMap +
   geom_point(aes(x = lon, y = lat, colour = offense, size = offense),
              data = violent_crimes)
 HoustonMap +
   stat_bin2d(
     aes(x = lon, y = lat, colour = offense, fill = offense),
     size = .5, bins = 30, alpha = 1/2,
     data = violent_crimes
   )


 houston <- get_map("houston", zoom = 14)
 HoustonMap <- ggmap("houston", extent = "device", legend = "topleft")
 HoustonMap +
   stat_density2d(
     aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
     size = 2, bins = 4, data = violent_crimes,
     geom = "polygon"
   )
 overlay <- stat_density2d(
   aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
   bins = 4, geom = "polygon",
   data = violent_crimes
 )
library(grid)
library(ggplot2)
  HoustonMap + overlay + inset(
   grob = ggplotGrob(ggplot() + overlay + theme_inset()),
   xmin = -95.35836, xmax = Inf, ymin = -Inf, ymax = 29.75062
 )

 houston <- get_map(location = "houston", zoom = 14, color = "bw",
                    source = "osm")
 HoustonMap <- ggmap(houston, base_layer = ggplot(aes(x = lon, y = lat),
                                                  data = violent_crimes))
 HoustonMap +
   stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                  bins = 5, geom = "polygon",
                  data = violent_crimes) +
   scale_fill_gradient(low = "black", high = "red") +
   facet_wrap(~ day)

 

#######################
 
houston <- get_map('houston', zoom = 14)
HoustonMap <- ggmap("houston",
                    extent = "device", legend = "topleft")
HoustonMap +
         stat_density2d(
                    aes(x = lon, y = lat, fill = ..level..,
                    alpha = ..level..),
                    size = 2, bins = 4, data = violent_crimes,
                    geom = "polygon")
                    

#################################################
library(ggplot2)
library(ggmap)
library(dplyr)

# only violent crimes
violent_crimes <- filter(crime, 
                         offense != "auto theft", offense != "theft", offense != "burglary"
)

# rank violent crimes
violent_crimes$offense <- factor(
  violent_crimes$offense,
  levels = c("robbery", "aggravated assault", "rape", "murder")
)

# restrict to downtown
violent_crimes <- filter(violent_crimes,
                         -95.39681 <= lon & lon <= -95.34188,
                         29.73631 <= lat & lat <=  29.78400
)
robberies <- violent_crimes %>% filter(offense == "robbery")
head(violent_crimes)
qmplot(lon, lat, data = violent_crimes, geom = "blank",
       zoom = 15, maptype = "toner-background", 
       darken = .7, legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Robbery\nPropensity", low = "white", mid = "yellow", high = "red", midpoint = 650)