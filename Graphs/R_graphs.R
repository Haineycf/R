# http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization

library(plot3D)

x <- dat$alcohol
y <- dat$density
z <- dat$quality
head(dat)
dat1 <- dat[,8:12]
dat1$sulphates <-NULL
head(dat1)
dat2 <- dat1
dat2$pH <-NULL
head(dat2)
mainmain <- "Wine characteristics"
x_lab <- "alcohol"
y_lab <-"density"
z_lab <-"quality"

scatter3D(x, y, z, colvar = NULL, col = "blue",
          pch = 19, cex = 0.5)

# full box
scatter3D(x, y, z, bty = "f", colkey = FALSE, main ="bty= 'f'")


# back panels and grid lines are visible
scatter3D(x, y, z, bty = "b2", colkey = FALSE, main ="bty= 'b2'" )

# grey background with white grid lines
scatter3D(x, y, z, bty = "g", colkey = FALSE, main ="bty= 'g'")
# User defined
scatter3D(x, y, z, pch = 18, bty = "u", colkey = FALSE, 
          main ="bty= 'u'", col.panel ="steelblue", expand =0.4, 
          col.grid = "darkblue")

# gg.col: ggplot2 like color
scatter3D(x, y, z, bty = "g", pch = 18, col = gg.col(100))

# ramp.col: custom palettes
scatter3D(x, y, z, bty = "g", pch = 18,
          col = ramp.col(c("blue", "yellow", "red")) )


scatter3D(x, y, z, bty = "g", pch = 18, 
          col.var = as.integer(dat$quality), 
          col = c("#1B9E77", "#D95F02", "#7570B3"),
          pch = 18, ticktype = "detailed",
          colkey = list(at = c(2, 3, 4), side = 1, 
                        addlines = TRUE, length = 0.5, width = 0.5,
                        labels = c("density", "alcohol", "quality")) )


# Bottom colkey
scatter3D(x, y, z, bty = "g",
          colkey = list(side = 1, length = 0.5))


scatter3D(x, y, z, theta = 15, phi = 20)

scatter3D(x, y, z, phi = 0, bty ="g")

scatter3D(x, y, z, pch = 18,  theta = 20, phi = 20,
          main = mainmain, xlab = x_lab,
          ylab =y_lab, zlab = z_lab)

scatter3D(x, y, z, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed")

##############################################################################
# Create a scatter plot
scatter3D(x, y, z, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed")
# Add another point (black color)
scatter3D(x = 7, y = 1, z = 5, add = TRUE, colkey = FALSE, 
          pch = 18, cex = 3, col = "black")

# Create a scatter plot
scatter3D(x, y, z, phi = 0, bty = "g", pch = 20, cex = 0.5)
# Add text
text3D(x, y, z,  labels = rownames(dat),
       add = TRUE, colkey = FALSE, cex = 0.5)

# type ="l" for lines only
scatter3D(x, y, z, phi = 0, bty = "g", type = "l", 
          ticktype = "detailed", lwd = 4)

# type ="b" for both points and lines
scatter3D(x, y, z, phi = 0, bty = "g", type = "b", 
          ticktype = "detailed", pch = 20, 
          cex = c(0.5, 1, 1.5))


# type ="h" for vertical lines
scatter3D(x, y, z, phi = 0, bty = "g",  type = "h", 
          ticktype = "detailed", pch = 19, cex = 0.5)


# Confidence interval # this is faked
CI <- list(z = matrix(nrow = length(x),
                      data = rep(0.1, 2*length(x))))
head(CI$z)

# 3D Scatter plot with CI
scatter3D(x, y, z, phi = 0, bty = "g", col = gg.col(100), 
          pch = 18, CI = CI)

# Add small dots on basal plane and on the depth plane
scatter3D_fancy <- function(x, y, z,..., colvar = z)
{
  panelfirst <- function(pmat) {
    XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 2, add = TRUE, colkey = FALSE)
    
    XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 2, add = TRUE, colkey = FALSE)
  }
  scatter3D(x, y, z, ..., colvar = colvar, panel.first=panelfirst,
            colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75)) 
}

scatter3D_fancy(x, y, z, pch = 16,
                ticktype = "detailed", theta = 15, d = 2,
                main = mainmain,  clab = c(z_lab) )

# Compute the linear regression (z = ax + by + d)
fit <- lm(z ~ x + y)
# predict values on regular xy grid
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- predict(fit)
# scatter plot with regression plane
scatter3D(x, y, z, pch = 18, cex = 2, 
          theta = 20, phi = 20, ticktype = "detailed",
          xlab = x_lab, ylab = y_lab, zlab = z_lab,  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = mainmain)
colnames(dat)
data(USArrests)
with(dat, text3D(alcohol, density, quality, 
                       labels = rownames(dat), colvar = pH, 
                       col = gg.col(100), theta = 60, phi = 20,
                       xlab = x_lab, ylab = y_lab, zlab = z_lab, 
                       main = mainmain, cex = 0.6, 
                       bty = "g", ticktype = "detailed", d = 2,
                       clab = c(z_lab), adj = 0.5, font = 2))


# Plot texts
with(dat, text3D(alcohol, density, quality, 
                       labels = rownames(dat), colvar = pH, 
                       col = gg.col(100), theta = 60, phi = 20,
                       xlab = x_lab, ylab = y_lab, zlab = z_lab, 
                       main = mainmain, cex = 0.6, 
                       bty = "g", ticktype = "detailed", d = 2,
                       clab = c(z_lab), adj = 0.5, font = 2))
# Add points
with(dat, scatter3D(alcohol, density, quality, - 1, 
                          colvar = pH, col = gg.col(100), 
                          type = "h", pch = ".", add = TRUE))


# Zoom near origin: choose suitable ranges
plotdev(xlim = c(min(x), max(x)), ylim = c(min(y), max(y)), 
        zlim = c(min(z),max(z)))



#  hist3D and ribbon3D with greyish background, rotated, rescaled,...
data("VADeaths")
head(VADeaths)
class(VADeaths)
dat1m <-as.matrix(dat1)
class(dat1m)
hist3D(z = dat1m, scale = FALSE, expand = 0.01, bty = "g", phi = 20,
       col = "#0072B2", border = "black", shade = 0.2, ltheta = 90,
       space = 0.3, ticktype = "detailed", d = 2)

hist3D (x = 1:length(x), y = 1:length(y), z = dat1m,
        bty = "g", phi = 20,  theta = -60,
        xlab = x_lab, ylab = y_lab, zlab = z_lab, main = mainmain,
        col = "#0072B2", border = "black", shade = 0.8,
        ticktype = "detailed", space = 0.15, d = 2, cex.axis = 1e-9)
# Use text3D to label x axis
text3D(x = 1:5, y = rep(0.5, 5), z = rep(3, 5),
       labels = rownames(dat1m),
       add = TRUE, adj = 0)
# Use text3D to label y axis
text3D(x = rep(1, 4),   y = 1:4, z = rep(0, 4),
       labels  = colnames(dat1m),
       add = TRUE, adj = 1)

##############################################################################
hist3D_fancy<- function(x, y, break.func = c("Sturges", "scott", "FD"), breaks = NULL,
                        colvar = NULL, col="white", clab=NULL, phi = 5, theta = 25, ...){
  
  # Compute the number of classes for a histogram
  break.func <- break.func [1]
  if(is.null(breaks)){
    x.breaks <- switch(break.func,
                       Sturges = nclass.Sturges(x),
                       scott = nclass.scott(x),
                       FD = nclass.FD(x))
    y.breaks <- switch(break.func,
                       Sturges = nclass.Sturges(y),
                       scott = nclass.scott(y),
                       FD = nclass.FD(y))
  } else x.breaks <- y.breaks <- breaks
  
  # Cut x and y variables in bins for counting
  x.bin <- seq(min(x), max(x), length.out = x.breaks)
  y.bin <- seq(min(y), max(y), length.out = y.breaks)
  xy <- table(cut(x, x.bin), cut(y, y.bin))
  z <- xy
  
  xmid <- 0.5*(x.bin[-1] + x.bin[-length(x.bin)])
  ymid <- 0.5*(y.bin[-1] + y.bin[-length(y.bin)])
  
  oldmar <- par("mar")
  par (mar = par("mar") + c(0, 0, 0, 2))
  hist3D(x = xmid, y = ymid, z = xy, ...,
         zlim = c(-max(z)/2, max(z)), zlab = "counts", bty= "g", 
         phi = phi, theta = theta,
         shade = 0.2, col = col, border = "black",
         d = 1, ticktype = "detailed")
  
  scatter3D(x, y,
            z = rep(-max(z)/2, length.out = length(x)),
            colvar = colvar, col = gg.col(100),
            add = TRUE, pch = 18, clab = clab,
            colkey = list(length = 0.5, width = 0.5,
                          dist = 0.05, cex.axis = 0.8, cex.clab = 0.8)
  )
  par(mar = oldmar)
}

# Create his3D using plot3D
hist3D_fancy(x, y, colvar=z)
# Make the rgl version
library("plot3Drgl")
plotrgl()