tp <- tapply(airquality, mean)
tp
lp <- lapply(airquality, mean)
lp
write.csv(lp,"./lp.csv")
sp <- sapply(airquality,mean)
sp
write.csv(sp,"./sp.csv")
vp <- vapply(airquality,mean)
vp
write.csv(vp,"./vp.csv")