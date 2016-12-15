## Structure analysis
## http://membres-timc.imag.fr/Olivier.Francois/tutoRstructure.pdf
### file format is no column names or row names with samples on rows and markers on columns
### only bi-alleles, no three calls at a marker
install.packages(c("fields","RColorBrewer","mapplots"))
source("http://bioconductor.org/biocLite.R")
biocLite("LEA")

#run if above does not work
#install.packages("LEA_1.4.0_tar.gz", repos = NULL, type ="source")

#these files help with file converstion
source("http://membres-timc.imag.fr/Olivier.Francois/Conversion.R")
source("http://membres-timc.imag.fr/Olivier.Francois/POPSutilities.R")

library(LEA)
dt <-data("tutorial")
head(tutorial.R)

#import files format = 1 when 1 line of data and format = 2 when two lines of data
#input.file = "http://membres-timc.imag.fr/Olivier.Francois/secondary_contact.str"

input.file = "datt3.csv"
struct2geno(file = input.file, TESS = TRUE, diploid = TRUE, FORMAT = 2,
            extra.row = 0, extra.col = 0, output = "secondary_contact.geno")
#read.lfmm("datt3.csv")


#clean up plot display
par(mfrow=c(1,1))

obj.snmf = snmf("secondary_contact.geno", K = 4, alpha = 100, project = "new")
qmatrix = Q(obj.snmf, K = 4)


barplot(t(qmatrix), col = c("orange","violet","lightgreen","green"),border = NA, space = 0,
        xlab = "Individuals", ylab = "Admixture_coefficients")
xx <-qmatrix
dim(xx)
write.csv(xx,"structure4.csv")
coord = read.table("coordinates.coord")
head(coord)
pop = rep(1:60, each = 10)

K = 4
Npop = length(unique(pop))
qpop = matrix(NA, ncol = K, nrow = Npop)
coord.pop = matrix(NA, ncol = 2, nrow = Npop)
for (i in unique(pop)){
  qpop[i,] = apply(qmatrix[pop == i,], 2, mean)
  coord.pop[i,] = apply(coord[pop == i,], 2, mean)}

library(mapplots)
plot(coord, xlab = "Longitude", ylab = "Latitude", type = "n")
map(add = T, col = "grey90", fill = TRUE)
for (i in 1:Npop){
  add.pie(z = qpop[i,], x = coord.pop[i,1], y = coord.pop[i,2], labels = "",
          col = c("orange","violet","lightgreen","red","purple","blue","black","pink"))}


pop = scan("mypop.txt")

obj.snmf = snmf("secondary_contact.geno", K = 1:8, ploidy = 2, entropy = T,
                alpha = 100, project = "new")
plot(obj.snmf, col = "blue4", cex = 1.4, pch = 19)

####################################example 2######################################
####################################################################################
url = "http://membres-timc.imag.fr/Olivier.Francois/Arabidopsis/A_thaliana_chr1.geno"
download.file(url = url, destfile = "./A_thaliana_chr1.geno")
url = "http://membres-timc.imag.fr/Olivier.Francois/Arabidopsis/at_coord.coord"
download.file(url = url, destfile = "./at_coord.coord")

obj.at = snmf("./A_thaliana_chr1.geno", K = 1:10, ploidy = 1, entropy = T,
              CPU = 1, project = "new")
plot(obj.at, col = "blue4", cex = 1.4, pch = 19)

qmatrix = Q(obj.at, K =5)

asc.raster="http://membres-timc.imag.fr/Olivier.Francois/RasterMaps/Europe.asc"
grid=createGridFromAsciiRaster(asc.raster)
constraints=getConstraintsFromAsciiRaster(asc.raster, cell_value_min=0)
coord.at = read.table("at_coord.coord")

maps(matrix = qmatrix, coord.at, grid, constraints, method = "max",
     main = "Ancestry coefficients", xlab = "Longitude", ylab = "Latitude", cex = .5)
map(add = T, interior = F)


