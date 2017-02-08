#https://www.bioconductor.org/packages/devel/bioc/vignettes/VariantAnnotation/inst/doc/VariantAnnotation.pdf
# by Valerie Obenchain
#########################################################################
#########################################################################

#source("http://bioconductor.org/biocLite.R") 
#biocLite("VariantAnnotation") #install the package
library(VariantAnnotation)
library(GenomicFeatures)
#library(vcfR)
#load file

# my data, try latter
#vcf <- readVcf("Homo_sapiens_somatic.vcf.gz","TAIR10")

#package information
library(VariantAnnotation)
fl <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
vcf <- readVcf(fl, "hg19")
vcf

header(vcf)

#get the list of samples from header
samples(header(vcf))

geno(header(vcf))


head(rowRanges(vcf), 3)

ref(vcf)[1:5]

qual(vcf)[1:5]


#DNAStringSetList 
#(allows for multiple alternate alleles per variant) or a DNAStringSet
alt(vcf)[1:5]

geno(vcf)
sapply(geno(vcf), class)

#Let's take a closer look at the genotype dosage (DS) variable. The header provides the variable definition and type.
geno(header(vcf))["DS",]

DS <-geno(vcf)$DS
dim(DS)
DS[1:3,]
fivenum(DS)
length(which(DS==0))/length(DS)

hist(DS[DS != 0], breaks=seq(0, 2, by=0.05),
     main="DS non-zero values", xlab="DS")








