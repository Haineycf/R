#source("http://bioconductor.org/biocLite.R") 
#biocLite("VariantAnnotation") #install the package
#library(VariantAnnotation)
#library(GenomicFeatures)
library(vcfR)
#load file
vcf <- readVcf("Homo_sapiens_somatic.vcf.gz","TAIR10")

# Find the files.
vcf_file <- system.file("extdata", "pinf_sc50.vcf.gz", package = "pinfsc50")
dna_file <- system.file("extdata", "pinf_sc50.fasta", package = "pinfsc50")
gff_file <- system.file("extdata", "pinf_sc50.gff", package = "pinfsc50")


# Input the files.
vcf <- read.vcfR(vcf_file, verbose = FALSE)
dna <- ape::read.dna(dna_file, format = "fasta")
gff <- read.table(gff_file, sep="\t", quote="")

# Create a chromR object.
chrom <- create.chromR(name="Supercontig", vcf=vcf, seq=dna, ann=gff, verbose=TRUE)

chromoqc(chrom, dp.alpha = 22)

#sets window size to 1kb
chrom <- proc.chromR(chrom, verbose=FALSE, win.size=1e4)
chromoqc(chrom, dp.alpha = 22)

#sets window size to 1kb
chrom <- proc.chromR(chrom, verbose=FALSE, win.size=1e3)

head(chrom)

plot(chrom)










