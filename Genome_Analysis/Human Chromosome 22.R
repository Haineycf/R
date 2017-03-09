## this script is to analyze vcf files with snp information
# the data originally used is from the 2000 genome resequening information
## comes from http://grunwaldlab.cgrb.oregonstate.edu/sites/default/files/041277.full%20(1).pdf
### data comes from: ftp://ftp.ncbi.nih.gov/snp/organisms/human_9606_b149_GRCh38p7/genotype/


library(XML)
library(vcfR)
library(pinfsc50)
library(reshape2)
library(ggplot2)
getwd()

# Give the input file name to the function.
result <- xmlParse(file = "ftp://ftp.ncbi.nih.gov/snp/organisms/human_9606_b149_GRCh38p7/genotype/gt_chr22.xml.gz")
dat <-readLines("./Human_Ch22/gt_chr22.xml")

# Find and input VCF data.
vcf <- system.file("extdata", "pinf_sc50.vcf.gz", package = "pinfsc50")
vcf <- vcfR::read.vcfR(vcf, verbose = FALSE)

# Parse DP from the gt region.
dp <- extract.gt(vcf, element="DP", as.numeric = TRUE)

# Reorganize and render violin plots.
dpf <- melt(dp, varnames=c("Index", "Sample"), value.name = "Depth", na.rm=TRUE)
dpf <- dpf[ dpf$Depth > 0,]
p <- ggplot(dpf, aes(x=Sample, y=Depth)) + geom_violin(fill="#C0C0C0", adjust=1.0,
       scale = "count", trim=TRUE)
p <- p + theme_bw()
p <- p + ylab("Read Depth (DP)")
p <- p + theme(axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 60, hjust = 1))
p <- p + stat_summary(fun.data=mean_sdl, geom="pointrange", color="black")
p <- p + scale_y_continuous(trans=scales::log2_trans(), breaks=c(1, 10, 100, 1000))
p

# Plot as heatmap.
heatmap.bp(dp[501:1500,])


##################################################################################
##################################################################################
######################### Data Quality  ##########################################
##################################################################################
##################################################################################

# Load libraries
library(vcfR)
library(pinfsc50)

# Determine file locations
vcf_file <- system.file("extdata", "pinf_sc50.vcf.gz", package = "pinfsc50")
dna_file <- system.file("extdata", "pinf_sc50.fasta",package = "pinfsc50")
gff_file <- system.file("extdata", "pinf_sc50.gff",package = "pinfsc50")

# Read data into memory
vcf <- read.vcfR(vcf_file)
dna <- ape::read.dna(dna_file, format = "fasta")
gff <- read.table(gff_file, sep="\t", quote="")

# Create a chromR plot
chrom <- create.chromR(name="Supercontig",vcf=vcf, seq=dna,ann=gff)

# Mask for read depth and mapping quality
chrom <- masker(chrom, min_QUAL=0, min_DP=350, max_DP=650, min_MQ=59.5, max_MQ=60.5)
chrom <- proc.chromR(chrom)

# Plot.
chromoqc(chrom, dp.alpha=20)