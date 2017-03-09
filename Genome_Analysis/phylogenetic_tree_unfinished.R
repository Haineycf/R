



install CRAN Task View for phylogenetics 
install.packages('ctv')
library('ctv') 
install.views('Phylogenetics')
update.views('Phylogenetics')

## load ape
library(ape)

## simulate a phylogeny
set.seed(1234)
tree <- rtree(n = 20)
plot(tree, edge.width = 2)







library(phangorn)
dat <-read.csv("snp_data.csv")

file="snp_data.csv"
dat = read.phyDat(file)
dm = dist.ml(dat, "F81")
tree = NJ(dm)
# as alternative for a starting tree:
tree <- pratchet(dat) # parsimony tree
tree <- nnls.phylo(tree, dm) # need edge weights
# 1. alternative: quick and dirty: GTR + G
fitStart = pml(tree, dat, k=4)
fit = optim.pml(fitStart, model="GTR", optGamma=TRUE, rearrangement="stochastic")
# 2. alternative: preper with modelTest
mt <- modelTest(dat, tree=tree, multicore=TRUE)
mt[order(mt$AICc),]
# choose best model from the table according to AICc
bestmodel <- mt$Model[which.min(mt$AICc)]
env = attr(mt, "env")
fitStart = eval(get("GTR+G+I", env), env)
# or let R search the table
fitStart = eval(get(bestmodel, env), env)
# equivalent to: fitStart = eval(get("GTR+G+I", env), env)
fit = optim.pml(fitStart, rearrangement = "stochastic",
                optGamma=TRUE, optInv=TRUE, model="GTR")
bs = bootstrap.pml(fit, bs=100, optNni=TRUE, multicore=TRUE)
