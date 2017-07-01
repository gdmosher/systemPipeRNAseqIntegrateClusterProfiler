# install.packages("tidyverse")
library(tidyverse)

# source("https://bioconductor.org/biocLite.R")
# biocLite("systemPipeR")
biocLite("systemPipeRdata")
library(systemPipeR)
library(systemPipeRdata)

# browseVignettes("systemPipeR")

# biocLite("clusterProfiler")
library(clusterProfiler)
sessionInfo()

data(geneList, package = "DOSE")
print(geneList)
gene <- names(geneList)[abs(geneList) > 2]
print(gene)

# biocLite("org.Hs.eg.db")
library(org.Hs.eg.db)
print(keytypes(org.Hs.eg.db))

# gene.df <- bitr(gene, fromType = "ENTREZID", toType = c("ENSEMBL", "SYMBOL"), OrgDb = "org.Hs.eg.db")
gene.df <- bitr(gene, fromType = "ENTREZID", toType = c("ENSEMBL", "SYMBOL"), OrgDb = org.Hs.eg.db)
print(head(gene.df))

library(systemPipeR)
library(systemPipeRdata)
# genWorkenvir(workflow="rnaseq")
setwd("rnaseq")
