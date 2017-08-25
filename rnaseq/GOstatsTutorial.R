## Code in this file is from:
## http://manuals.bioinformatics.ucr.edu/home/R_BioCondManual#TOC-Gene-Ontologies
##

## Code from man phyper
## (2017-0813 22:30 paste code to calculate hypergeometric distribution in R)
m <- 10; n <- 7; k <- 8
x <- 0:(k+1)
rbind(phyper(x, m, n, k), dhyper(x, m, n, k))
all(phyper(x, m, n, k) == cumsum(dhyper(x, m, n, k)))  # FALSE
## but error is very small:
signif(phyper(x, m, n, k) - cumsum(dhyper(x, m, n, k)), digits = 3)


## This code was included to debug X11 issues on 2017-0806
## from same manual, see Bar Plots - Base Graphics
##
plot(cars)
y <- as.data.frame(matrix(runif(30), ncol=3, dimnames=list(letters[1:10], LETTERS[1:3]))) # Generates a sample data set.
barplot(as.matrix(y[1:4,]), ylim=c(0,max(y[1:4,])+0.1), beside=T) 
   # Generates a bar diagram for the first four rows in y. The barplot() function expects as input format a matrix, and 
   # it uses by default the column titles for labeling the bars.
text(labels=round(as.vector(as.matrix(y[1:4,])),2), x=seq(1.5, 13, by=1)+sort(rep(c(0,1,2), 4)), y=as.vector(as.matrix(y[1:4,]))+0.02) 
   # Adds corresponding values on top of each bar.
ysub <- as.matrix(y[1:4,]); myN <- length(ysub[,1])
mycol1 <- gray(1:(myN+1)/(myN+1))[-(myN+1)]
mycol2 <- sample(colors(),myN); barplot(ysub, beside=T, ylim=c(0,max(ysub)*1.2), col=mycol2, main="Bar Plot", sub="data: ysub")
legend("topright", legend=row.names(ysub), cex=1.3, bty="n", pch=15, pt.cex=1.8, col=mycol2, ncol=myN) 
   # Generates a bar plot with a legend for the first four rows of the input matrix 'y'. To plot the diagram in black and 
   # white, set the 'col' argument to 'col=col1". The argument 'ncol' controls the number of columns that are used for printing the legend.
par(mar=c(10.1, 4.1, 4.1, 2.1)); par(xpd=TRUE); 
barplot(ysub, beside=T, ylim=c(0,max(ysub)*1.2), col=mycol2, main="Bar Plot"); legend(x=4.5, y=-0.3, legend=row.names(ysub), cex=1.3, bty="n", pch=15, pt.cex=1.8, col=mycol2, ncol=myN) 
   # Same as above, but places the legend below the bar plot. The arguments 'x' and 'y' control the placement of the legend 
   # and the 'mar' argument specifies the margin sizes around the plotting area in this order: c(bottom, left, top, right).
bar <- barplot(x <- abs(rnorm(10,2,1)), names.arg = letters[1:10], col="red", ylim=c(0,5))
stdev <- x/5; arrows(bar, x, bar, x + stdev, length=0.15, angle = 90)
arrows(bar, x, bar, x + -(stdev), length=0.15, angle = 90) 
   # Creates bar plot with standard deviation bars. The example in the 'barplot' documentation provides a very useful outline 
   # of this function.
source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/mortgage.R") 
   # Imports a function that plots a loan amortization table as bar plot.
###=========================================================================================

## from same manual, see ggplot2
## (A) Sample Set: the following transforms the iris data set into a ggplot2-friendly format  
library(tidyverse)
## (A) Sample Set: the following transforms the iris data set into a ggplot2-friendly format  
iris_mean <- aggregate(iris[,1:4], by=list(Species=iris$Species), FUN=mean) # Calculates the mean values for the aggregates given by the Species column in the iris data set.
iris_sd <- aggregate(iris[,1:4], by=list(Species=iris$Species), FUN=sd) # Calculates the standard deviations for the aggregates given by the Species column in the iris data set.
convertDF <- function(df=df, mycolnames=c("Species", "Values", "Samples")) { myfactor <- rep(colnames(df)[-1], each=length(df[,1])); mydata <- as.vector(as.matrix(df[,-1])); df <- data.frame(df[,1], mydata, myfactor); colnames(df) <- mycolnames; return(df) } # Defines function to convert data frames into ggplot2-friendly format.
df_mean <- convertDF(iris_mean, mycolnames=c("Species", "Values", "Samples")) # Converts iris_mean.
df_sd <- convertDF(iris_sd, mycolnames=c("Species", "Values", "Samples")) # Converts iris_sd.
limits <- aes(ymax = df_mean[,2] + df_sd[,2], ymin=df_mean[,2] - df_sd[,2]) # Define standard deviation limits.

## (B) Bar plots of data stored in df_mean
ggplot(df_mean, aes(Samples, Values, fill = Species)) + geom_bar(position="dodge") # Plots bar sets defined by 'Species' column next to each other.
ggplot(df_mean, aes(Samples, Values, fill = Species)) + geom_bar(position="dodge") + coord_flip() + opts(axis.text.y=theme_text(angle=0, hjust=1)) # Plots bars and labels sideways.
ggplot(df_mean, aes(Samples, Values, fill = Species)) + geom_bar(position="stack") # Plots same data set as stacked bars.
ggplot(df_mean, aes(Samples, Values)) + geom_bar(aes(fill = Species)) + facet_wrap(~Species, ncol=1) # Plots data sets below each other.
ggplot(df_mean, aes(Samples, Values, fill = Species)) + geom_bar(position="dodge") + geom_errorbar(limits, position="dodge") # Generates the same plot as before, but with error bars.

# (C) Customizing colors
library(RColorBrewer); display.brewer.all() # Select a color scheme and pass it on to 'scale_*' arguments.
ggplot(df_mean, aes(Samples, Values, fill=Species, color=Species)) + geom_bar(position="dodge") + geom_errorbar(limits, position="dodge") + scale_fill_brewer(pal="Greys") + scale_color_brewer(pal = "Greys") # Generates the same plot as before, but with grey color scheme. 
ggplot(df_mean, aes(Samples, Values, fill=Species, color=Species)) + geom_bar(position="dodge") + geom_errorbar(limits, position="dodge") + scale_fill_manual(values=c("red", "green3", "blue")) + scale_color_manual(values=c("red", "green3", "blue")) # Uses custom colors passed on as vectors.
###=========================================================================================
## Chromosome maps (from same manual)
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("hgu95av2.db")
library(annotate); library(geneplotter); library(hgu95av2.db); newChrom <- buildChromLocation("hgu95av2"); newChrom; cPlot(newChrom) # This displays all genes on the chromosomes of an organisms. Genes encoded by the antisense strands are represented by lines below the chromosomes.
data(sample.ExpressionSet); myeset <- sample.ExpressionSet; cColor(featureNames(sample.ExpressionSet), "red", newChrom) # This highlights in the above plot a set of genes of interest in red color (e.g. expressed genes of an experiment).
cPlot(newChrom,c("1","2"), fg="yellow", scale="relative"); cColor(featureNames(myeset), "red", newChrom) # Plots just a specific set of chromosomes.
plot(cars) # X11 not working?
###=========================================================================================
###################################
## Basic usage of GO information ##
###################################
library(GOstats); library(GO.db); library(ath1121501.db); library(annotate) # Loads the required libraries.
goann <- as.list(GOTERM) # Retrieves full set of GO annotations.
str(goann)
head(goann)
zz <- eapply(GOTERM, function(x) x@Ontology); table(unlist(zz)) # Calculates the number of annotations for each ontology category.
unlist(zz)
#?GOTERM # To find out, how to access the different GO components.
GOTERM$"GO:0003700";
GOMFPARENTS$"GO:0003700";
GOMFCHILDREN$"GO:0003700" 
   # Shows how to print out the GO annotations for one entry and how to retrieve its direct parents and children.
GOMFANCESTOR$"GO:0003700";
GOMFOFFSPRING$"GO:0003700" # Prints out complete lineages of parents and children for a GO ID.
goterms <- unlist(eapply(GOTERM, function(x) x@Term));
goterms[grep("molecular_function", goterms)] 
goterms[grep("mitochondrion inheritance", goterms)] 
   # Retrieves all GO terms and prints out only those matching a search string given in the grep function. The same can 
   # be done for the definition field with 'x@Definition'. A set of GO IDs can be provided as well: goterms[GOMFANCESTOR$"GO:0005507"]
str(goterms)
head(goterms)
go_df <- data.frame(GOID=unlist(eapply(GOTERM, function(x) x@GOID)), Term=unlist(eapply(GOTERM, function(x) x@Term)), Ont=unlist(eapply(GOTERM, function(x) x@Ontology))) 
   # Generates data frame of the commonly used GO components: GOID, GO Term and Ontology Type.
str(go_df)
head(go_df)
ls()
ls()
affyGO <- AnnotationDbi::eapply(ath1121501GO, getOntology, "MF");
str(affyGO)
head(affyGO)
str(ath1121501GO)
head(ath1121501GO)
ath1121501GO
table(BiocGenerics::sapply(affyGO, length)) 
   # Retrieves MF GO terms for all probe IDs of a chosen Affy chip and calculates how many probes have multiple GO terms 
   # associated. Use "BP" and "CC" arguments to retrieve BP/CC GO terms.
affyGOdf <- data.frame(unlist(affyGO));
affyGOdf <- data.frame(AffyID=row.names(affyGOdf), GOID=affyGOdf[,1]);
affyGOdf <- merge(affyGOdf, go_df, by.x="GOID", by.y="GOID", all.x=T) 
str(affyGOdf)
head(affyGOdf) # nice
# Converts above MF list object into a data frame. The AffyID occurence counts are appended to AffyIDs. The last step 
   # merges the two data frames: 'affyGOdf' and 'go_df'.
unique(lookUp("GO:0004713", "ath1121501", "GO2ALLPROBES")) # Retrieves all Affy IDs that are associated with a GO node.
z <- affyGO[c("254759_at", "260744_at")];
as.list(GOTERM)[z[[1]]] 
   # Retrieves GO IDs for set of Affy IDs and then the corresponding GO term for first Affy ID.
a <- data.frame(unlist(z));
a <- data.frame(ID=row.names(a), a);
b <- data.frame(goterms[as.vector(unlist(z))]);
b <- data.frame(ID=row.names(b), b);
merge(b, a, by.x = "ID", by.y="unlist.z.") 
   # Merges Affy ID, GO ID and GO annotation information.
affyEv <- eapply(ath1121501GO, getEvidence);
table(unlist(affyEv, use.names = FALSE)) 
   # Provides evidence code information for each gene and summarizes the result.
test1 <- eapply(ath1121501GO, dropECode, c("IEA", "NR"));
table(unlist(sapply(test1, getEvidence), use.names = FALSE)) 
   # This example shows how one can remove certain evidence codes (e.g. IEA, IEP) from the analysis.

##############################################
## GO term enrichment analysis with GOstats ##
##############################################
## Example of how to test a sample set of probe set keys for over-representation of GO terms using a hypergeometric distribution
## test with the function hyperGTest(). For more information, read the GOstatsHyperG manual.
library(ath1121501.db); library(ath1121501cdf)
affySample <- c("266592_at", "266703_at", "266199_at", "246949_at", "267370_at", "267115_s_at", "266489_at", "259845_at", "266295_at", "262632_at")
geneSample <- as.vector(unlist(mget(affySample, ath1121501ACCNUM, ifnotfound=NA)))
affyUniverse <- ls(ath1121501cdf)
geneUniverse <- as.vector(unlist(mget(affyUniverse, ath1121501ACCNUM, ifnotfound=NA)))
params <- new("GOHyperGParams", geneIds = geneSample, universeGeneIds = geneUniverse, annotation="ath1121501", ontology = "MF", pvalueCutoff = 0.5, conditional = FALSE, testDirection = "over")
hgOver <- hyperGTest(params)
summary(hgOver)
htmlReport(hgOver, file = "MyhyperGresult.html") # nice
##==========================================================================================
## GOHyperGALL (same manual)
#############################################################################
## (1.1) Import all required functions with the following source() command ## 
#############################################################################
source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/GOHyperGAll.txt")

###########################################################
## (2.1) Using gene-to-GO mappings from geneontology.org ##
###########################################################
readGOorg(myfile = "gene_association.tair", colno = c(5,11,9), org = "Arabidopsis"); gene2GOlist(rootUK=T) 
   # Download the required annotation table from geneontology.org and unzip it. Then point the 'readGOorg()' function to 
   # this file name. The two functions generate 4 data frames with the assigned gene-to-GO mappings and 3 lists containing 
   # the gene-to-GO-OFFSPRING associations. When the processes are completed, 6 files will be saved in your working directory! 
   # They can be reloaded in future R sessions with the 'load' command below. If the argument 'rootUK' is set to TRUE, then 
   # the root nodes are treated as terminal nodes to account for the new assignment of unknown genes to the root nodes.

###############################################
## (2.2) Using gene-to-GO mappings from Bioc ##
############################################### 
## Note: users should execute either step (2.1) or (2.2), but not both!
sampleDFgene2GO(lib="ath1121501.db"); gene2GOlist(rootUK=T) 
   # Similar as above, but the gene-to-GO mappings are obtained from BioC. The generated 4 sample data frame and 3 
   # list objects can be reloaded in future R sessions with the 'load' command below.

######################################################################
## (2.3) Obtain AffyID-to-GeneID mappings when working with AffyIDs ##
######################################################################
AffyID2GeneID(map = "ftp://ftp.arabidopsis.org/home/tair/Microarrays/Affymetrix/affy_ATH1_array_elements-2010-12-20.txt") 
   # When working with AffyIDs, this function creates a AffyID-to-GeneID mapping data frame using by default the TAIR 
   # mappings for the Arabidopsis ATH1 chip. To use the function for the mappings of other chips, one needs to create the 
   # corresponding decoding data frame 'affy2locusDF'.

############################################################
## (3.1) Reloading required data objects from local files ##
############################################################
loadData(); load(file="MF_node_affy_list"); load(file="BP_node_affy_list"); load(file="CC_node_affy_list") 
   # This step makes future sessions much faster, since it allows to skip the previous data generation steps (2.1-2.3). 
   # A sample data set is available here: ArabSampleGOHyperGAll.zip (Jan 2010).

##########################################
## (3.2) Obtain a sample set of GeneIDs ##
##########################################
test_sample <- unique(as.vector(GO_MF_DF[1:40,2])) # When working with GeneIDs.
test_sample <- AffyID2GeneID(affyIDs=affy_sample, probe2gene=1) 
   # When working with AffyIDs, one can use the function 'AffyID2GeneID' to obtain for a set of AffyIDs their corresponding 
   # GeneIDs from the data frame 'affy2locusDF' (see above). For probe sets that match several loci, only the first locus 
   # ID will be used if the argument 'probe2gene' is set to 1. To demo the function, one can use the following sample 
affy_sample <- c("266592_at", "266703_at", "266199_at", "246949_at", "267370_at", "267115_s_at", "266489_at", "259845_at", "266295_at", "262632_at") # AffyID sample 

##########################################################################
## (4.1) Perform phyper test, goSlim subsetting and plotting of results ##
##########################################################################
GOHyperGAll_result <- GOHyperGAll(gocat="MF", sample=test_sample, Nannot=2); GOHyperGAll_result[1:10,-8] 
   # The function 'GOHyperGAll()' performs the phyper test against all nodes in the GO network. It returns raw and 
   # adjusted p-values. The Bonferroni correction is used as p-values adjustment method according to Boyle et al, 2004 (online). 
   # The argument 'Nannot' defines the minimum number of direct annotations per GO node from the sample set to determine 
   # the number of tested hypotheses for the p-value adjustment. The argument 'gocat' can be assigned the values "MF", "BP" 
   # and "CC". Omitting the '-8' delimiter will provide the sample keys matching at every GO node.
subset <- GOHyperGAll_Subset(GOHyperGAll_result, sample=test_sample, type="goSlim"); subset[,-8] 
   # The function 'GOHyperGAll_Subset()' subsets the GOHyperGAll results by assigned GO nodes or custom goSlim categories. 
   # The argument 'type' can be assigned the values "goSlim" or "assigned". The optional argument 'myslimv' can be used to 
   # provide a custom goSlim vector. Omitting the '-8' delimiter will show the sample keys matching at every GO node.
pie(subset[subset$SampleMatch>0 ,3], labels=as.vector(subset[subset$SampleMatch>0 ,1]), main=unique(as.vector(subset[subset$SampleMatch>0, 7]))) # Plots pie chart of subsetted results.

##############################################################
## (4.2) Reduce GO Term Redundancy in 'GOHyperGAll_results' ##
##############################################################
simplifyDF <- GOHyperGAll_Simplify(GOHyperGAll_result, gocat="MF", cutoff=0.001, correct=T) 
   # The result data frame 'GOHyperGAll_result' often contains several connected GO terms with significant scores which 
   # can complicate the interpretation of large sample sets. To reduce the redundancy, the function 'GOHyperGAll_Simplify' 
   # subsets the data frame 'GOHyperGAll_result' by a user specified p-value cutoff and removes from it all GO nodes with 
   # overlapping children sets (OFFSPRING), while the best scoring nodes remain in the data frame.
data.frame(GOHyperGAll_result[GOHyperGAll_result[,1] %in% simplifyDF[,1], -8], GO_OL_Match=simplifyDF[,2]) 
   # This command returns the redundancy reduced data set. The column 'GO_OL_Match' provides the number of accessions that 
   # match the connected nodes.

################################################
## (4.3) Batch Analysis of Many Gene Clusters ##
################################################
BatchResult <- GOCluster_Report(CL_DF=CL_DF, method="all", id_type="gene", CLSZ=10, cutoff=0.001, gocats=c("MF", "BP", "CC"), recordSpecGO=c("GO:0003674", "GO:0008150", "GO:0005575")) 
   # The function 'GOCluster_Report' performs the three GO analyses in batch mode: 'GOHyperGAll', 'GOHyperGAll_Subset' 
   # or 'GOHyperGAll_Simplify'. It processes many groups of genes (e.g. gene expression clusters) and returns the results 
   # conveniently organized in a single data frame. The gene sets need to be provided in a data frame of the format 
   # specified at the end of the GOHyperGAll script. CLSZ: minimum cluster size to consider; method: "all", "slim" or 
   # "simplify"; gocat: "MF", "BP" or "CC"; cutoff: adjusted p-value cutoff; recordSpecGO: argument to include one specific 
   # GOID in each of the 3 ontologies, e.g: recordSpecGO=c("GO:0003674", "GO:0008150", "GO:0005575").
#==========================================================================================
