#   Differential expression analysis with limma

### To load a library in R you will need to first install it, e.g.:
### > source("http://bioconductor.org/biocLite.R")
### > biocLite("affy")
### > biocLite("limma")
### You can then get information about various functions in these  
### packages, e.g. > limmaUsersGuide()
library(affy)
library(limma)

# If you'd like to replicate code in Pevsner p 505-510 with same dataset:
#  download and unpack Down_Syndrome-GE.zip, setwd below to your own path
#  then run these 2 lines instead of the succeeding 2 lines: 
#setwd("/Users/Pat/Documents/R/CS_640/labs/microarray/Down_Syndrome-GE")
#phenoData <- read.AnnotatedDataFrame("pheno.txt", header = T, sep="\t")

# For our assignment run code in Pevsner p 505-510 on the files here:
#  download and unpack GSE2443_cel.zip, setwd below to your own path
# run these 2 lines:
setwd("/Users/Pat/Documents/R/CS_640/labs/microarray/GSE2443_cel")
phenoData <- read.AnnotatedDataFrame("pheno2443.txt", header = T, sep="\t")

phenoData
dim(phenoData)
summary(phenoData)

MyBioInfData <- ReadAffy()
MyBioInfData  # note number of samples 

# compare with info for GSE2443:
# https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE2443

# Note that in Pevsner the following lines are in error:
# hist(eset)
# boxplot(eset) 
# MAplot(dilution, pairs=T, plot.method="smoothScatter")
# boxplot(eset, ylab="log2 intensities", col=colors)

# Should be:
hist(exprs(eset))
boxplot(exprs(eset))
par(mfrow=c(5,5))
MAplot(MyBioInfData)
boxplot(exprs(eset), ylab="log2 intensities", col=colors)
