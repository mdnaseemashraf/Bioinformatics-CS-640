# Read the data in "count_matrix_sub_2.5M.txt" 
# and run a DESeq2 experiment comparing the transcriptome 
#   of untreated MCF-7 tumor cells to those treated with estrogen. 
# There are 7 replicates for each of the conditions (untreated, treated) 

### RUN next two lines once 
#source("http://bioconductor.org/biocLite.R")
#biocLite("DESeq2")
library('DESeq2')

#setwd("C:/Users/Pat/Documents/R/RNASeq")
setwd("C:/Users/Naseem Ashraf/Desktop 1/Fall 16/Bioinformatics/DESeq2 Assignment")

countTable <- read.table("count_matrix_sub.txt", header=TRUE, sep="\t", row.names=1)

# These substrings of column names indicate the 7 replicates for control sample: 
ControlCistrackID <- c("2012.562","2012.563","2012.564","2012.565","2012.566","2012.568","2012.569")

# These substrings of column names indicate the 7 replicates for sample treated with estrogen:
E2CistrackID <- c("2012.570","2012.571","2012.572","2012.574","2012.575","2012.576","2012.577" )

# These substrings of column names indicate the 3 different depth of reads 
#   for each of the replicates of both untreated and treated:
##readDepth <- c("2.5M","10M","30M") # bases sequenced per sample (actually down-sampled from 30M)

numRep <- 7   # 7 repetitions each of untreated, treated

# Produce  dataset for read depth ("2.5M","10M","30M")
# The dataset should include 7 replicates of untreated and 7 replicates oftreated.
for(readDepth in c("2.5M","10M","30M"))
{
      makeDataSet<- function ( data, controlIDs, E2IDs, readDepth){
        controlReplicates <- paste( "X", controlIDs, ".subsamp.", sep="")
        controlReplicates_readDepth <- paste(controlReplicates, readDepth, sep="")
        E2Replicates <- paste( "X", E2IDs, ".subsamp.", sep="")
        E2Replicates_readDepth <- paste(E2Replicates, readDepth, sep="")  
        dataSet <- data[, c(controlReplicates_readDepth, E2Replicates_readDepth)]
        return(dataSet)
     }
      
      
      dataset <- makeDataSet(countTable, ControlCistrackID, E2CistrackID, readDepth)
    
      sampleCondition<- c(rep("untreated",numRep), rep("treated",numRep) )
      sampleTable<-data.frame(sampleName=colnames(dataset), condition=sampleCondition)
      
      countData = dataset
    
      colData = sampleTable
      design = ~ condition
      ##?DESeqDataSetFromMatrix
      deseq2dataset <- DESeqDataSetFromMatrix(countData, colData, design)
      ##?? deseq2dataset <- DESeqDataSetFromMatrix(countData = dataset, colData = sampleTable, design = ~ condition)
      
      colData(deseq2dataset)
      colData(deseq2dataset)$condition<-factor(colData(deseq2dataset)$condition, levels=c("untreated","treated"))
      dds<-DESeq(deseq2dataset)     # *** stats for differential expression done here
      res<-results(dds)  
      head(res)
      res<-res[order(res$padj),]
      head(res)
      resSig <- subset(res, res$padj < 0.05 ) # significant fold changes
      resSig
      genesUp = subset(resSig, resSig$log2FoldChange >=2 )    # up-regulated genes at log2FoldChange >= 2
      genesUp <-genesUp[order(genesUp$log2FoldChange),]
      genesUp
      genesDown = subset(resSig, resSig$log2FoldChange <= -1.5 )  # down-regulated genes at log2FoldChange <= -1.5
      genesDown <-genesDown[order(genesDown$log2FoldChange),]
      genesDown
      
      
      #pvalue is the Wald test p-value: condition treated vs untreated
      #padj is BH (Benjamini-Hochberg) adjusted p-values <- adjusted to control the false discovery rate
      
      if(readDepth=="2.5M")
      {
        hist( res$pvalue, breaks=50, col="grey" )
        dev.copy(png,"MA-2.5.png")
        dev.off()
        
        dev.copy(png,"Hist-padjacent2-2.5.png")
        dev.copy(png,"MvA-2.5.png")
        dev.off()
      }
      if(readDepth=="10M")
      {
        hist( res$pvalue, breaks=50, col="grey" )
        dev.copy(png,"Hist-pvalue-10.png")
        dev.off()
        
        hist( res$padj, breaks=50, col="grey" )
        dev.copy(png,"Hist-padjacent2-10.png")
        dev.off()
      } 
      else
      {
        hist( res$pvalue, breaks=50, col="grey" )
        dev.copy(png,"Hist-pvalue-30.png")
        dev.off()
        
        hist( res$padj, breaks=50, col="grey" )
        dev.copy(png,"Hist-padjacent2-30.png")
        dev.off()
      }
      
      # MA plot is application of a Bland-Altman plot for visual representation of two channel gene expression data
      #  which has been transformed onto the M (log ratios) and A (mean average) scale.
      # M vs. A plots of each pair (untreated, treated) is produced.
      ##plotMA(dds,ylim=c(-2,2),main= paste("DESeq2 ", "2.5M "), alpha=.05 )  # default threshold (ie: alpha) is .1
     
      if(readDepth=="2.5M")
      {
        plotMA(dds,ylim=c(-2,2),main= paste("DESeq2 ", "2.5M"), alpha=.05 )
        dev.copy(png,"MA-2.5.png")
        dev.off()
        plotMA(dds,ylim=c(-3,3),main= paste("DESeq2 ", "2.5M"), alpha=.05)
        dev.copy(png,"MvA-2.5.png")
        dev.off()
      }
      if(readDepth=="10M")
      {
        plotMA(dds,ylim=c(-2,2),main= paste("DESeq2 ", "10M"), alpha=.05 )
        dev.copy(png,"MA-10.png")
        dev.off()
        plotMA(dds,ylim=c(-3,3),main= paste("DESeq2 ", "10M"), alpha=.05)
        dev.copy(png,"MvA-10.png")
        dev.off()
      } 
      else
      {
        plotMA(dds,ylim=c(-2,2),main= paste("DESeq2 ", "30m"), alpha=.05 )
        dev.copy(png,"MA-30.png")
        dev.off()
        plotMA(dds,ylim=c(-3,3),main= paste("DESeq2 ", "30m"), alpha=.05)
        dev.copy(png,"MvA-30.png")
        dev.off()
      }
      
       
      
      
      #transform the raw counts for clustering:
      #choose blind so that conditions does not influence the outcome, 
      # to see if conditions cluster based purely on the individual datasets, in an unbiased way.
      rld <- rlogTransformation(dds, blind=TRUE) # regularized log
      vsd <- varianceStabilizingTransformation(dds, blind=TRUE) # DESeq's variance stabilisation
      
      ################## Biotech may stop here ############################### :) It does not!
      
      #see which approaches have more consistent SD across the read counts
      # source("http://bioconductor.org/biocLite.R")
      # biocLite("vsn")
      library("vsn")
      par(mfrow=c(1,3))
      notAllZero <- (rowSums(counts(dds))>0)
      # log2 doesn't do well for low read counts (SD is high, varies)
      #?meanSdPlot
      
      if(readDepth=="2.5M")
      {
        ylabval = 2.5
        meanSdPlot(log2(counts(dds,normalized=TRUE)[notAllZero,] + 1), ylab = c(0,ylabval))
        dev.copy(png,"deseq2_heatmaps_samplebysample_1-2.5.png")
        dev.off()
        
        meanSdPlot(assay(rld[notAllZero,]), ylab = c(0,ylabval))
        dev.copy(png,"deseq2_heatmaps_samplebysample_2-2.5.png")
        dev.off()
        
        meanSdPlot(assay(vsd[notAllZero,]), ylab = c(0,ylabval))
        dev.copy(png,"deseq2_heatmaps_samplebysample_3-2.5.png")
        dev.off()
      }
      if(readDepth=="10M")
      {
        ylabval = 10
        meanSdPlot(log2(counts(dds,normalized=TRUE)[notAllZero,] + 1), ylab = c(0,ylabval))
        dev.copy(png,"deseq2_heatmaps_samplebysample_1-10.png")
        dev.off()
        
        meanSdPlot(assay(rld[notAllZero,]), ylab = c(0,ylabval))
        dev.copy(png,"deseq2_heatmaps_samplebysample_2-10.png")
        dev.off()
        
        meanSdPlot(assay(vsd[notAllZero,]), ylab = c(0,ylabval))
        dev.copy(png,"deseq2_heatmaps_samplebysample_3-10.png")
        dev.off()
      } 
      else
      {
        ylabval = 30
        meanSdPlot(log2(counts(dds,normalized=TRUE)[notAllZero,] + 1), ylab = c(0,ylabval))
        dev.copy(png,"deseq2_heatmaps_samplebysample_1-30.png")
        dev.off()
        
        meanSdPlot(assay(rld[notAllZero,]), ylab = c(0,ylabval))
        dev.copy(png,"deseq2_heatmaps_samplebysample_2-30.png")
        dev.off()
        
        meanSdPlot(assay(vsd[notAllZero,]), ylab = c(0,ylabval))
        dev.copy(png,"deseq2_heatmaps_samplebysample_3-30.png")
        dev.off()
      }
      
      
      ##n
      # regularized log (center) and DESeq's variance stabilisation (right)
      #  transformations do better across the  range of counts
      
      
      # calculate sample to sample Euclidean distances betw lg fold change of genes < WRT what? ********
      #   if dist is near zero, then very similar (dark blue is same sample zero dist)
      # use rlog-transformed data to avoid domination by a few highly variable genes 
      # dist calculates distances between rlog-transforms in data rows
      #  our samples constitute columns so use t to transpose the matrix
      distsRL <- dist(t(assay(rld)))   
      mat <- as.matrix(distsRL)
      rownames(mat) <- colnames(mat) <- with(colData(dds), paste(condition,sampleName , sep=" : "))
      
      # visualizes distances and clusters:
      #From the Apr 2015 vignette
      library("RColorBrewer")
      library("gplots")
      select <- order(rowMeans(counts(dds,normalized=TRUE)),decreasing=TRUE)[1:30]
      hmcol <- colorRampPalette(brewer.pal(9, "GnBu"))(100)
      hc <- hclust(distsRL)
      heatmap.2(mat, Rowv=as.dendrogram(hc),
                symm=TRUE, trace="none",
                col = rev(hmcol), margin=c(13, 13))
      
      if(readDepth=="2.5M")
      {
        dev.copy(png,"deseq2_heatmaps_samplebysample_2.5.png")
        dev.off()
      }
      if(readDepth=="10M")
      {
        dev.copy(png,"deseq2_heatmaps_samplebysample_10.png")
        dev.off()
      }
      else
      {
        dev.copy(png,"deseq2_heatmaps_samplebysample_30.png")
        dev.off()
      }
      #PCA
      colours <- c(rgb(1:3/4,0,0),rgb(0,1:3/4,0),rgb(0,0,1:3/4),rgb(1:3/4,0,1:3/4))
      plotPCA( rld, intgroup = c("sampleName","condition") )
      
}

