#install.packages("RCurl")
library(Biostrings)
library(RCurl)
#install.packages("rentrez")
library(rentrez)


#You may want to fetch sequences for MSA with MUSCLE using this sort of command for each of your 16 sequences 
seq1 <- entrez_fetch(db = "sequences", rettype = 'fasta', id = "187830855") #human
seq2 <- entrez_fetch(db = "sequences", rettype = 'fasta', id = "470595373") # bottlenosed dolphin 


#The line beginning with "fasta=" has been commented out and replaced with a line
# to use when seq is created as a vector of sequences retrieved from Entrez DB
muscle = function(sequences) {
  # convert sequences to fasta
  fasta=paste(sequences,collapse="\n")
  #fasta=paste(paste(">",names(sequences),sep=""),as.character(sequences),sep="\n",collapse="\n")
  id=postForm("http://www.ebi.ac.uk/Tools/services/rest/muscle/run/",style="post",email="pfrancislyon@usfca.edu",format="fasta",sequence=fasta)
  status = ""
  while (status != "FINISHED") {
    Sys.sleep(1)
    status=getURL(sprintf("http://www.ebi.ac.uk/Tools/services/rest/muscle/status/%s",id))
  }
  resulttype="aln-fasta"
  ali=getURL(sprintf("http://www.ebi.ac.uk/Tools/services/rest/muscle/result/%s/%s", id, resulttype))
}

# write to file
result = muscle(seqs)
sink("muscle_align.txt")
cat(result)
sink()

## create an object from file of aligned sequences
msa <- readAAMultipleAlignment(filepath ="muscle_align.txt", format="fasta") 

## See a detailed pager view
if (interactive()) {
  detail(msa)
}

## calculate frequencies
alphabetFrequency(msa)
consensusMatrix(msa, baseOnly=TRUE)[, 84:90] 

## cluster the alignments
sdist <- stringDist(as(msa,"AAStringSet"), method="levenshtein")
clust <- hclust(sdist, method = "single")
plot(clust)
fourgroups <- cutree(clust, 4)
fourgroups

# ## if you have a phylips alignment write out the alignement object to Phylip format
# write.phylip(x = msa, filepath = tempfile("foo.txt",tempdir()))


autoMasked <- maskGaps(msa, min.fraction=0.5, min.block.width=4)
autoMasked


# code chunks from Biostrings documentation that comes with the Biostrings package:
### http://127.0.0.1:18596/library/Biostrings/doc/MultipleAlignments.R
###################################################
### code chunk number 14: cluster
###################################################
sdist <- stringDist(as(msa,"AAStringSet"), method="hamming")
clust <- hclust(sdist, method = "single")
#pdf(file="badTree.pdf")
plot(clust)
#dev.off()


###################################################
### code chunk number 15: cluster2
###################################################
sdist <- stringDist(as(autoMasked,"AAStringSet"), method="hamming")  ## DNAStringSet vs AAStringSet
clust <- hclust(sdist, method = "single")
#pdf(file="goodTree.pdf")
plot(clust)
#dev.off()
fourgroups <- cutree(clust, 4)
fourgroups


###################################################
### code chunk number 16: fastaExample (eval = FALSE)
###################################################
AAStr = as(msa, "AAStringSet")
writeXStringSet(AAStr, file="myFile.fa")

