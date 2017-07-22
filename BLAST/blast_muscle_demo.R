# from page bottom of:
# http://www.molgen.ua.ac.be/bioinfo/acourse/Bioconductor-by-examples.txt

# Some web services based packages
# ================================

#install.packages("RCurl")
#install.packages("Biostrings")
#library(curl)
#install.packages("curl")
#install.packages("rentrez")
#install.packages("curl")

library(Biostrings)
library(RCurl)
library(rentrez)

seq1 <- entrez_fetch(db = "sequences", rettype = 'fasta', id = "187830855") #human
seq2 <- entrez_fetch(db = "sequences", rettype = 'fasta', id = "470595373") # bottlenosed dolphin 

sequences <- c(seq1, seq2)

# # get a sequence by id (using NCBI eutil efetch)
getprotein = function(id) {
  seq=getForm("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi", db="protein", id=id, retmode="text", rettype="fasta", binary=F)
  s=strsplit(seq[1],"\\n")[[1]]
  seq=AAString(paste(s[2:length(s)],collapse=""))
}
seq=getprotein("15718680")

#Here is the FASTA sequence for one of the isoforms of human P53 protein:
# seq <- paste("1 meepqsdpsv epplsqetfs dlwkllpenn vlsplpsqam ddlmlspddi eqwftedpgp
#  61 deaprmpeaa ppvapapaap tpaapapaps wplsssvpsq ktyqgsygfr lgflhsgtak
# 121 svtctyspal nkmfcqlakt cpvqlwvdst pppgtrvram aiykqsqhmt evvrrcphhe
# 181 rcsdsdglap pqhlirvegn lrveylddrn tfrhsvvvpy eppevgsdct tihynymcns
# 241 scmggmnrrp iltiitleds sgnllgrnsf evrvcacpgr drrteeenlr kkgephhelp
# 301 pgstkralpn ntssspqpkk kpldgeyftl qdqtsfqken c")

# blast a sequence using the NCBI REST interface
ncbi_blast = function(seq,database="nr",expect=10,program="blastp",hitlistsize=100) {
  # based on the NCBI REST interface (http://www.ncbi.nlm.nih.gov/blast/Doc/urlapi.html)
  job = as.character(getForm("http://blast.ncbi.nlm.nih.gov/Blast.cgi", QUERY=seq, DATABASE=database,
                             HITLIST_SIZE=hitlistsize, FILTER="L", EXPECT=expect, FORMAT_TYPE="Text", PROGRAM=program, CLIENT="web",
                             SERVICE="plain", NCBI_GI="on", PAGE="Nucleotides", CMD="Put"))
  m = regexpr("RID = ([^\n]+)",job)
  id = substring(job,m[1]+6,m[1]+attributes(m)$match.length-1)
  m = regexpr("RTOE = ([^\n]+)",job)
  rtoe = substring(job,m[1]+7,m[1]+attributes(m)$match.length-1)
  result = "Status=WAITING"
  while (length(grep("Status=WAITING",result))) {
    Sys.sleep(1)
    result = as.character(getForm("http://blast.ncbi.nlm.nih.gov/Blast.cgi", RID=id, FORMAT_TYPE="Text", CMD="Get"))
  }
  getForm("http://blast.ncbi.nlm.nih.gov/Blast.cgi", RID=id, CMD="Delete")
  return(result)
}
b = ncbi_blast(seq,database="nr",program="blastp") 
#b = ncbi_blast(seq,database="nr",program="PSI-BLAST") 
head(b)
cat(b)

# multiple sequence alignment
# EBI REST service for multiple sequence alignment using muscle (http://www.ebi.ac.uk/Tools/webservices/services/msa/muscle_rest#resulttypes)
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

#result = muscle(b)
result = muscle(sequences)
cat(result)
