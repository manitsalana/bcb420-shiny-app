# personalGenome.R
#
# Purpose: Analyze the differences between coding and non-coding regions of
#          Chr 20 in participant 16 of the Personal Genome project
# Version: 1.0
# Date: March 2018
# Author: Alana Man <alana.man@mail.utoronto.ca>
#
# Input: vcf files
# Output: 
# Dependencies: 
#
# Version history:
#
# ToDo:
# Notes:
#
# ==============================================================================


# ====  PARAMETERS  ============================================================
# Define and explain all parameters. No "magic numbers" in your code below.



# ====  PACKAGES  ==============================================================
# Load all required packages. Example for the idiom:

#if (! require(stringr, quietly=TRUE)) {
#  install.packages("stringr")
#  library(stringr)
#}

if (! require(vcfR, quietly=TRUE)) {
  install.packages("vcfR")
  library(vcfR)
}

if (! require(biomaRt, quietly=TRUE)) {
  install.packages("biomaRt")
  library(biomaRt)
}
# Package information:
#  library(help = stringr)       # basic information
#  browseVignettes("stringr")    # available vignettes
#  data(package = "stringr")     # available datasets

# ====  FUNCTIONS  =============================================================

# Define functions or source external files

myFunction <- function(a, b=1) {
  # Purpose:
  #     Describe ...
  # Parameters:
  #     a: ...
  #     b: ...
  # Value:
  #     result: ...
  
  # code ...
  
  return(result)
}



# ====  PROCESS  ===============================================================

p16Vcf <- read.vcfR(file="./personalGenome/PGPC_0016/PGPC_0016_S1.flt.vcf")

# my original idea... gave me a matrix with 790k elements
chr20Vcf = p16Vcf@fix[p16Vcf@fix[, 1]=="chr20", ]

# this seems to be the more correct method... 
# returns a matrix of 691k elements
p16FIX <- getFIX(p16Vcf)
p16FIX <- p16FIX[p16FIX[,1]=="chr20", ]

# this only helps prune 80k... so now down to 614740 elements
p16FIX_highQ <- p16FIX[(p16FIX[p16FIX[,1]=="chr20", ])[,7]=="PASS", ]

# looks like we can get the reference from biomaRt
# http://useast.ensembl.org/info/data/biomart/biomart_r_package.html
referenceGenome = useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl", GRCh=37)

# now we want to see which variants belong in coding regions and which ones don't
# so first from the reference genome, we get everything chromosome 20 related
# and the start and end positions of the genes, and probably the gene symbols will
# be nice to have as well
# listFilters(referenceGenome)[,1:2]
# listAttributes(referenceGenome)[,1:2]
referenceChr20 <- getBM(filters=c('chromosome_name', 'biotype'), 
      attributes = c('chromosome_name','start_position', 'end_position', 'hgnc_symbol'), 
      values = list(20, 'protein_coding'), 
      mart = referenceGenome)
nrow(referenceChr20) # 560

# let's work with a data frame because it's easier
p16FIXhq_df <- as.data.frame(p16FIX_highQ)
p16FIXhq_df$Coding <- rep(FALSE, nrow(p16FIXhq_df))
# https://stackoverflow.com/questions/37707060/converting-data-frame-column-from-character-to-numeric
# let's get that position number as a numeric to assist with my labelling task
p16FIXhq_df$POS <- as.numeric(as.character(p16FIXhq_df$POS)) # no NANs and no NAs so assume
                                                             # that it tranformed properly

# label set Coding to true if they lie within the start and end position of a gene
for (i in seq_along(referenceChr20$chromosome_name)) {
  
  # check if there are any gene variants within the start and end position of the gene
  # we are currently looking at for chromosome 20
  currGeneVariants <- p16FIXhq_df[((p16FIXhq_df$POS >= referenceChr20$start_position[i]) & 
                (p16FIXhq_df$POS <= referenceChr20$end_position[i])),]
  
  # if there are any, then let's set them all to TRUE
  # I probably didn't need this if statement, but I am leaving it here
  # for clarity... the run time for this for loop is super small anyways
  if (nrow(currGeneVariants) > 0) {
    p16FIXhq_df[((p16FIXhq_df$POS >= referenceChr20$start_position[i]) & 
                   (p16FIXhq_df$POS <= referenceChr20$end_position[i])),'Coding'] <-
      rep(TRUE, nrow(currGeneVariants))
  }
}

nrow(p16FIXhq_df[p16FIXhq_df$Coding==TRUE,]) # 35882
nrow(p16FIXhq_df[p16FIXhq_df$Coding==TRUE,])/nrow(p16FIXhq_df) # 0.4085857

# ============ PLOTTING  =====================================================

# first let's which genes are located where
plot(table(p16FIXhq_df$POS), main="Number of variants at bp positions", 
     xlab="Variant Position", ylab="Number of Variants")

# well.. what else can we understand given our data? we have the coding region variants,
# and we have the non coding ones... we can look at number of variants at a specific location.
# It could give a sense on which genes have more stacked variants
plot(table(p16FIXhq_df$POS[p16FIXhq_df$Coding==TRUE]), 
     main="Number of variants at bp positions", 
     xlab="Variant Position", ylab="Number of Variants", 
     type="p", col=rgb(1, 0.6, 0, alpha=0.3), pch=1)
points(table(p16FIXhq_df$POS[p16FIXhq_df$Coding==FALSE]), 
       type="p", col=rgb(0, 0.5, 1, alpha=0.3), pch=2)
legend(x="bottomright", y=1, pch=c(1,2), 
       col=c("orange", "blue"), 
       legend=c("Coding", "Non-Coding"), cex=0.8)

# [END]