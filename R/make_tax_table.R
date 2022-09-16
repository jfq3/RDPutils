#' @title Make a Phyloseq tax_table from Classifier Result
#' @name make_tax_table
#' @aliases make_tax_table
#' @description This function creates a phyloseq tax_table from the RDP classifier result (detail format) for renamed representative sequences.
#' @usage make_tax_table(in_file="fixrank_classified.txt", confidence=0.5)
#' @param in_file File name for RDP classifier result in detail format.
#' @param confidence Confidence level for construction of tax_table.
#' @details The input file is the file designated by the "-o" argument to the command line RDP classifer.  The format given by "-f" must be "fixrank."  The confidence level "-c" in the classifier command is unimportant; instead, the tax_table will be constructed according to the confidence argument to make_tax_table.  This confidence argument is similar in concept to "-c" which actually affects only the hierarchical (-h) and fillterbyconf (-f) output of the classifier.  For example, after classifying the file "renamed_repseqs.fasta" (16S rRNA DNA sequences) with the commands:
#' cd <to directory with fasta file to be classified>
#' java -Xmx1g -jar /path_to_classfier.jar/classifier.jar classify -g 16srrna -f fixrank -o test_classify.txt renamed_repseqs.fasta
#' use "test_classify.txt" as the in_file to make_tax_table.
#' Ranks classified with confidence less than that specified are filled as unclassifed higher rank.  For example, the family and genus assigned to OTUs classified with confidence greater than 0.5 only as far as order "Actinomycetales" would be "unclass_Actinomycetales."
#' @returns A phyloseq tax_table object.
#' @author John Quensen
#' @export
#' @examples 
#' my.in.file <- system.file("extdata", "fixrank_classified.txt", package="RDPutils")
#' my.tax.table <- make_tax_table(in_file = my.in.file, confidence=0.5)
#' my.tax.table
#' @keywords RDPTools

make_tax_table <- function(in_file="fixrank_classified.txt", confidence=0.5) {
  #Begin with output from command line version of classifier.
  #    In this example, the example output is named "dist_03_class.txt."
  #    It is fixed rank.
  class.table <- read.table(in_file, sep="\t", fill=TRUE, stringsAsFactors=FALSE)
  
  rank.pos <- seq(from = 4, to = ncol(class.table), by = 3)
  ranks <- as.vector(class.table[1 , rank.pos])
  
  # Remove unnecessary columns
  class.table <- class.table[ , -c(2, rank.pos)]
  head(class.table)
  
  # Assign first column of class.table as row names.
  # Delete first column of class.table.
  # Sort class.table by row names.
  row.names(class.table) <- class.table[ , 1]
  class.table <- class.table[ , -1]
  class.table <- class.table[order(rownames(class.table)), ]
  
  # Fix class.table's by consolidating groups based on confidences.
  # For example, genera classified with less than specified confidence become uncl_family, etc.
  
  # Create a vector designating confidence columns
  col.no <- seq(from = 2, to = ncol(class.table), by = 2)
  
  #There may be NA's in some columns, so replace them first 
  # with confidence < specified confidence:
  for (i in 1:nrow(class.table)) {
    for (j in col.no) {
      if (is.na(class.table[i, j])) {
        class.table[i, j] <- confidence/2
      }
    }
  }
  
  #Replace IDs where first rank is unidentfied.
  first.rank <- ranks[1]
  for (i in  1:nrow(class.table)) {
    if (class.table[i, 2] < confidence) {
      class.table[i, 2] <- 1
      class.table[i, 1] <- paste("uncl", first.rank, sep = "_")
    }
  }
  
  #Replace IDs where confidence is less than specified confidence:
  col.no <- seq(from=4, to=ncol(class.table), by=2)
  for (i in 1:nrow(class.table)) {
    for (j in col.no) {
      if (class.table[i, j] < confidence) {
        class.table[i, j] <- 1
        if(substr(class.table[i, (j-3)], 1, 4)=="uncl") {class.table[i, (j-1)] <- class.table[i, (j-3)]}
        else {class.table[i, (j-1)] <- paste("uncl_", class.table[i, (j-3)], sep="")}
      }
    }
  }
  
  #Convert character columns to factors.
  i <- sapply(class.table, is.character) 
  class.table[i] <- lapply(class.table[i], as.factor) 
  
  #Remove confidence columns
  class.table <- class.table[ , -c(seq(from = 2, to = ncol(class.table), by = 2))]
  ranks <- sapply(ranks, simple_cap)
  colnames(class.table) <- ranks
  class.table <- tax_table(as.matrix(class.table), errorIfNULL=TRUE)
  return(class.table)
}