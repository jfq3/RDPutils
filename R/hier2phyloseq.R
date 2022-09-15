#' @title Classification File Converter
#' @name hier2phyloseq
#' @aliases hier2phyloseq
#' @description Converts RDP's hierarchical classification file to a phyloseq object.
#' @usage hier2phyloseq(hier_file = "test_hier.txt")
#' @param hier_file Tab-delimited text file from RDP's command line classifier with option filterbyconf.
#' @details 
#' RDP's classifier generates two types of output.  The detail format gives the classification of each sequence input.  The hierachical format gives the number of sequences in each taxon.  If the classifier is given a number of samples at the same time, the hierachical format can be filtered in Excel to give an otu table for a given rank, with taxa as rows and samples as columns.
#' The command line classifier takes arguments for confidence level and output format.  For example, 28S fungal sequences can be classified with the commands:
#' cd c:\\test
#' java -Xmx1g -jar /path_to_classfier.jar/classifier.jar classify -g fungallsu -c 0.5 -f filterbyconf -o test_classified.txt -h test_hier.txt *.fasta
#' All sample fasta files in directory c:\\test are classified to a rank with confidence of 0.5 or more.  By setting the format to filterbyconf, ranks not identified with a confidence of at least 0.5 are empty.
#' hier2phyloseq converts the hierachical result, file test_hier.txt in the example above, to a phyloseq object with otu_table and tax_table.  Empty ranks are filled as unclassifed higher rank.  For example, the family and genus assigned to sequences classified with confidence greater than 0.5 only as far as order "Pleosporales"" would be "unclass_Pleosporales."
#' The sample names are shortened by removing common prefixes and suffixes introduced by RDP's tools.  These include "nc_", "aligned_", "_trimmed", ".fasta" and ".fastq."
#' This function is not appropriate for processing a single sample.  To make a tax_table phyloseq object, use function "make_tax_table."
#' @returns Returns a phyloseq object with otu_table and tax_table.
#' @author John Quensen
#' @seealso make_tax_table import_rdp_tax_table
#' @examples 
#' hier.file <- system.file("extdata", "test_hier.txt", package="RDPutils")
#' expt <- hier2phyloseq(hier_file=hier.file)
#' expt
#' @keywords RDPTools
#' @export


# Function to convert classifier hierarchy file to phyloseq object
# with OTU and taxa tables.
hier2phyloseq = function(hier_file="test_hier.txt") {
  
  # Read classifier's hier.txt file.
  taxa <- read.table(file=hier_file, header=FALSE, sep="\t", stringsAsFactors=FALSE, fill=TRUE)
  colnames(taxa) <- taxa[1,]
  taxa <- taxa[-1,]

  all.ranks <- unique(taxa$rank)
  possible.ranks <- c("rootrank", "kingdom", "domain","phylum","class", "order", "family", "genus", "species", "Rootrank", "Kingdom", "Domain","Phylum","Class", "Order", "Family", "Genus", "Species")
  actual.ranks <- intersect(all.ranks, possible.ranks)
  n.ranks <- length(actual.ranks)
  last.rank <- actual.ranks[n.ranks]
  
  # Subset rows of taxa to those with lowest rank or empty
  taxa <- taxa[taxa$rank %in% c(last.rank, ""), ]
  
  # Add taxa names of format OTU_xxxx.
  rownames(taxa) <-make_otu_names(seq_len(nrow(taxa)))
  
  # Make empty classification table.
  my.table <- matrix("", n.ranks, nrow=nrow(taxa))
  colnames(my.table) <- actual.ranks
  rownames(my.table) <- rownames(taxa)
    
  # Fill in classification table with rank names from taxa.
  rank.names <- colnames(my.table)
  
  for (k in 1:ncol(my.table)){
    for (i in 1:nrow(my.table)) {
      my.line <- unlist(strsplit(taxa[i,2],";"))
      if (rank.names[k] %in% my.line) {
        j <- which(my.line==rank.names[k])
        level.name <- my.line[j-1]
        my.table[i,k] <- level.name
      }
    }
  }
  
  # Fill in empty cells in classfication table with"unclass_higher rank".
  for (i in 1:nrow(my.table)) {
    for (j in 2:ncol(my.table)) {
      if (my.table[i,j]=="") {
        if (substr(my.table[i,j-1],1,7)=="unclass") {my.table[i,j]<-my.table[i,j-1]}
        else {my.table[i,j]<- paste("unclass_", my.table[i,j-1], sep="")}
      }
    }
  }
  
  # Make sure rank names are capitalized.
  rank.names <- sapply(rank.names, simple_cap)
  colnames(my.table) <- rank.names

  # Make phyloseq object containing OTU table and taxonomy table
  otu <- taxa[ , -c(1:4)]
  rownames(otu) <- rownames(taxa)
  otu[] <- lapply(otu, as.numeric)
  
  # Make common changes to sample names.
  sam.names <- colnames(otu)
  sam.names <- gsub("nc_", "", sam.names, fixed=TRUE)
  sam.names <- gsub("_trimmed", "", sam.names, fixed=TRUE)
  sam.names <- gsub("aligned_", "", sam.names, fixed=TRUE)
  sam.names <- gsub(".fasta","", sam.names, fixed=TRUE)
  sam.names <- gsub(".fastq","", sam.names, fixed=TRUE)
  sam.names <- gsub("_",".", sam.names, fixed=TRUE)
  colnames(otu) <- sam.names

  my.tax <- phyloseq::tax_table(my.table, errorIfNULL=TRUE)
  my.otu <- phyloseq::otu_table(as.matrix(otu), taxa_are_rows=TRUE, errorIfNULL=TRUE)
  my.expt <- phyloseq::phyloseq(my.otu, my.tax)
  
  return(my.expt)
  
}
