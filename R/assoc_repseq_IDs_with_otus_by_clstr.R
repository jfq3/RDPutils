#' Associate Representative Sequence IDs with OTUs from Cluster File
#' assoc_repseq_IDs_with_otus_by_clstr
#' @description This function parses a cluster file for a single distance and makes a table associating the representative sequence machine names with the OTU names as given by RDP's cluster file formatter with options "R" or "biom."
#' @param clstr_file The name of a cluster file for a single distance.
#' @param rep_seqs A vector of representative sequence machine names.
#' @param otu_format When equal to "R" (default) OTU names have the form "OTUxxxnn."  When equal to "biom", OTU names have the form "cluster_nn."
#' @details 
#' The first input to this function is the name of a cluster file for a single distance. The function reads the cluster file from disk. The cluster file for a single distance is excised from the original cluster file which likely contained cluster information for several distances with either of the functions clstr2otu or split_clstr_file. 
#' The second input to this function is a vector of representative sequence machine names corresponding to the input cluster file; that is, they are for the same distance.  This vector is created from a fasta file of the representative sequences with the function get_repseq_IDs_from_fasta. 
#' OTUs may be named in either of two formats, corresponding to those output by the RDP's cluster file formatter with options "R" and "biom."  With option "R" (the default), OTU names begin with "OTU" and are padded to equal length with leading zeros, e.g. "OTU00067."  Thus they can be sorted in numerical order.  With option "biom," OTU names begin with "cluster_" and are not padded with leading zeros, e.g "cluster_67."
#' @usage assoc_repseq_IDs_with_otus_by_clstr(clstr_file, rep_seqs, otu_format)
#' @author John Quensen
#' @references The web-based tool for retrieving representative sequences is here: http://pyro.cme.msu.edu/   The RDPTools are available on GitHub:  https://github.com/rdpstaff
#' @examples 
#' repseq.file <- system.file("extdata", "all_seq_complete.clust_rep_seqs.fasta", package="RDPutils")
#' rep.seqs <- get_repseq_IDs_from_fasta(repseq_file = repseq.file)
#' clstr.file <- system.file("extdata", "dist_03.clust", package="RDPutils")
#' assoc.table <- assoc_repseq_IDs_with_otus_by_clstr(clstr_file = clstr.file, rep_seqs = rep.seqs)
#' head(assoc.table)
#' @keywords RDPTools

assoc_repseq_IDs_with_otus_by_clstr <-
function(clstr_file, rep_seqs, otu_format="R") {
  clstr.file <- read.table(file=clstr_file, stringsAsFactors=FALSE, header=FALSE,  sep="\t", fill=TRUE)
  
  # Create empty data frame of proper dimensions.
  my.table <- matrix(0, length(rep_seqs), ncol=4)
  my.table <- as.data.frame(my.table)
  colnames(my.table) <- c("RepSeq.ID", "Cluster #", "Sample","Sequences in Sample")
  
  # Fill in the table. Takes a while.
  for (i in (1:length(rep_seqs))){
    j <- grep(rep_seqs[i], clstr.file[,4], fixed=TRUE)
    my.row <- c(rep_seqs[i], clstr.file[j,1:3])
    my.row <- unlist(my.row)
    my.table[i,] <- my.row
  }
  
  # Set proper classes for each column
  i <- c(TRUE, FALSE, TRUE, FALSE)
  my.table[i] <- lapply(my.table[i], as.character)
  i <- c(FALSE, TRUE, FALSE, TRUE)
  my.table[i] <- lapply(my.table[i], as.numeric)
  
  # Add column of OTU names.
  my.table$OTU <- make_otu_names(my.table[,2], otu_format)
  my.table <- my.table[, c(1,5,2,3,4)]
  i <- c(F,T,F,F,F)
  my.table[i] <- lapply(my.table[i], as.character)
  return(my.table)
}
