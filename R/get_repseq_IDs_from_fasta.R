#' @title Get Representative Sequence Names
#' @name get_repseq_IDs_from_fasta
#' @aliases get_repseq_IDs_from_fasta
#' @description This function retrieves a vector of the representative sequence machine names from a fasta file of representative sequences.
#' @usage get_repseq_IDs_from_fasta(repseq_file = "all_seq_complete.clust_rep_seqs.fasta")
#' @param repseq_file The file name of the combined fasta file containing the representative sequences.
#' @details The function reads the fasta file of representative sequences from disk.
#' @return A vector of the machine names of the representative sequences.
#' @references 
#' The web-based tool for retrieving representative sequences is here:
#' http://pyro.cme.msu.edu/
#'  
#' A command line version of the tool is included in RDPTools, available on GitHub:
#' https://github.com/rdpstaff
#' @author John Quensen
#' @examples 
#' repseq.file <- system.file("extdata", "all_seq_complete.clust_rep_seqs.fasta", package="RDPutils")
#' rep.seqs <- get_repseq_IDs_from_fasta(repseq_file = repseq.file)
#' rep.seqs
#' @keywords RDPTols

get_repseq_IDs_from_fasta <-
function(repseq_file="all_seq_complete.clust_rep_seqs.fasta") {
  # Read in fasta file as a text file.
  fasta <- readLines(repseq_file)
  
  # Remove sequences, keep only headers.
  keep <- seq(from=1, to=(length(fasta)), by=2)
  fasta <- fasta[keep]
  
  # Get repseq.IDs
  repseq.IDs <- gsub(">(\\w+).+", "\\1", fasta, perl=TRUE)
  
  return(repseq.IDs)
}
