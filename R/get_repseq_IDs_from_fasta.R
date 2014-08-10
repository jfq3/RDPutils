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
