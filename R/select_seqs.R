#' @title Select Fasta Sequences
#' @name select_seqs
#' @aliases select_seqs
#' @description This function subsets a combined fasta file.
#' @usage select_seqs(in_file, select_list, out_file)
#' @param in_file The name of a combined fasta file to be read from disk.
#' @param select_list A vector of the names of the individual fasta files to be kept.
#' @param out_file The name of the modified combined fasta file to be written to disk.
#' @details This function can be used to select a subset of the renamed representative sequences corresponding to OTUs containing at least n sequences.  See the example section below.
#' Reducing the number of representative sequences in this manner makes several subsequent steps go faster: classifying the representative sequences, making a phyloseq tax_table, and treeing the representative sequences.
#' @returns This function operates on disk files.  It is not normally assigned to a variable.  It returns a message that it has completed.
#' @author John Quensen
#' @export
#' @examples 
#' renamed.fasta <- system.file("extdata", "renamed.fasta", package="RDPutils")
#' data(otu)
#' otu <- otu[ , colSums(otu)>=5]
#' select.list <- colnames(otu)
#' select_seqs(in_file=renamed.fasta, select_list=select.list, out_file="subset.renamed.repseqs.fasta")
#' @keywords RDptools

select_seqs = function(in_file, select_list, out_file) {
  con_out <- file(out_file, "a")
  fasta.in <- readLines(in_file)
  n <- seq(1, length(fasta.in),2)
  for (i in n) {
    otu.name <- sub(">(\\w+)", "\\1", fasta.in[i], perl=TRUE)
    if (otu.name %in% select_list) {
      write(fasta.in[i:(i+1)], con_out)
    }
  }
  close(con_out)
}

