#' @title Rename Representative Sequences
#' @name rename_fasta
#' @aliases rename_fasta
#' @description This function renames representative sequences from their machine names to their OTU names.
#' @usage rename_fasta(in_file = "names_trimmed.fasta", out_file = "renamed.fasta", rename_table)
#' @param in_file The name of the fasta file containing representative sequences with their machine names.
#' @param out_file The name of the fasta file containing representative sequences with their OTU names.
#' @param rename_table A data frame associating the machine names of representative sequences with their corresponding OTU names.
#' @details 
#' The output of the function assoc_repseq_IDs_with_otus_by_fasta or assoc_repseq_IDs_with_otus_by_clstr  can be used as the rename_table.
#' Before representative sequences can be renamed with this function, their IDs have to be shortened to include only their machine names with the function trim_fasta_names.
#' @author John Quensen
#' @export
#' @seealso 
#' [assoc_repseq_IDs_with_otus_by_clstr()]
#' [assoc_repseq_IDs_with_otus_by_fasta()]
#' [trim_fasta_names()]
#' @examples 
#' in.file <- system.file("extdata", "names_trimmed.fasta", package="RDPutils")
#' data(assoc.table)
#' rename_fasta(in_file = in.file, out_file = "renamed.fasta", rename_table = assoc.table)
#' @keywords RDPTools

rename_fasta <-
function(in_file="names_trimmed.fasta", out_file="renamed.fasta", rename_table) {
  # Read in fasta file as text file.
  fasta <- readLines(in_file)
  # Rename the fasta files with corresponding OTU name taken from rename_table
  for (i in 1:nrow(rename_table)) {
    b <- paste(">", rename_table[i,2], sep="")
    fasta[2*i-1] <- b
  }
  # Write the renamed fasta files to out_file.
  con <- file(out_file, 'w') 
  writeLines(fasta, con)
  close(con)
  message <- paste("Fasta sequences renamed with OTU names; written to file ", out_file, ".", sep="")
  return(message)
}
