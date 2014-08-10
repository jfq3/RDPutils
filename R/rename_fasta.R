rename_fasta <-
function(in_file="names_trimmed.fasta", out_file="renamed.fasta", rename_table=assoc.table) {
  # Read in fasta file as text file.
  fasta <- readLines(in_file)
  # Rename the fasta files with corresponoding OTU name taken from rename_table
  for (i in 1:nrow(rename_table)) {
    a <- paste(">", rename_table[i,1], sep="")
    b <- paste(">", rename_table[i,2], sep="")
    fasta[grep(a, fasta)] <- b
  }
  # Write the renamed fasta files to out_file.
  con <- file(out_file, 'w') 
  writeLines(fasta, con)
  close(con)
  message <- paste("Fasta sequences renamed with OTU names; written to file ", out_file, ".", sep="")
  return(message)
}
