rename_fasta <-
function(in_file="names_trimmed.fasta", out_file="renamed.fasta", rename_table=assoc.table) {
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
