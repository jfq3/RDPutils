unalign_fasta <-
function(in_file, out_file) {
  # Read in fasta file as a text file.
  fasta <- readLines(in_file)
  # Remove model sequences if present.
  x <- length(grep(">#=GC_RF", fasta, fixed=TRUE))
  if (x > 0) {
    message <- paste(x, "model sequences found and removed.")
    line.no <- grep(">#=GC_RF", fasta, fixed=TRUE)
    line.no <- c(line.no,(line.no+1))
    fasta <- fasta[-line.no]
  }
  # Open output file.
  con <- file(out_file, 'w') 
  # Write altered file to disk.
  for (i in seq(fasta)) {
    if (i %% 2 == 0) {
      fasta[i] <- gsub("[^agctAGCT]", "", fasta[i], perl=TRUE)
      fasta[i] <- toupper(fasta[i])
    }
    writeLines(fasta[i], con)
  }
  close(con)
  message <- paste("Unaligned fasta file written to file ", out_file, ".", sep="" )
  return(message)
}
