trim_fasta_names <-
function(repseq_file, trimmed_names, strip=FALSE) {
  # Read in fasta file as a text file.
  fasta <- readLines(repseq_file)
  for (i in seq(fasta)) {
    if (i %% 2 != 0) {
      fasta[i] <- sub("(>\\w+).+", "\\1", fasta[i], perl=TRUE)
    } else {
      if (strip==TRUE) {
        fasta[i] <- gsub("[^agctAGCT]", "", fasta[i], perl=TRUE)
      }
    }
  }
  if (strip==TRUE) {
    x <- length(grep(">#=GC_RF", fasta, fixed=TRUE))
    if (x > 0) {
      line.no <- grep(">#=GC_RF", fasta, fixed=TRUE)
      line.no <- c(line.no,(line.no+1))
      fasta <- fasta[-line.no]
    }
  }
  writeLines(fasta, trimmed_names)
  message <- paste("Fasta file with trimmed names written to file ",trimmed_names,".", sep="" )
  return(message)
}
