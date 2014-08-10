remove_model_seqs <-
function(in_file, out_file=in_file) {
  fasta <- readLines(in_file)
  x <- length(grep(">#=GC_RF", fasta, fixed=TRUE))
  if (x > 0) {
    message <- paste(x, "model sequences found and removed.")
    line.no <- grep(">#=GC_RF", fasta, fixed=TRUE)
    line.no <- c(line.no,(line.no+1))
    fasta <- fasta[-line.no]
    writeLines(fasta, out_file)
  } else {
    message <- "No model sequence found."
  }
  return(message)
}
