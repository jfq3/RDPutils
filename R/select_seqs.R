# Function select_seqs
# Reads in a fasta file and writes out a subset of that fasta file.
# Names of sequences in the output file are given in list select_list.

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

