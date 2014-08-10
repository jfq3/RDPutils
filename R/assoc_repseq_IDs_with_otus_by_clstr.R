assoc_repseq_IDs_with_otus_by_clstr <-
function(clstr_file="dist_0.03.clust", rep_seqs=rep.seqs) {
  clstr.file <- read.table(file=clstr_file, stringsAsFactors=FALSE, header=FALSE,  sep="\t", fill=TRUE)
  
  # Create empty data frame of proper dimensions.
  my.table <- matrix(0, length(rep_seqs), ncol=4)
  my.table <- as.data.frame(my.table)
  colnames(my.table) <- c("RepSeq.ID", "Cluster #", "Sample","Sequences in Sample")
  dim(my.table) # Check
  head(my.table) # Check
  
  # Fill in the table. Takes a while.
  for (i in (1:length(rep_seqs))){
    j <- grep(rep_seqs[i], clstr.file[,4], fixed=TRUE)
    my.row <- c(rep_seqs[i], clstr.file[j,1:3])
    my.row <- unlist(my.row)
    my.table[i,] <- my.row
  }
  
  # Set proper classes for each column
  i <- c(TRUE, FALSE, TRUE, FALSE)
  my.table[i] <- lapply(my.table[i], as.character)
  i <- c(FALSE, TRUE, FALSE, TRUE)
  my.table[i] <- lapply(my.table[i], as.numeric)
  
  # Add column of OTU names.
  my.table$OTU <- make_otu_names(my.table[,2])
  my.table <- my.table[, c(1,5,2,3,4)]
  i <- c(F,T,F,F,F)
  my.table[i] <- lapply(my.table[i], as.character)
  return(my.table)
}
