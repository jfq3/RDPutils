# Function assoc_repseq_IDs_with_otus_by_fasta

assoc_repseq_IDs_with_otus_by_fasta = function(repseq_file="all_seq_complete.clust_rep_seqs.fasta") {
  
  # Read in fasta file as a text file.
  fasta <- read.delim(file=repseq_file, header=FALSE, sep="\t", as.is=TRUE)

  # Remove sequences, keep only IDs.
  keep <- seq(from=1, to=(nrow(fasta)), by=2)
  fasta <- fasta[keep,]

  # Extract sequence ID, cluster number, number of sequences from each header.
  fasta.sub <- sub(">(\\w+)\\s+[a-z=,_]+(\\d+)[a-z,=]+(\\d+)", "\\1, \\2, \\3", fasta, perl=TRUE);
  fasta.sub <- as.matrix(fasta.sub)

  # Split lines into columns to give sequence ID, cluster number, number of sequences
  assoc.table <- rep("", length(fasta.sub))
  for (n in 1:length(fasta.sub)) {
    assoc.table[n] <- strsplit(fasta.sub[n], ", ", fixed=TRUE)
  }
  
  # Convert the results from lists to a data frame.
  assoc.table <- as.data.frame(assoc.table)
  assoc.table <- t(assoc.table)
  rownames(assoc.table) <- NULL
  assoc.table <- as.data.frame(assoc.table)
  
  # Convert the variable types from type factor.
  # First convert all to character
  assoc.table[] <- lapply(assoc.table[], as.character)
  # Then convert columns 2 & 3 to numeric. If not done in this order, chaos.  
  i <- c(FALSE, TRUE, TRUE)
  assoc.table[i] <- lapply(assoc.table[i], as.numeric)
 
  # Add column names.
  colnames(assoc.table) <- c("RepSeq.ID", "Cluster #", "Cluster size")
  
  # Add OTU names
  assoc.table$OTU <- as.character(make_otu_names(assoc.table[,2]))
  
  # Reorder columns
  assoc.table <- assoc.table[,c(1,4,2,3)]
  # Reorder rows
  assoc.table <- assoc.table[order(assoc.table[,3]), ]
  head(assoc.table)
  
return(assoc.table)
} # End function.
