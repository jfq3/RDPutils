# Function assoc_repseq_IDs_with_otus_by_fasta

assoc_repseq_IDs_with_otus_by_fasta = function(repseq_file="all_seq_complete.clust_rep_seqs.fasta", otu_format="R") {
  
  # Read in fasta file as a text file.
  fasta <- read.delim(file=repseq_file, header=FALSE, sep="\t", as.is=TRUE)
  
  # Remove sequences, keep only IDs.
  keep <- seq(from=1, to=(nrow(fasta)), by=2)
  fasta <- fasta[keep,]
  
  # Count number of equal signsin the fasta header.
  n.equals <- count_char_occurrences(char="=", strng_x=fasta[1])
  
  if(n.equals==5) {
    # Extract fields, separate by commas.
    fasta.sub  <- sub(">(\\w+)\\s+[a-z=,]+(\\d+)[a-z,=]+(\\d+)[a-zA-Z,=]+([0-9.]+)[a-zA-Z,=]+([0-9.]+)", "\\1, \\2, \\3, \\4, \\5", fasta, perl=TRUE);
    assoc.table <- strsplit(fasta.sub, ", ", fixed=TRUE)
    # Convert the results from lists to a data frame.
    assoc.table <- as.data.frame(assoc.table)
    assoc.table <- t(assoc.table)
    rownames(assoc.table) <- NULL
    assoc.table <- as.data.frame(assoc.table)
    # Convert the variable types from type factor.
    # First convert all to character
    assoc.table[] <- lapply(assoc.table[], as.character)
    # Then convert columns 2 & 3 to integer. If not done in this order, chaos.  
    i <- c(FALSE, TRUE, TRUE, FALSE, FALSE)
    assoc.table[i] <- lapply(assoc.table[i], as.integer)
    i <- c(FALSE, FALSE, FALSE, TRUE, TRUE)
    # Convert columns 4 and 5 to numeric.
    assoc.table[i] <- lapply(assoc.table[i], as.numeric)
    # Add column names.
    colnames(assoc.table) <- c("RepSeq.ID", "Cluster #", "Cluster size", "MaxDist", "minSS")
    # Add OTU names
    assoc.table$OTU <- as.character(make_otu_names(assoc.table[,2], otu_format))
    # Reorder columns
    assoc.table <- assoc.table[,c(1,6,2,3,4,5)]
  }
  else {
    # Extract sequence ID, cluster number, number of sequences from each header.
    fasta.sub <- sub(">(\\w+)\\s+[a-z=,_]+(\\d+)[a-z,=]+(\\d+)", "\\1, \\2, \\3", fasta, perl=TRUE);
    assoc.table <- strsplit(fasta.sub, ", ", fixed=TRUE)
    # Convert the results from lists to a data frame.
    assoc.table <- as.data.frame(assoc.table)  
    assoc.table <- t(assoc.table)
    rownames(assoc.table) <- NULL
    assoc.table <- as.data.frame(assoc.table)
    # Convert the variable types from type factor.
    # First convert all to character
    assoc.table[] <- lapply(assoc.table[], as.character)
    # Then convert columns 2 & 3 to integer. If not done in this order, chaos.  
    i <- c(FALSE, TRUE, TRUE)
    assoc.table[i] <- lapply(assoc.table[i], as.integer)
    # Add column names.
    colnames(assoc.table) <- c("RepSeq.ID", "Cluster #", "Cluster size")
    # Add OTU names
    assoc.table$OTU <- as.character(make_otu_names(assoc.table[,2], otu_format))
    # Reorder columns
    assoc.table <- assoc.table[,c(1,4,2,3)]
  }
  return(assoc.table)
} # End function.
