# Function to convert classifier hierarchy file to phyloseq object
# with OTU and taxa tables.
hier2phyloseq = function(hier_file="test_hier.txt") {
  
  # Read classifier's hier.txt file.
  taxa <- read.table(file=hier_file, header=FALSE, sep="\t", stringsAsFactors=FALSE, fill=TRUE)
  colnames(taxa) <- taxa[1,]
  taxa <- taxa[-1,]

  all.ranks <- unique(taxa$rank)
  possible.ranks <- c("rootrank", "kingdom", "domain","phylum","class", "order", "family", "genus", "species", "Rootrank", "Kingdom", "Domain","Phylum","Class", "Order", "Family", "Genus", "Species")
  actual.ranks <- intersect(all.ranks, possible.ranks)
  n.ranks <- length(actual.ranks)
  last.rank <- actual.ranks[n.ranks]
  
  # Subset rows of taxa to those with lowest rank or empty
  taxa <- taxa[taxa$rank %in% c(last.rank, ""), ]
  
  # Add taxa names of format OTU_xxxx.
  rownames(taxa) <-make_otu_names(seq_len(nrow(taxa)))
  
  # Make empty classification table.
  my.table <- matrix("", n.ranks, nrow=nrow(taxa))
  colnames(my.table) <- actual.ranks
  # rownames(my.table) <- paste("ID_", as.character(taxa[,1]), sep="")
  rownames(my.table) <- rownames(taxa)
    
  # Fill in classification table with rank names from taxa.
  rank.names <- colnames(my.table)
  
  for (k in 1:ncol(my.table)){
    for (i in 1:nrow(my.table)) {
      my.line <- unlist(strsplit(taxa[i,2],";"))
      if (rank.names[k] %in% my.line) {
        j <- which(my.line==rank.names[k])
        level.name <- my.line[j-1]
        my.table[i,k] <- level.name
      }
    }
  }
  
  # Fill in empty cells in classfication table with"unclass_higher rank".
  for (i in 1:nrow(my.table)) {
    for (j in 2:ncol(my.table)) {
      if (my.table[i,j]=="") {
        if (substr(my.table[i,j-1],1,7)=="unclass") {my.table[i,j]<-my.table[i,j-1]}
        else {my.table[i,j]<- paste("unclass_", my.table[i,j-1], sep="")}
      }
    }
  }
  
  # Make sure rank names are capitalized.
  rank.names <- sapply(rank.names, simple_cap)
  colnames(my.table) <- rank.names

  # Make phyloseq object containing OTU table and taxonomy table
  otu <- taxa[ , -c(1:4)]
  # rownames(otu) <- paste("ID_", as.character(taxa[,1]), sep="")
  rownames(otu) <- rownames(taxa)
  otu[] <- lapply(otu, as.numeric)
  
  # Make common changes to sample names.
  sam.names <- colnames(otu)
  sam.names <- gsub("nc_", "", sam.names, fixed=TRUE)
  sam.names <- gsub("_trimmed", "", sam.names, fixed=TRUE)
  sam.names <- gsub("aligned_", "", sam.names, fixed=TRUE)
  sam.names <- gsub(".fasta","", sam.names, fixed=TRUE)
  sam.names <- gsub(".fastq","", sam.names, fixed=TRUE)
  sam.names <- gsub("_",".", sam.names, fixed=TRUE)
  colnames(otu) <- sam.names

  my.tax <- tax_table(my.table, errorIfNULL=TRUE)
  my.otu <- otu_table(as.matrix(otu), taxa_are_rows=TRUE, errorIfNULL=TRUE)
  my.expt <- phyloseq(my.otu, my.tax)
  
  return(my.expt)
  
}
