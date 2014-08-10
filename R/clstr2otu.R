clstr2otu <-
function(clstr_file="all_seq_complete.clust", dist=0.03, OutFile=FALSE, file.name="dist_03.clust") {
  # Read in cluster file.
  clstr <- read.delim(file=clstr_file, header=FALSE, as.is=TRUE)
  clstr.header <- clstr[1:2, ] # Keep first 2 lines to print with subsetted cluster file.
  
  # Make index of row numbers for each distance in cluster file.
  dist.index <- NULL
  for (n in seq(along=clstr[,1])) {
    if (clstr[n,1] == "distance cutoff:") {
      temp <- c(n, clstr[n,2])
      dist.index <- rbind(dist.index, temp)
    }
  }
  rownames(dist.index) <- NULL
  dist.index[] <- lapply(dist.index, as.numeric)
  dim(dist.index) <- c(length(dist.index)/2, 2)
  dist.index <- as.data.frame(dist.index)
  colnames(dist.index) <- c("line.no", "distance")
  
  # Get first and last row numbers for chosen distance.
  # Return error message if requested distance is not present.
  # Check for maximum distance in dist.index
  # If requested distance is maximum distance, last.row = last line in cluster file.
  if (dist %in% dist.index[,2]) {
    n <- which(dist.index[,2]==dist)
    if (n==nrow(dist.index)) {
      first.row <- as.numeric(dist.index[n,1])
      last.row <- nrow(clstr)
    } else {
      first.row <- as.numeric(dist.index[n,1])
      last.row <- as.numeric(dist.index[n+1,1])-1
    }
  } else {
    message <- paste("Distance ",dist," not in cluster file.")
    return(message)
  }
  
  # Subset lines in cluster file to chosen distance
  clstr <- clstr[first.row:last.row, ]
  
  # If enabled, write this subsetted cluster file to disk.
  if (OutFile==TRUE) {
    cluster <- rbind(clstr.header, clstr)
    write.table(cluster, file=file.name, sep="\t", col.names=FALSE, row.names=FALSE)
    rm(cluster)
  }
  
  #   Remove first 2 lines of clstr; also 4th column is not needed.
  clstr <- clstr[-c(1:2), -4]
  
  # Get number of clusters
  no.clusters <- length(unique(clstr[ ,1]))
  
  # Get number of samples
  no.samples <- length(unique(clstr[ ,2]))
  
  # Make empty OTU table; samples are rows.
  otu <- matrix(0, no.samples, no.clusters)
  row.names <- unique(clstr[ ,2])
  col.names <- unique(clstr[ ,1])
  rownames(otu) <- row.names
  colnames(otu) <- col.names
  
  # Sort counts into the OTU matrix
  for (k in 1:nrow(clstr)){
    i <- which(row.names==clstr[k,2])
    j <- which(col.names==clstr[k,1])
    otu[i,j] <- clstr[k,3]
  }
  

# Make otu names comparable to RDP's R-formatter tool.
  new.col.names <- make_otu_names(as.numeric(colnames(otu)))
  colnames(otu) <- new.col.names
  
  # Strip common prefix and suffix from sample names (if present).
  temp <- row.names
  temp <- gsub("nc_", "", temp)
  temp <- gsub("_trimmed", "", temp)
  temp <- gsub("aligned_", "", temp)
  temp <- gsub(".fasta","", temp)
  rownames(otu) <- temp
  otu <- otu[order(rownames(otu)), ]
  otu <- otu[ , order(colnames(otu))]
  otu <- as.data.frame(otu)
  return(otu)
}
