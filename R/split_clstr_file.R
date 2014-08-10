split_clstr_file <-
function(clstr_file="all_seq_complete.clust", file.prefix="dist_") {
  # Read in cluster file.
  clust <- read.delim(file=clstr_file, header=FALSE, as.is=TRUE)
  cluster.header <- clust[1:2, ] # Keep first 2 lines to print with subsetted cluster file.
  
  # Make index of row numbers for each distance in cluster file.
  dist.index <- NULL
  for (n in seq(along=clust[,1])) {
    if (clust[n,1] == "distance cutoff:") {
      temp <- c(n, clust[n,2])
      dist.index <- rbind(dist.index, temp)
    }
  }
  rownames(dist.index) <- NULL
  dist.index[] <- lapply(dist.index, as.numeric)
  dim(dist.index) <- c(length(dist.index)/2, 2)
  dist.index <- as.data.frame(dist.index)
  colnames(dist.index) <- c("line.no", "distance")
  
  # Get first and last row numbers for chosen distance.
 for (n in 1:nrow(dist.index)) {
    if (n==nrow(dist.index)) {
      first.row <- as.numeric(dist.index[n,1])
      last.row <- nrow(clust)
    } else {
      first.row <- as.numeric(dist.index[n,1])
      last.row <- as.numeric(dist.index[n+1,1])-1
    }
    # Subset lines in cluster file to chosen distance
    clus <- clust[first.row:last.row, ]
    # Write file
    file.name <- paste(file.prefix,as.character(dist.index[n,2]),".clust", sep="")
    cluster <- rbind(cluster.header, clus)
    write.table(cluster, file=file.name, sep="\t", col.names=FALSE, row.names=FALSE)
  }
message <- paste("Wrote ",nrow(dist.index), " cluster files to disk.", sep="")
return(message)
}
