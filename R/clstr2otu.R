clstr2otu <-
function(clstr_file="all_seq_complete.clust", dist=0.03, OutFile=FALSE, file_name="dist_03.clust", otu_format="R") {
  # Read in cluster file.
  clstr <- read.delim(file=clstr_file, header=FALSE, as.is=TRUE)
  clstr.header <- clstr[1:2, ] # Keep first 2 lines to print with subsetted cluster file.
 
  # Make index of row numbers for each distance in cluster file.
  line.no <- which(clstr[ , 1]=="distance cutoff:")
  distance <- clstr[line.no, 2]
  dist.index <- data.frame(line.no, distance)

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
    write.table(cluster, file=file_name, sep="\t", col.names=FALSE, row.names=FALSE)
    rm(cluster)
  }
  
  #   Remove first 2 lines of clstr; also 4th column is not needed.
  clstr <- clstr[-c(1:2), -4]

  # Use dcast function from package reshape2 to reformat clstr as a dataframe.
  clstr <- clstr[,c(2,1,3)]
  colnames(clstr) <- c("sample", "clstr_no", "value")
  i <- as.logical(c(0,1,1))
  clstr[i] <- lapply(clstr[i], as.integer)
  otu <- dcast(clstr, sample ~ clstr_no, fun.aggregate=NULL, fill=0)
  rownames(otu) <- otu[,1]
  otu <- otu[ , -1]

# Make otu names comparable to RDP's R-formatter tool.
  new.col.names <- make_otu_names(as.numeric(colnames(otu)), otu_format)
  colnames(otu) <- new.col.names
  
  # Strip common prefix and suffix from sample names (if present).
  temp <- rownames(otu)
  temp <- gsub("nc_", "", temp)
  temp <- gsub("_trimmed", "", temp)
  temp <- gsub("aligned_", "", temp)
  temp <- gsub(".fasta","", temp)
  temp <- gsub(".fastq","", temp)
  rownames(otu) <- temp
  otu <- otu[order(rownames(otu)), ]
  otu <- otu[ , order(colnames(otu))]
  otu <- as.data.frame(otu)
  return(otu)
}
