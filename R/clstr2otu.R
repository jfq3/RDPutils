#' @title Create an OTU Table from a Cluster File
#' @name clstr2otu
#' @aliases clstr2otu
#' @description Parses a cluster file into an otu table with samples as rows and OTUs as columns.  The input cluster file can contain data for more than one distance, or for a single distance.  This function provides the same result as RDP's R-formatter, but for only one distance.  If OutFile=TRUE, then the function also writes a cluster file for distance = dist to file.name.
#' @usage clstr2otu(clstr_file = "all_seq_complete.clust", dist = 0.03, OutFile = FALSE, file_name = "dist_03.clust", otu_format="R")
#' @param clstr_file The output file given by RDP's cluster tool.  This file usually contains cluster information for more than one distance.
#' @param dist dist is the maximum distance between sequences in the same cluster.  An OTU table will be created for the distance given here, which must be present in the cluster file.
#' @param OutFile A logical.  If OutFile is TRUE, then a cluster file is written to disk for the single distance given by dist.  Default = FALSE.
#' @param file_name The name of the cluster file written to disk if OutFile is TRUE.  Otherwise ignored.
#' @param otu_format When equal to "R" (default) OTU names have the form "OTUxxxnn."  When equal to "biom", OTU names have the form "cluster_nn."
#' @details 
#' This function can take several minutes, depending on the number of OTUs and samples.  The cluster file for a single distance output by the function can be used to associate the OTU names in the OTU table produced with the representative sequence machine names.
#' The sample names are shortened by removing common prefixes and suffixes introduced by RDP's tools.  These include "nc_", "aligned_", "_trimmed", ".fasta" and ".fastq."
#' The two OTU formats correspond to those output by the RDP's cluster file formatter, which has the options "R"" and "biom."  With option "R," OTU names begin with "OTU" and are padded to equal length with leading zeros, e.g. "OTU00067."  Thus they can be sorted in numerical order.  With option "biom," OTU names begin with "cluster_" and are not padded with leading zeros, e.g "cluster_67." 
#' @export
#' @returns Returns a numerical data frame, or OTU table, where samples are rows and OTUs are columns.  This is the convention used by vegan.  To import the result into phyloseq, it should first be transposed and converted to class matrix.
#' @references 
#' RDP web-based clustering services and tutorials on clustering sequence data are available at: http://rdp.cme.msu.edu/
#' RDPTools is an open source package available from https://github.com/rdpstaff; it includes a command line function for clustering, necessary for data sets too large to process with the web-based clustering tool.
#' Cole, J. R., Q. Wang, J. A. Fish, B. Chai, D. M. McGarrell, Y. Sun, C. T. Brown, A. Porras-Alfaro, C. R. Kuske, and J. M. Tiedje. 2014. Ribosomal Database Project: data and tools for high throughput rRNA analysis Nucl. Acids Res. 41(Database issue):D633-D642; doi: 10.1093/nar/gkt1244 [PMID: 24288368]
#' @author John Quensen
#' @examples 
#' clstr.file <- system.file("extdata", "all_seq_complete.clust", package="RDPutils")
#' otu <- clstr2otu(clstr_file=clstr.file, dist=0.03, OutFile=TRUE, file_name= "dist_03.clust")
#' otu[ , 1:5]
#' @keywords RDPTools

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
