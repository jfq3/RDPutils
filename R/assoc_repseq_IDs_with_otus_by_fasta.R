#' @title Associate Representative Sequences with OTUs from Fasta Headers
#' @name assoc_repseq_IDs_with_otus_by_fasta
#' @aliases assoc_repseq_IDs_with_otus_by_fasta
#' @description This function parses representative sequence IDs and makes a table associating the representative sequence machine names with the OTU names as given by RDP's cluster file formatter with options "R" or "biom," or function clstr2otu in this package.
#' @usage assoc_repseq_IDs_with_otus_by_fasta(repseq_file="all_seq_complete.clust_rep_seqs.fasta", otu_format="R")
#' @param repseq_file The name of the fasta file containing representative sequences.
#' @param otu_format When equal to "R" (default) OTU names have the form "OTUxxxnn."  When equal to "biom", OTU names have the form "cluster_nn."
#' @details 
#' Representative sequences from clusters for a given distance may be obtained with either the web-based representative sequence tool currently on the rdpipeline page (http://pyro.cme.msu.edu/), or with the RDPTools' cluster function using a command similar to:
#' java -Xmx2g -jar $Clustering rep-seqs --one-rep-per-otu all_seq_complete.clust 0.03 merged_aligned.fasta
#' In these cases the fasta headers contain information on the cluster number and the size of the cluster.  This function parses this information into a table that can be used as input to function rename_fasta, which renames the representative sequences with their corresponding OTU names.
#' @returns This function returns a data frame with 4 columns: the machine name of the representative sequence, the corresponding OTU name as given by RDP's cluster file formatter with options "R" or "biom" and by function clstr2otu in this package, the cluster number, and the total number of sequences in the cluster (cluster size).
#' @author John Quensen
#' @note 
#' The representative sequence tool on the FunGene pipeline page (http://fungene.cme.msu.edu/FunGenePipeline/) returns one representative sequences per sample, a format which is not compatible with this function.
#' This function expects the representative sequence IDs to be formatted in one of these ways:
#'  >HC9DO0P01BCTC4  prefered=false,cluster=0,clustsize=2
#'  >HC9DO0P01BCTC4  cluster_id=1,size=2
#'  If the representative sequence IDs are not formatted as in these examples, or do not contain information on cluster number and size, a similar association table may be made using function assoc_repseq_IDs_with_otus_by_clstr.
#' @seealso 
#' \code{\link{assoc_repseq_IDs_with_otus_by_clstr}}, \code{\link{clstr2otu}}, \code{\link{rename_fasta}}
#' @examples 
#' repseq.file <- system.file("extdata", "all_seq_complete.clust_rep_seqs.fasta", package="RDPutils")
#' assoc.table <- assoc_repseq_IDs_with_otus_by_fasta(repseq_file=repseq.file)
#' head(assoc.table)
#' @keywords RDPTools

assoc_repseq_IDs_with_otus_by_fasta = function(repseq_file="all_seq_complete.clust_rep_seqs.fasta", otu_format="R") {
  
  # Read in fasta file as a text file.
  fasta <- read.delim(file=repseq_file, header=FALSE, sep="\t", as.is=TRUE)
  
  # Remove sequences, keep only IDs.
  keep <- seq(from=1, to=(nrow(fasta)), by=2)
  fasta <- fasta[keep,]
  
  # Count number of equal signs in the fasta header.
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
