import_itagger_otutab_taxa <- function(in_file) {
    # first line begins with #; need to ignore it by setting comment.char to "".
  temp <- read.table(file = in_file, comment.char = "", header = TRUE, row.names = 1, stringsAsFactors = FALSE, sep = "\t")
  rownames(temp) <- make_otu_names(as.integer(row.names(temp)))
  # Make OTU matrix
  otu <- temp[ , 1:ncol(temp)-1]
  otu <- otu_table(as.matrix(otu), taxa_are_rows = TRUE, errorIfNULL = TRUE)
  # Make tax_table
  tax <- matrix(data = NA, nrow = nrow(temp), ncol = 6)
  rownames(tax) <- rownames(temp)
  colnames(tax) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus")
  # Parse taxonomy
  for (i in 1:nrow(temp)) {
    l <- temp[i, ncol(temp)]
    # l <- gsub(":", "_", l)
    l.s <- strsplit(l, split = ";", fixed = TRUE)
    n <- length(l.s[[1]])
    for (j in 1:n){
      tax[i, j] <- l.s[[1]][j]
      
    }
  }
  # Fill NAs
  for (i in 1:nrow(tax)) {
    for (j in 1:ncol(tax)) {
      if (is.na(tax[i,j])) {
        t <- paste("uncl", tax[i, j-1], sep = "_")
        for (n in j:ncol(tax)){
          tax[i, n] <- t
        }
      }
    }
  }
  tax <- tax_table(tax, errorIfNULL = TRUE)
  # Return phyloseq object
  expt <- phyloseq(otu, tax)
  return(expt)
}