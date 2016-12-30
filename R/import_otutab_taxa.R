import_otutab_taxa <- function(in_file) {
  # first line begins with #; need to ignore it by setting comment.char to "".
  temp <- read.table(file = in_file, comment.char = "", header = TRUE, row.names = 1, stringsAsFactors = FALSE, sep = "\t")
  # Test if taxonomy field is present.
  if ("taxonomy" %in% colnames(temp)) {
    # Make OTU matrix
    otu <- temp[ , 1:ncol(temp)-1]
    # Determine number of ranks.
    n.ranks <- vector("integer")
    for (i in 1:nrow(temp)) {
      n.ranks[i] <- count_char_occurrences(":", temp[i, ncol(otu)+1])
    }
    # Get longest taxonomy line.
    max.taxa <- temp[which.max(n.ranks),ncol(otu)+1]
    n.ranks <- max(n.ranks)
    # Generate rank names
    colon.pos <- lapply(strsplit(max.taxa, ''), function(x) which(x == ':'))
    colon.pos <- colon.pos[[1]] - 1
    rank.code <- vector("character")
    for (i in 1:length(colon.pos)) {
      s <- colon.pos[i]
      rank.code[i] <- substring(max.taxa, s, s + 1)
    }
    rank.code <- sub("d:", "Domain", rank.code)
    rank.code <- sub("k:", "Kingdom", rank.code)
    rank.code <- sub("p:", "Phylum", rank.code)
    rank.code <- sub("c:", "Class", rank.code)
    rank.code <- sub("o:", "Order", rank.code)
    rank.code <- sub("f:", "Family", rank.code)
    rank.code <- sub("g:", "Genus", rank.code)
    rank.code <- sub("s:", "Species", rank.code)

    # Make tax_table
    tax <- matrix(data = NA, nrow = nrow(temp), ncol = n.ranks)
    rownames(tax) <- rownames(temp)
    colnames(tax) <- rank.code
    # Parse taxonomy
    for (i in 1:nrow(temp)) {
      l <- temp[i, ncol(temp)]
      l <- gsub(":", "_", l)
      l.s <- strsplit(l, split = ",", fixed = TRUE)
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
    otu <- otu_table(as.matrix(otu), taxa_are_rows = TRUE, errorIfNULL = TRUE)
    tax <- tax_table(tax, errorIfNULL = TRUE)
    expt <- phyloseq(otu, tax)
  } else {
    expt <- otu_table(temp, taxa_are_rows = TRUE, errorIfNULL = TRUE)
  }
  
  # Return phyloseq object
  return(expt)
}