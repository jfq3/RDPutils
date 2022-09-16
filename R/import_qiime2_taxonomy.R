#' @title Import QIIME2 Taxonomy File
#' @name import_qiime2_taxonomy
#' @aliases import_qiime2_taxonomy
#' @description Converts a tab-delimited taxonomy table made with QIIME2 to a phyloseq tax_table object.
#' @usage import_qiime2_taxonomy(in_file)
#' @param in_file 
#' @returns A phyloseq tax_table
#' @references Bolyen E, Rideout JR, et al., 2019. Reproducible, interactive, scalable and extensible microbiome data science using QIIME 2. Nature Biotechnology 37: 852–857. https://doi.org/10.1038/s41587-019-0209-9
#' @author John Quensen
#' @export

import_qiime2_taxonomy <- function(in_file) {
  temp <- read.table(file = in_file, header = TRUE,
                     stringsAsFactors = FALSE, sep = "\t")
  tax.table <- matrix(nrow = nrow(temp), ncol=8)
  for (i in 1:nrow(temp)) {
    tax.table[i, 1] <- temp[i, 1]
    tax <- split_qiime2_string(temp[i, 2])
    for (j in 2:8) {
      tax.table[i, j] <- tax[j-1]
    }
  }
  rownames(tax.table) <- tax.table[, 1]
  tax.table <- tax.table[, -1]
  colnames(tax.table) <- c("Domian", "Phylum", "Class", "order", "Family", "Genus", "Species")
  tax.table[is.na(tax.table)] <- ""
  for (i in 1:nrow(tax.table)) {
    if (tax.table[i, 7] != "") {
      tax.table[i, 7] <-  paste(tax.table[i, 6], tax.table[i, 7], sep = "_")
    }
  }
  return(tax.table)  
}

split_qiime2_string <- function(tax.str) {
  tax <- strsplit(tax.str, ";")
  tax <- tax[[1]]
  tax <- gsub("k__", "", tax)
  tax <- gsub("p__", "", tax)
  tax <- gsub("c__", "", tax)
  tax <- gsub("o__", "", tax)
  tax <- gsub("f__", "", tax)
  tax <- gsub("g__", "", tax)
  tax <- gsub("s__", "", tax)
  tax <- gsub("\n", "", tax)
  tax <- gsub(" ", "", tax)
  return(tax)
}



