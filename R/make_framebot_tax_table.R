#' @title Import utax Taxonomy File
#' @name import_utax_file
#' @aliases import_utax_file
#' @description Converts the tab-delimited output of USEARCH's utax command to a phyloseq tax_table object. The confidence level for taxonomic assignment is chosen on import.
#' @usage import_utax_file(in_file, confidence)
#' @param in_file The utaxout result from USEARCH's utax command.
#' @param confidence The confidence level to use in assigning taxonomic categories.
#' @details The default confidence level is 0.8.
#' @returns A phyloseq tax_table.
#' @author John Quensen
#' @export
#' @examples 
#' ##---- Not run. ----
#' ##-- tax.table <- import_utax_file(in_file="utax_result.txt", confidence = 0.8)
#' @keywords USEARCH

make_framebot_tax_table <- function(clstr_machine="match_cluster_machine_name.txt", taxa_machine="match_taxa_machine_names.txt") {
  clstr_machine <- read.table(file=clstr_machine, header = FALSE, stringsAsFactors = FALSE, sep="\t")
  taxa_machine <- read.table(file=taxa_machine, header = FALSE, stringsAsFactors = FALSE, sep="\t")
  colnames(taxa_machine) <- c("strain","machine_name", "nucl_length", "as_length", "%identity")
  taxa_machine[ , 1] <- paste(taxa_machine[ , 1], taxa_machine[ , 5], sep = "_%")
  colnames(clstr_machine) <- c("cluster", "machine_name")
  taxonomy_table <- merge(clstr_machine, taxa_machine, by.x="machine_name", by.y = "machine_name")
  row.names(taxonomy_table) <- taxonomy_table[,"cluster"]
  for (i in 1:nrow(taxonomy_table)) {
    taxonomy_table[i,"genus"] <- strsplit(taxonomy_table[i,"strain"],"_")[[1]][1]
    taxonomy_table[i,"species"] <- strsplit(taxonomy_table[i,"strain"],"_")[[1]][2]
  }
  taxonomy_table <- taxonomy_table[ , c("genus", "species", "strain")]
  colnames(taxonomy_table) <- c("Genus", "Species", "Strain")
  taxonomy_table <- tax_table(as.matrix(taxonomy_table), errorIfNULL = TRUE)
  return(taxonomy_table)
}