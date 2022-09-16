#' @title Create a FrameBot Taxonomy Table
#' @name make_framebot_tax_table
#' @aliases make_framebot_tax_table
#' @description Create a taxonomy table from FunGene Pipeline output.
#' @usage make_framebot_tax_table(clstr_machine, taxa_machine)
#' @param clstr_machine match_cluster_machine_name.txt from my FunGene Pipeline script
#' @param taxa_machine match_taxa_machine_names.txt from my FunGene Pipeline script
#' @details My FunGene pipeline script (see john-quensen.com) produces one file matching representative sequence machine names with cluster numbers and a second matching representative sequence machine names with taxa names found by FrameBot. This function parses and combines the two files to produce a phyloseq tax_table. Ranks are Genus, Species, and Strain. Classification is the closest match to sequence in the FrameBot reference database. The percent identity to the closest match is appended to the name of the closest match (strain level in the tax_table).
#' @note See the workshop page Command Line FunGene Pipeline at john-quensen.com for the FunGene Pipeline script.
#' @returns A phyloseq tax_table.
#' @author John Quensen
#' @export
#' @examples 
#' match.clst <- system.file("extdata", "match_cluster_machine_name.txt", package="RDPutils")
#' match.taxa <- system.file("extdata", "match_taxa_machine_names.txt", package="RDPutils")
#' my_taxa <- make_framebot_tax_table(clstr_machine=match.clst, taxa_machine=match.taxa)

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