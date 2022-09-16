#' @title Import a USEARCH biom File
#' @name import_usearch_biom_file
#' @aliases import_usearch_biom_file
#' @description Converts a biom file produced with USEARCH's usearch_global command into a phyloseq object.
#' @usage import_usearch_biom_file(in_file, ...)
#' @param in_file A biom file produced with USEARCH's usearch_global command.
#' @param ... Other objects accepted by phyloseq, e.g. sample data table, representative sequences.
#' @details This function will import the OTU table contained in the USEARCH biom file, and the taxonomy table if it is present. The function will also accept arguments for sample data table, reference sequences, and a phyloseq class taxonomy table from another source, if one is not present in the biom file.
#' @returns An experiment level phyloseq object (i.e. with at least an otu_table).
#' @author John Quesen
#' @export
#' @examples 
#' ##---- Not run. ----
#' ##-- expt <- import_usearch_biome(infile = "otutab_taxa_03.json")

import_usearch_biom_file <- function(in_file, ...) {
  parse_taxonomy_usearch <- function (char.vec){
    parse_taxonomy_default(strsplit(char.vec, ",", TRUE)[[1]])
  }
  expt <- import_biom(in_file, parseFunction = parse_taxonomy_usearch, ...)
  return(expt)
}

