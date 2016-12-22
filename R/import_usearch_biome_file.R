import_usearch_biome_file <- function(in_file, ...) {
  parse_taxonomy_usearch <- function (char.vec){
    parse_taxonomy_default(strsplit(char.vec, ",", TRUE)[[1]])
  }
  expt <- import_biom(in_file, parseFunction = parse_taxonomy_usearch, ...)
  return(expt)
}

