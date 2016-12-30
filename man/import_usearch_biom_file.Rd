\name{import_usearch_biom_file}
\alias{import_usearch_biom_file}

\title{
Import a USEARCH biom File
}
\description{
Converts a biom file produced with USEARCH's usearch_global command into a phyloseq object.
}
\usage{
import_usearch_biom_file(in_file, ...)
}

\arguments{
  \item{in_file}{
A biom file produced with USEARCH's usearch_global command.
}
  \item{...}{
Other objects accepted by phyloseq, e.g. sample data table, representative sequences.
}
}
\details{
This function will import the OTU table contained in the USEARCH biom file, and the taxonomy table if it is present. The function will also accept arguments for sample data table, reference sequences, and a phyloseq class taxonomy table from another source, if one is not present in the biom file.
}
\value{
An experiment level phyloseq object (i.e. with at least an otu_table).
}

\author{
John Quensen
}

\examples{
##---- Not run. ----
##-- expt <- import_usearach_biome(infile = "otutab_taxa_03.json")

## The function is currently defined as
import_usearch_biome_file <- function(in_file) {
  parse_taxonomy_usearch <- function (char.vec){
    parse_taxonomy_default(strsplit(char.vec, ",", TRUE)[[1]])
  }
  expt <- import_biom(in_file, parseFunction = parse_taxonomy_usearch, ...)
  return(expt)
}

}

\keyword{ USEARCH }

