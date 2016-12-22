\name{import_usearch_biome_file}
\alias{import_usearch_biome_file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import a USEARCH biome File
}
\description{
Converts a biome file produced with USEARCH's usearch_global command into a phyloseq object.
}
\usage{
import_usearch_biome_file(in_file, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in_file}{
A biome file produced with USEARCH's usearch_global command.
}
}
\details{
This function will import the OTU table contained in the USEARCH biome file, and the taxonomy table if it is present. The function will also accept arguments for sample data table, reference sequences, and a phyloseq class taxonomy table from another source, if one is not present in the biome file.
}
\value{
An experiment level phyloseq object (i.e. with at least an otu_table).
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
John Quensen
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ USEARCH }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
