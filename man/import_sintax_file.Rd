\name{import_sintax_file}
\alias{import_sintax_file}

\title{
Import sintax Taxonomy File
}
\description{
Converts the tab-delimited output of USEARCH's (version 9+) sintax command to a phyloseq tax_table object. The confidence level for taxonomic assignment is chosen on import.
}
\usage{
import_sintax_file(in_file, ragged = FALSE, confidence = 0.8)
}

\arguments{
  \item{in_file}{
    A sintax file.
}
  \item{ragged}{
    A logical. If FALSE (the default), fills in unidentified taxa with "unlc_" prependend to identified higher order taxa.
}
  \item{confidence}{
    The confidence level to use in assigning taxonomic categories.
}
}
\details{
The sintax algorithm is new to USEARCH 9. It does not require training and still calculates the confidence with which taxonomic assignments are made. It does this by a method purported to be more accurate than the RDP classifier method. This function allows choice of the confidence level (0.8 by default) at the time the file is imported as a phyloseq tax_table.
}
\value{
A phyloseq tax_table.
}
\references{
Edgar, R.C. (2016), SINTAX, a simple non-Bayesian taxonomy classifier for 16S and ITS sequences, http://dx.doi.org/10.1101/074161.
}
\author{
John Quensen
}

\examples{
##---- Not run. ----
##-- import_sintax_file(in_file="sintax_result.txt", confidence = 0.8)

}

\keyword{ USEARCH }
