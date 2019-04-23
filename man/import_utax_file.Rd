\name{import_utax_file}
\alias{import_utax_file}

\title{
Import utax Taxonomy File
}
\description{
Converts the tab-delimited output of USEARCH's utax command to a phyloseq tax_table object. The confidence level for taxonomic assignment is chosen on import.
}
\usage{
import_utax_file(in_file, ragged = FALSE, confidence)
}

\arguments{
  \item{in_file}{
The utaxout result from USEARCH's utax command.
}
  \item{ragged}{
A logical. If FALSE (the default), fills in unidentified taxa with "unlc_" prependend to identified higher order taxa.
}
  \item{confidence}{
The confidence level to use in assigning taxonomic categories.
}
}
\details{
The default confidence level is 0.8.
}
\value{
A phyloseq tax_table.
}

\author{
John Quensen
}

\examples{
##---- Not run. ----
##-- tax.table <- import_utax_file(in_file="utax_result.txt", confidence = 0.8)

}

\keyword{ USEARCH }

