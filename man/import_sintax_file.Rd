\name{import_sintax_file}
\alias{import_sintax_file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import sintax Taxonomy File
}
\description{
Converts the tab-delimited output of USEARCH's (version 9+) sintax command to a phyloseq tax_table object. The confidence level for taxonomic assignment is chosen on import.
}
\usage{
import_sintax_file(in_file, confidence = 0.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in_file}{
    A sintax file.
}
  \item{confidence}{
    The confidence level to use in assigning taxonomic categories
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
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Not run. ----
##-- import_sintax_file(in_file="sintax_result.txt", confidence = 0.8)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ USEARCH }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
