\name{import_utax_file}
\alias{import_utax_file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import utax Taxonomy File
}
\description{
Converts the tab-delimited output of USEARCH's utax command to a phyloseq tax_table object. The confidence level for taxonomic assignment is chosen on import.
}
\usage{
import_utax_file(in_file, confidence)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in_file}{
The utaxout result from USEARCH's utax command.
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
##-- tax.table <- import_utax_file(in_file="utax_result.txt", confidence = 0.8)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ USEARCH }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
