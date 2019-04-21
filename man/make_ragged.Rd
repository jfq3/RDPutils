\name{make_ragged}
\alias{make_ragged}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make ragged
}
\description{
Converts a taxonomy table to ragged format.
}
\usage{
make_ragged(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A taxonomy table of class matrix or phyloseq's tax_table.
}
}
\details{
Converts a taxonomy table, either matrix or phyloseq tax_table class, to ragged format by replacing all taxa beginning with "uncl" with an empty string. Necessary to reformat taxonomy tables as part of the CONSTAX workflow.
}
\value{
A taxonomy table of class matrix.
}
\references{
Gdanetz, K., Benucci, G.M.N., Pol, N.V., and Bonito, G. (2017). CONSTAX: a tool for improved taxonomic resolution of environmental fungal ITS sequences. Bmc Bioinformatics 18. doi: 10.1186/s12859-017-1952-x.
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
%% ~~ ##---- Should be DIRECTLY executable !! ----
%% ~~ ##-- ==>  Define data, use random,
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
