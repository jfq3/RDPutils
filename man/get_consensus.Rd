\name{get_consensus}
\alias{get_consensus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get a Consensus Taxonomy Table
}
\description{
An implementation of CONSTAX.
}
\usage{
get_consensus(tab.a, tab.b, tab.c)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab.a}{
A properly formatted taxonomy table.
}
  \item{tab.b}{
A second properly formatted taxonomy table.
}
  \item{tab.c}{
A third properly formatted taxonomy table.
}
}
\details{
The order of the three taxonomy tables does not matter. Returns a consensus taxonomy table. This is the final step in the CONSTAX workflow.
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
%% ~~ ##---- nOT RUN ----
%% ~~ ##-- get_consensus(tab.a = taxonomy.rdp, tab.b = taxonomy.sintax, tab.c = taxonomy.utax)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
