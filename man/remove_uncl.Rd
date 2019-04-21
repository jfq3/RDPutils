\name{remove_uncl}
\alias{remove_uncl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
remove_uncl
}
\description{
Given a string beginning with "uncl," returns an empty string.
}
\usage{
remove_uncl(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A string at least 4 characters long.
}
}
\details{
Used by the function make_ragged, which removes all taxa beginning with "uncl" from a taxonomy table (either a matrix or phyloseq tax_table). Make_ragged is necessary to reformat a taxonomy table for input to the get_consensus function.
}
\value{
An empty string.
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
make_ragged  
get_consensus
}
\examples{
%% ~~##---- Should be DIRECTLY executable !! ----
%% ~~##-- ==>  Define data, use random,
%% ~~##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
