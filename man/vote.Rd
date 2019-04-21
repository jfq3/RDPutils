\name{vote}
\alias{vote}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Vote
}
\description{
Selects the most abundant string from a vector of strings.
}
\usage{
vote(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector of strings.
}
}
\details{
Used by the function get_consensus to select the most abundant taxon from three classification tables, but can be sued to select the most abundant string from any vector of strings.
}
\value{
A character string.
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
get_consensus
}
\examples{
x = c("a", "a", "b")
vote(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ constaxr }

