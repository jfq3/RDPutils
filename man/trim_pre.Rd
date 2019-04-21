\name{trim_pre}
\alias{trim_pre}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
trim_pre
}
\description{
Removes the first two characters from string.
}
\usage{
trim_pre(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A string more than two characters long.
}
}
\details{
Used to reformat UTAX and SINTAX taxonomy tables before submitting them to the get_consensus function. Part of the CONSTAX work flow. Used after the remove_uncl function.
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
remove_uncl
get_consensus
}
\examples{
trim_pre("p_Basidiomycota")
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
