\name{import_constax}
\alias{import_constax}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import a CONSTAX Taxonomy File
}
\description{
Imports a CONSTAX consensus taxonomy file.
}
\usage{
import_constax(in.file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in.file}{
A consensus_taxonomy.txt file. 
}
}
\details{
Imports a consensus_taxonomy.txt file produced by the CONSTAX python script CombineTaxonomy.py.
}
\value{
A taxonomy table of class matrix.
}
\references{
Gdanetz, K., Benucci, G.M.N., Pol, N.V., and Bonito, G. (2017). CONSTAX: a tool for improved taxonomic resolution of environmental fungal ITS sequences. Bmc Bioinformatics 18. doi: 10.1186/s12859-017-1952-x.
https://github.com/jfq3/CONSTAX
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
%% ~~ ##---- Not run. ----
%% ~~ ##--import_constax(in.file="consensus_taxonomy.txt")
}
