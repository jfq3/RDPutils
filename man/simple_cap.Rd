\name{simple_cap}
\alias{simple_cap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
simple_cap
}
\description{
Capitalizes the first letter of each word in a string.
}
\usage{
simple_cap(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A text string containing one or more words.
}
}
\details{
This function is used by the function make_tax_table to make sure that the rank names begin with capital letters. Makes for prettier plotting when rank names are used as legend headers, etc.
}
\value{
 A text vector with first letter of each word capitalized.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%%
}
\author{
%%  ~~who you are~~
}
\note{
This function is given as an example in the documentation for the base function toupper.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
make_tax_table
}
\examples{
x <- "The quick brown fox jumped over the lazy dog."
simple_cap(x)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
