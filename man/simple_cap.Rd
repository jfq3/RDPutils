\name{simple_cap}
\alias{simple_cap}

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
}

\note{
This function is given as an example in the documentation for the base function toupper.
}

\seealso{
make_tax_table
}
\examples{
x <- "The quick brown fox jumped over the lazy dog."
simple_cap(x)
}

