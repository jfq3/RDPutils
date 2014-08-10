\name{make_otu_names}
\alias{make_otu_names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make OTU Names
}
\description{
This function takes a vector of integers and makes OTU names of the form OTU_xxxnn where nn is the corresponding integer.  The names are padded with the x's so that they are all the same length and can be sorted into numerical order if desired.
}
\usage{
make_otu_names(otu_nums)
}

\arguments{
  \item{otu_nums}{
	     A vector of integers.
}
}
\details{
This function is used by the function clstr2otu to name the columns of the OTU table it returns, but make_otu_names can be used separately if desired.
}
\value{
A vector of character strings of the form OTU_xxxnn.
}

\author{
John Quensen
}

\examples{
n <- c(1:10)
otu.names <- make_otu_names(n)
otu.names

}

