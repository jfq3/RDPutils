\name{make_otu_names}
\alias{make_otu_names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make OTU Names
}
\description{
This function takes a vector of integers and makes a vector of OTU names.
}
\usage{
make_otu_names(otu_nums, otu_format="R")
}

\arguments{
  \item{otu_nums}{
	     A vector of integers.
}
  \item{otu_format}{
When equal to "R" (default) OTU names have the form "OTUxxxnn."  When equal to "biom", OTU names have the form "cluster_nn."
}
}
\details{
This function is used by the function clstr2otu to name the columns of the OTU table it returns, but make_otu_names can be used separately if desired.

OTUs may be named in either of two formats, corresponding to those output by the RDP's cluster file formatter with options "R" and "biom."  With option "R" (the default), OTU names begin with "OTU" and are padded to equal length with leading zeros, e.g. "OTU00067."  Thus they can be sorted in numerical order.  With option "biom," OTU names begin with "cluster_" and are not padded with leading zeros, e.g "cluster_67." 
}
\value{
A vector of character strings of the form "OTU_xxxnn"" or "cluster_nn."
}

\author{
John Quensen
}

\examples{
n <- c(1:10)
otu.names <- make_otu_names(n)
otu.names

}

\keyword{RDPTools}
