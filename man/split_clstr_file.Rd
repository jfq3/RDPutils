\name{split_clstr_file}
\alias{split_clstr_file}

\title{
Split a Cluster File
}
\description{
This function splits a cluster file containing cluster information for several distances into separate cluster files for each distance.
}
\usage{
split_clstr_file(clstr_file = "all_seq_complete.clust", file.prefix = "dist_")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clstr_file}{
The name of the cluster file for multiple distances.
}
  \item{file.prefix}{
The prefix for the individual cluster files to be written to disk.
}
}
\details{
Output files have names of the form prefix0.nn.clust where nn is the decimal distance at which sequences were clustered.  The resulting cluster files may be used as input to the function assoc_repseq_IDs_with_otus_by_clstr which requires that the input cluster file be for a single distance.  The resulting files may also be used as input to clstr2otu with some gain in speed compared to inputting a cluster file with multiple distances.
}
\value{
This function operates on files.  It returns a message when it has completed.  It is not normally assigned to a variable; if so, the variable contains only the message.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
John Quensen
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{clstr2otu}}

\code{\link{assoc_repseq_IDs_with_otus_by_clstr}}
}
\examples{

clstr.file <- system.file("extdata", "all_seq_complete.clust", package="RDPutils")
split_clstr_file(clstr_file = clstr.file, file.prefix = "dist_")

}
