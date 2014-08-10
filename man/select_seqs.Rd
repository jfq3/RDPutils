\name{select_seqs}
\alias{select_seqs}

\title{
Select Fasta Sequences
}
\description{
This function subsets a combined fasta file.
}
\usage{
select_seqs(in_file, select_list, out_file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in_file}{
The name of a combined fasta file to be read from disk.
}
  \item{select_list}{
A vector of the names of the individual fasta files to be kept.
}
  \item{out_file}{
The name of the modified combined fasta file to be written to disk.
}
}
\details{
This function can be used to select a subset of the renamed representative sequences corresponding to OTUs containing at least n sequences.  See the example section below.

Reducing the number of representative sequences in this manner makes several subsequent steps go faster: classifying the representative sequences, making a phyloseq tax_table, and treeing the representative sequences.
}
\value{
This function operates on disk files.  It is not normally assigned to a variable.  It returns a message that it has completed.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
renamed.fasta <- system.file("extdata", "renamed.fasta", package="RDPutils")
data(otu)
otu <- otu[ , colSums(otu)>=5]
select.list <- colnames(otu)
select_seqs(in_file=renamed.fasta, select_list=select.list, out_file="subset.renamed.repseqs.fasta")

}
