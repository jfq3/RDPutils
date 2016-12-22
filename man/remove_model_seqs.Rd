\name{remove_model_seqs}
\alias{remove_model_seqs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Model Sequences
}
\description{
This function removes model sequences from a combined fasta file.
}
\usage{
remove_model_seqs(in_file, out_file = in_file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in_file}{
The name of a combined fasta file with model sequences.
}
  \item{out_file}{
The name of the corresponding fasta file written to disk without model sequences.
}
}
\details{
This function operates on files.  It is not normally assigned to a variable.  By default, the input file is overwritten.  If no model sequence is found in the input file, a message to that effect is returned.

RDP aligns 16S rRNA gene sequences using the Infernal aligner with model sequences for Bacteria or Archaea.  A model sequence is introduced into each alignment.  These model sequences are required by the cluster tool and the merge alignment tool.  If a model sequence is present in the fasta file of representative sequences, it needs to be removed before the sequences can be treed with FastTree; this function provides the means to do so.
}
\value{
The function returns a message that it has completed.
}

\author{
John Quensen
}

\examples{

in.file <- system.file("extdata", "all_seq_complete.clust_rep_seqs.fasta", package="RDPutils")
remove_model_seqs(in_file=in.file, out_file = file2tree.fasta)

}

\keyword{RDPTools}
