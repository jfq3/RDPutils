\name{trim_fasta_names}
\alias{trim_fasta_names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Trim Representative Sequence Names
}
\description{
This function trims the fasta IDs of the representative sequences to include only the machine name.
}
\usage{
trim_fasta_names(repseq_file, trimmed_names, strip = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repseq_file}{
The name of the combined fasta file of representative sequences.
}
  \item{trimmed_names}{
The name of the modified fasta file written to disk.
}
  \item{strip}{
A logical.  If TRUE, then the output fasta file is unaligned and any model sequences are removed.  The default is FALSE.
}
}
\details{
This function is a necessary prerequisite to renaming representative sequences with their corresponding OTU names.

Unaligning the sequences results in a smaller file size and is acceptable if the sequences are to be classified only.  It may be desirable if using the web-based classifier because the smaller file size makes for shorter upload time.  Unaligning must not be done if the sequences are to be treed.
}
\value{
This function operates on files.  It returns a message that it has completed.  It is not normally assigned to a variable; if so, the variable contains only the message.
}
\author{
John Quensen
}
\examples{

repseq.file <- system.file("extdata", "all_seq_complete.clust_rep_seqs.fasta", package="RDPutils")
trim_fasta_names(repseq_file = repseq.file, trimmed_names = "names_trimmed.fasta", strip = FALSE)

}

\keyword{RDPTools}

