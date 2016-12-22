\name{rename_fasta}
\alias{rename_fasta}

\title{
Rename Representative Sequences
}
\description{
This function renames representative sequences from their machine names to their OTU names.
}
\usage{
rename_fasta(in_file = "names_trimmed.fasta", out_file = "renamed.fasta", rename_table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in_file}{
The name of the fasta file containing representative sequences with their machine names.
}
  \item{out_file}{
The name of the fasta file containing representative sequences with their OTU names.
}
  \item{rename_table}{
A data frame associating the machine names of representative sequences with their corresponding OTU names.
}
}
\details{
The output of the function assoc_repseq_IDs_with_otus_by_fasta or assoc_repseq_IDs_with_otus_by_clstr  can be used as the rename_table.

Before representative sequences can be renamed with this function, their IDs have to be shortened to include only their machine names with the function trim_fasta_names.
}
\value{
A fasta file with renamed representative sequences is written to disk. This function is not normally assigned to a variable.  The function returns a message that it has completed.
}
\author{
John Quensen
}
\seealso{
\code{\link{assoc_repseq_IDs_with_otus_by_clstr}}

\code{\link{assoc_repseq_IDs_with_otus_by_fasta}}

\code{\link{trim_fasta_names}}
}
\examples{
in.file <- system.file("extdata", "names_trimmed.fasta", package="RDPutils")
data(assoc.table)
rename_fasta(in_file = in.file, out_file = "renamed.fasta", rename_table = assoc.table)
}

\keyword{RDPTools}
