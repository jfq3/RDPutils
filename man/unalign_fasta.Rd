\name{unalign_fasta}
\alias{unalign_fasta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Unalign a Fasta File
}
\description{
This function unaligns a combined fasta file and removes any model sequences present.  The modified fasta file is written to disk.
}
\usage{
unalign_fasta(in_file, out_file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in_file}{
The name of the input fasta file.  The file is read from disk.
}
  \item{out_file}{
The name of the modified fasta file written to disk.
}
}
\details{
This function can be used to unalign renamed representative sequences in order to reduce the file size before uploading the file to the web-based RDP classifier.
}
\value{
This function operates on files.  It returns a message that it has completed.  It is not normally assigned to a variable; if so, the variable contains only the message.
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

in.file <- system.file("extdata", "merged_aligned.fasta", package="RDPutils")
unalign_fasta(in_file=in.file, out_file="unaligned.fasta")

}

