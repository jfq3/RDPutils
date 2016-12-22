\name{hier2phyloseq}
\alias{hier2phyloseq}

\title{
Classification File Converter
}
\description{
Converts RDP's hierarchical classification file to a phyloseq object.
}
\usage{
hier2phyloseq(hier_file = "test_hier.txt")
}

\arguments{
  \item{hier_file}{
Tab-delimited text file from RDP's command line classifier with option filterbyconf.
}
}
\details{
RDP's classifier generates two types of output.  The detail format gives the classification of each sequence input.  The hierachical format gives the number of sequences in each taxon.  If the classifier is given a number of samples at the same time, the hierachical format can be filtered in Excel to give an otu table for a given rank, with taxa as rows and samples as columns.

The command line classifier takes arguments for confidence level and output format.  For example, 28S fungal sequences can be classified with the commands:

cd c:\\test

java -Xmx1g -jar /path_to_classfier.jar/classifier.jar classify -g fungallsu -c 0.5 -f filterbyconf -o test_classified.txt -h test_hier.txt *.fasta

All sample fasta files in directory c:\\test are classified to a rank with confidence of 0.5 or more.  By setting the format to filterbyconf, ranks not identified with a confidence of at least 0.5 are empty.

hier2phyloseq converts the hierachical result, file test_hier.txt in the example above, to a phyloseq object with otu_table and tax_table.  Empty ranks are filled as unclassifed higher rank.  For example, the family and genus assigned to sequences classified with confidence greater than 0.5 only as far as order "Pleosporales"" would be "unclass_Pleosporales."

The sample names are shortened by removing common prefixes and suffixes introduced by RDP's tools.  These include "nc_", "aligned_", "_trimmed", ".fasta" and ".fastq."

This function is not appropriate for processing a single sample.  To make a tax_table phyloseq object, use function "make_tax_table."

}
\value{
Returns a phyloseq object with otu_table and tax_table.
}

\author{
John Quensen
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{make_tax_table}}
}
\examples{

	hier.file <- system.file("extdata", "test_hier.txt", package="RDPutils")
	expt <- hier2phyloseq(hier_file=hier.file)
	expt

}

\keyword{RDPTools}
