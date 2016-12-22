\name{make_tax_table}
\alias{make_tax_table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make a Phyloseq tax_table from Classifier Result
}
\description{
This function creates a phyloseq tax_table from the RDP classifier result (detail format) for renamed representative sequences.
}
\usage{
make_tax_table(in_file="fixrank_classified.txt", confidence=0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in_file}{
File name for RDP classifier result in detail format.
}
  \item{confidence}{
Confidence level for construction of tax_table.
}
}
\details{
The input file is the file designated by the "-o" argument to the command line RDP classifer.  The format given by "-f" must be "fixrank."  The confidence level "-c" in the classifier command is unimportant; instead, the tax_table will be constructed according to the confidence argument to make_tax_table.  This confidence argument is similar in concept to "-c" which actually affects only the hierarchical (-h) and fillterbyconf (-f) output of the classifier.  For example, after classifying the file "renamed_repseqs.fasta" (16S rRNA DNA sequences) with the commands:

cd <to directory with fasta file to be classified>

java -Xmx1g -jar /path_to_classfier.jar/classifier.jar classify -g 16srrna -f fixrank -o test_classify.txt renamed_repseqs.fasta

use "test_classify.txt" as the in_file to make_tax_table.

Ranks classified with confidence less than that specified are filled as unclassifed higher rank.  For example, the family and genus assigned to OTUs classified with confidence greater than 0.5 only as far as order "Actinomycetales" would be "unclass_Actinomycetales."
}
\value{
A phyloseq tax_table object.
}
\author{
John Quensen
}
\examples{
my.in.file <- system.file("extdata", "fixrank_classified.txt", package="RDPutils")
my.tax.table <- make_tax_table(in_file = my.in.file, confidence=0.5)
my.tax.table
}
