\name{assoc_repseq_IDs_with_otus_by_fasta}
\alias{assoc_repseq_IDs_with_otus_by_fasta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Associate Representative Sequences with OTUs from Fasta Headers
}
\description{
This function parses representative sequence IDs and makes a table 
  associating the representative sequence machine names with the OTU names 
	as given by RDPTools R-formatter or function clstr2otu in this package.
}
\usage{
assoc_repseq_IDs_with_otus_by_fasta(repseq_file="all_seq_complete.clust_rep_seqs.fasta")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repseq_file}{
The name of the fasta file containing representative sequences.
}
}
\details{
Representative sequences from clusters for a given distance may be obtained with either the web-based representative sequence tool currently on the rdpipeline page (http://pyro.cme.msu.edu/), or with the RDPTools' cluster function using a command similar to:

java -Xmx2g -jar $Clustering rep-seqs --one-rep-per-otu all_seq_complete.clust 0.03 merged_aligned.fasta

In these cases the fasta headers contain information on the cluster number and the size of the cluster.  This function parses this information into a table that can be used as input to function rename_fasta, which renames the representative sequences with their corresponding OTU names.
}
\value{
This function returns a data frame with 4 columns: the machine name of the representative sequence, the corresponding OTU name as given by the function clstr2otu and by RDP's R-formatter tool, the cluster number, and the total number of sequences in the cluster (cluster size).
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
John Quensen
}
\note{
The representative sequence tool on the FunGene pipeline page (http://fungene.cme.msu.edu/FunGenePipeline/) returns one representative sequences per sample, a format which is not compatible with this function.

This function expects the representative sequence IDs to be formatted in one of these ways:

>HC9DO0P01BCTC4  prefered=false,cluster=0,clustsize=2

>HC9DO0P01BCTC4  cluster_id=1,size=2

If the representative sequence IDs are not formatted as in these examples, or do not contain information on cluster number and size, a similar association table may be made using function assoc_repseq_IDs_with_otus_by_clstr.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{assoc_repseq_IDs_with_otus_by_clstr}}, \code{\link{clstr2otu}}, \code{\link{rename_fasta}}
}
\examples{
repseq.file <- system.file("extdata", "all_seq_complete.clust_rep_seqs.fasta", package="RDPutils")
assoc.table <- assoc_repseq_IDs_with_otus_by_fasta(repseq_file=repseq.file)
head(assoc.table)
}
