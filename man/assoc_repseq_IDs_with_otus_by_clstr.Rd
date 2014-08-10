\name{assoc_repseq_IDs_with_otus_by_clstr}
\alias{assoc_repseq_IDs_with_otus_by_clstr}

\title{
Associate Representative Sequence IDs with OTUs from Cluster File
}
\description{
This function parses a cluster file for a single distance and makes a table 
	associating the representative sequence machine names with the OTU names 
	as given by RDPTools R-formatter or function clstr2otu in this package.
}
\usage{
assoc_repseq_IDs_with_otus_by_clstr(clstr_file="dist_0.03.clust", rep_seqs=rep.seqs)

}
\arguments{
  \item{clstr_file}{
The name of a cluster file for a single distance.
}
  \item{rep_seqs}{
A vector of representative sequence machine names.
}
}
\details{
The first input to this function is the name of a cluster file for a single 
	distance.  The function reads the cluster file from disk.  The cluster file
	for a single distance is excised from the original cluster file which likely contained cluster 
	information for several distances with either of the functions clstr2otu or 
	split_clstr_file.

The second input to this function is a vector of representative sequence machine 
	names corresponding to the input cluster file; that is, they are for the same distance.  This vector is created from a fasta 
	file of the representative sequences with the function get_repseq_IDs_from_fasta.
}
\value{
This function returns a data frame.  The first column contains the machine names 
	of the representative sequences.  The second column contains the names of the 
	corresponding OTUs as given by clstr2otu, by RDP's web-based R-formatter, 
	and by the command line version of function Cluster in RDPTools.  The third 
	column contains sample names and the fourth the number of sequences in the
  OTU for the sample in column three.  These last two columns are provided as a 
	means of checking the result.
}
\references{
The web-based tool for retrieving representative sequences is here:
http://pyro.cme.msu.edu/

The RDPTools are available on GitHub:
https://github.com/rdpstaff
}
\author{
John Quensen
}

\examples{
repseq.file <- system.file("extdata", "all_seq_complete.clust_rep_seqs.fasta", package="RDPutils")
rep.seqs <- get_repseq_IDs_from_fasta(repseq_file = repseq.file)
clstr.file <- system.file("extdata", "dist_03.clust", package="RDPutils")
assoc.table <- assoc_repseq_IDs_with_otus_by_clstr(clstr_file = clstr.file, rep_seqs = rep.seqs)
head(assoc.table)
}
