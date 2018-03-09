\name{make_framebot_tax_table}
\alias{make_framebot_tax_table}
\title{
make_framebot_tax_table
}
\description{
Create a taxonomy table from FunGene Pipeline output. 
}
\usage{
make_framebot_tax_table(clstr_machine, taxa_machine)
}
\arguments{
  \item{clstr_machine}{
match_cluster_machine_name.txt from my FunGene Pipeline script
}
  \item{taxa_machine}{
match_taxa_machine_names.txt from my FunGene Pipeline script
}
}
\details{
My FunGene pipeline script (see john-quensen.com) produces one file matching representative sequence machine names with cluster numbers and a second matching representative sequence machine names with taxa names found by FrameBot. This function parses and combines the two files to produce a phyloseq tax_table. Ranks are Genus, Species, and Strain. Classification is the closest match to sequence in the FrameBot reference database. The percent identity to the closest match is appended to the name of the closest match (strain level in the tax_table).
}
\value{
A tax_table of class phyloseq.
}
\author{
John Quensen
}
\note{
See the workshop page Command Line FunGene Pipeline at john-quensen.com for the FunGene Pipeline script.
}
\examples{
match.clst <- system.file("extdata", "match_cluster_machine_name.txt", package="RDPutils")
match.taxa <- system.file("extdata", "match_taxa_machine_names.txt", package="RDPutils")
my_taxa <- make_framebot_tax_table(clstr_machine=match.clst, taxa_machine=match.taxa) 
}

