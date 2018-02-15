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
My FunGene pipeline script produces one file matching representative sequence machine names with cluster numbers and a second matching representative sequence machine names with taxa names found by FrameBot. This function parses and combines the two files to produce a phyloseq tax_table. Ranks are Genus, Species, and Strain. Classification is the closest match to sequence in the FrameBot reference database.
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
##---- Not run ----
##-- my_tax <- make_framebot_tax_table(clstr_machine="match_cluster_machine_name.txt", taxa_machine="match_taxa_machine_names.txt") 
}

