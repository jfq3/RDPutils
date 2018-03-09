\name{RDPutils-package}
\alias{RDPutils-package}
\alias{RDPutils}
\docType{package}
\title{
RDPutils
}
\description{
This package provides utilities for importing RDP output into R and the Bioconductor package phyloseq. Version 1.3.0 adds functions for importing   USEARCH  and iTagger output.

The RDP provides both web-based and command line tools for processing rRNA gene sequences from Bacteria, Archaea, and Fungi as well as functional genes.  These tools cluster sequences, reformat cluster files into OTU tables that can be imported into R, retrieve representative sequences for each cluster, and classify sequences from one or more samples.  RDP does not, however, provide means of attaching taxonomic information to clusters (OTUs), of renaming representative sequences to correspond to OTUs, nor of easily importing classifier results into R.  The functions in this package fill these gaps.  Renaming representative sequences makes it possible to assign taxonomy to OTUs and to label a tree of the representative sequences with OTU names. In this manner, a phyloseq object may be fully populated with an otu_table, tax_table, sample_data, and tree.

Phyloseq is a Bioconductor/R package that provides a convenient way of organizing data from a sequencing experiment and also provides wrappers for common analyses and plots.  Additionally, functions from other R packages may be applied to phyloseq components.  Thus RDPutils makes it possible to apply the full range of analytic procedures available in R to RDP's clustering and classifier results.
}
\details{
\tabular{ll}{
Package: \tab RDPutils\cr
Type: \tab Package\cr
Version: \tab 1.4.1\cr
Date: \tab 2018-03-08\cr
License: \tab GPL-2\cr
}

Brief descriptions of the more important functions follow.

For clustering results:

clstr2otu parses an OTU file for a given distance from a cluster file.

assoc_repseq_IDs_with_otus_by_fasta - parses representative sequence headers to give a table associating the machine names of representative sequences for each cluster with the OTU name in the OTU table.  This function depends on the fasta IDs being formatted in certain ways.

assoc_repseq_IDs_with_otus_by_clstr - parses the cluster file to give a table associating the machine names of representative sequences for each cluster with the OTU name in the OTU table.  This function is more robust but slower than assoc_repseq_IDs_with_otus_by_fasta.  It can be used in cases where the fasta IDs are formatted differently - i.e. for some legacy results, or in case the fasta ID format changes.

rename_fasta - renames the representative sequences from their machine names to their corresponding OTU names.  These renamed sequences can then be classified and treed, and the results imported into a phyloseq object.

make_tax_table - reformats RDP classifier results into a phyloseq tax_table.

For classifier results:

hier2phyloseq  - parses a hierarchical classification file to give a phyloseq object with otu_table and tax_table.
	
Additional functions are provided for dividing a cluster file into separate files for each distance and for modifying fasta files to prepare them for renaming and treeing.	

For USEARCH results:

import_usearch_biom_file - For correctly importing biom files.

import_otutab_taxa - For importing OTU tables and combined OTU/taxonomy tables.

import_utax_file and import_sintax_file for importing taxonomy files.

For iTagger results:

import_itagger_otutab_taxa - For importing combined OTU/taxonomy tables created with iTagger.
}

\author{
John Quensen

Maintainer: John Quensen <quensenj@msu.edu>
}
\references{
Cole, J. R., Q. Wang, J. A. Fish, B. Chai, D. M. McGarrell, Y. Sun, C. T. Brown, A. Porras-Alfaro, C. R. Kuske, and J. M. Tiedje. 2014. Ribosomal Database Project: data and tools for high throughput rRNA analysis Nucl. Acids Res. 41(Database issue):D633-D642; doi: 10.1093/nar/gkt1244 [PMID: 24288368]

McMurdie and Holmes (2013) phyloseq: An R Package for Reproducible Interactive Analysis and Graphics of Microbiome Census Data. PLoS ONE. 8(4):e61217

Wang, Q, G. M. Garrity, J. M. Tiedje, and J. R. Cole. 2007. Native Bayesian Classifier for Rapid Assignment of rRNA Sequences into the New Bacterial Taxonomy. Appl Environ Microbiol. 73(16):5261-5267; doi: 10.1128/AEM.00062-07 [PMID: 17586664]
}

\keyword{ RDP }
