## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
suppressPackageStartupMessages(library("RDPutils"))
hier.file <- system.file("extdata", "test_hier.txt", package = "RDPutils")
class.expt <- hier2phyloseq(hier.file)
class.expt
rank_names(class.expt)

## ------------------------------------------------------------------------
sample.data.file <- system.file("extdata", "WI_ext_sam.txt", package = "RDPutils")
sam <- read.table(sample.data.file, header = TRUE, row.names = 1, sep = "\t")
sample_data(class.expt) <- sample_data(sam, errorIfNULL = TRUE)
class.expt

## ------------------------------------------------------------------------
rank_names(class.expt)

## ------------------------------------------------------------------------
get_taxa_unique(class.expt, taxonomic.rank="Domain")

## ------------------------------------------------------------------------
fungi <- subset_taxa(class.expt, Domain=="Fungi")
fungi
get_taxa_unique(fungi, taxonomic.rank="Domain")

## ------------------------------------------------------------------------
suppressMessages(suppressWarnings(library(phyloseq)))

biom.file <- system.file("extdata", "all_seq_complete.clust_classified.biom",
                        package="RDPutils")

tree.file <- system.file("extdata", "my_expt_tree.nwk", package = "RDPutils")

ref.seqs <- system.file("extdata", "unaligned_short_names.fasta", package = "RDPutils")

clst.expt <- import_biom(biom.file, 
  treefilename=tree.file,
  refseqfilename=ref.seqs,
  parseFunction=parse_taxonomy_qiime)

clst.expt

