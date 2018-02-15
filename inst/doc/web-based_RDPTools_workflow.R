## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo=FALSE---------------------------------------------------------
suppressWarnings(suppressPackageStartupMessages(library(RDPutils)))

## ---- eval=FALSE---------------------------------------------------------
#  clstr.file <- system.file("extdata", "all_seq_complete.clust",
#                            package="RDPutils")
#  otu <- clstr2otu(clstr_file=clstr.file, dist=0.03, OutFile=TRUE,
#                   file_name= "dist_03.clust")

## ---- eval=FALSE---------------------------------------------------------
#  otu <- clstr2otu(clstr_file = "all_seq_complete.clust", dist = 0.03,
#                   OutFile = FALSE, file_name = "dist_03.clust")

## ------------------------------------------------------------------------
library(RDPutils)
clstr.file <- system.file("extdata", "all_seq_complete.clust", 
                          package="RDPutils")
otu <- clstr2otu(clstr_file=clstr.file, dist=0.03, 
                 OutFile=TRUE, file_name= "dist_03.clust")
otu[ , 1:5] 

## ------------------------------------------------------------------------
rformatfile <- system.file("extdata", "rformat_dist_0.03.txt", 
                           package="RDPutils")
otu.rdp <- read.table(rformatfile, header=TRUE, row.names=1, 
                      sep="\t")
otu.rdp[ , 1:5]

## ------------------------------------------------------------------------
otu.rdp <- otu.rdp[sort(rownames(otu.rdp)), ]
otu.rdp[ , 1:5]

## ------------------------------------------------------------------------
repseq.file <- system.file("extdata", 
               "all_seq_complete.clust_rep_seqs.fasta", 
                package="RDPutils")
assoc.table <- assoc_repseq_IDs_with_otus_by_fasta(repseq_file = 
                                                     repseq.file)
head(assoc.table)

## ------------------------------------------------------------------------
repseq.file <- system.file("extdata", 
                           "all_seq_complete.clust_rep_seqs.fasta", 
                           package="RDPutils")
rep.seqs <- get_repseq_IDs_from_fasta(repseq_file = repseq.file)
clstr <- system.file("extdata", "dist_03.clust", package="RDPutils")
assoc.table <- assoc_repseq_IDs_with_otus_by_clstr(clstr_file=clstr, 
                                                   rep_seqs=rep.seqs)
head(assoc.table)

## ------------------------------------------------------------------------
repseq.file <- system.file("extdata", 
                           "all_seq_complete.clust_rep_seqs.fasta", 
                           package="RDPutils")
trim_fasta_names(repseq_file = repseq.file, 
                 trimmed_names = "names_trimmed.fasta", strip = FALSE)
rename_fasta(in_file = "names_trimmed.fasta", out_file = "renamed.fasta", 
             rename_table = assoc.table)

## ------------------------------------------------------------------------
my.in.file <- system.file("extdata", "fixrank_classified.txt", package="RDPutils")
my.tax.table <- make_tax_table(in_file = my.in.file, confidence=0.5)
head(my.tax.table)

## ------------------------------------------------------------------------
data(sam.data)
sam.data
rep.tree <- system.file("extdata", "rep.tree.nwk", package="RDPutils")
my.tree <- read_tree(rep.tree)
my.otu <- otu_table(as.matrix(t(otu)), taxa_are_rows=TRUE, errorIfNULL=TRUE)
my.data <- sample_data(sam.data)
my.expt <- phyloseq(my.otu, my.tax.table, my.data, my.tree)
my.expt

## ------------------------------------------------------------------------
hier.file <- system.file("extdata", "test_hier.txt", package="RDPutils")
expt <- hier2phyloseq(hier_file=hier.file)
expt

## ------------------------------------------------------------------------
rank_names(expt)
get_taxa_unique(expt, taxonomic.rank = "Domain")

## ------------------------------------------------------------------------
fungi <- subset_taxa(expt, Domain=="Fungi")
fungi

## ------------------------------------------------------------------------
expt.sum <- sum(otu_table(expt))
fungi.sum <- sum(otu_table(fungi))
100*((expt.sum-fungi.sum)/expt.sum)

