## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
suppressWarnings(suppressMessages(library(phyloseq)))
suppressWarnings(suppressMessages(library(RDPutils)))
suppressWarnings(suppressMessages(library(Biostrings)))
otu.file <- system.file("extdata", "otu_03_table_only.txt",
                        package="RDPutils")
otu <- read.table(file = otu.file, header = TRUE, row.names = 1, 
                  sep = '\t', comment.char = "")
head(otu)
my_otu <- otu_table(otu, taxa_are_rows = TRUE, errorIfNULL = TRUE)
class(my_otu)

## ------------------------------------------------------------------------
otu <- import_otutab_taxa(in_file = otu.file)
head(otu)
class(otu)

## ------------------------------------------------------------------------
parse_taxonomy_usearch <- function (char.vec){
  parse_taxonomy_default(strsplit(char.vec, ",", TRUE)[[1]])
}


biom.file <- system.file("extdata", "otu_03_table_only.json",
                        package="RDPutils")

otu <- import_biom(BIOMfilename = biom.file, parseFunction = parse_taxonomy_usearch)
head(otu)
class(otu)

## ------------------------------------------------------------------------
rdp.class.file <- system.file("extdata", "rdp_classified_03.txt", package = "RDPutils")

rdp_tax <- make_tax_table(in_file = rdp.class.file, confidence = 0.8)
head(rdp_tax)
rank_names(rdp_tax)
taxa_names(rdp_tax)
class(rdp_tax)

## ------------------------------------------------------------------------
utax.table.file <- system.file("extdata", "utax_tax_table_03.txt", package = "RDPutils")

u_tax <- import_utax_file(in_file = utax.table.file, confidence = 0.8)
head(u_tax)
class(u_tax)

## ------------------------------------------------------------------------
sintax.table.file <- system.file("extdata", "sintax_tax_table.txt", package = "RDPutils")

s_tax <- import_sintax_file(in_file = sintax.table.file, confidence = 0.8)
head(s_tax)
class(s_tax)

## ------------------------------------------------------------------------
otu.tab.tax.file <- system.file("extdata", "otu_03_tax_table.txt", package = "RDPutils")

otu_tax <- import_otutab_taxa(in_file = otu.tab.tax.file)
otu_tax
head(otu_table(otu_tax))
head(tax_table(otu_tax))

## ---- eval=FALSE---------------------------------------------------------
#  biom.otu.tax.file <- system.file("extdata", "otu_03_tax_table.json", package="RDPutils")
#  
#  biom_otu_tax <- import_biom(biom.otu.tax.file, parseFunction = parse_taxonomy_usearch)
#  biom_otu_tax

## ------------------------------------------------------------------------
seq.file <- system.file("extdata", "otus_03.fa", package = "RDPutils")

my_seqs <- readDNAStringSet(seq.file, format = "fasta")
my_seqs

usearch.tree.file <- system.file("extdata", "usearch_03_tree.nwk", package = "RDPutils")
my_tree <- read_tree(usearch.tree.file, errorIfNULL = TRUE)
my_tree

sam.data.file <- system.file("extdata", "sam_data.txt", package = "RDPutils")
sam.data <- read.table(file = sam.data.file, header = TRUE, row.names = 1, sep = "\t")
my_sam <- sample_data(sam.data, errorIfNULL = TRUE)

expt <- phyloseq(my_otu, my_sam, s_tax, my_tree, my_seqs)
expt

## ------------------------------------------------------------------------
keep <- names(sort(taxa_sums(expt), decreasing = TRUE)[1:20])
expt.top.20 <- prune_taxa(keep, expt)
tax_table(expt.top.20) <- rdp_tax
expt.top.20

## ------------------------------------------------------------------------
head(tax_table(expt))
head(tax_table(expt.top.20))

