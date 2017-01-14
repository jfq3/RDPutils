\name{import_itagger_otutab_taxa}
\alias{import_itagger_otutab_taxa}

\title{
Import iTagger File
}
\description{
Converts the tab-delimited iTagger otu.tax.tsv file into a phyloseq object with otu_table and tax_table.
}
\usage{
import_itagger_otutab_taxa(in_file)
}

\arguments{
  \item{in_file}{
otu.tax.tav file
}
}
\details{
The iTagger otu.tax.tsv file is similar to legacy QIIME format. This function expects six ranks: Kingdom, Phylum, Class, Order, Family, and Genus. The taxonomy field in otu.tax.tsv is parsed on semicolons.
}
\value{
A phyloseq object with OTU and taxonomy tables.

}

\author{
John Quensen
}

\examples{
##---- Not run. ----
##-- expt <- import_itagger_otutab_taxa(in_file = "iTagger_otutab_taxa_file.txt")

## The function is currently defined as
function (in_file) 
{
    temp <- read.table(file = in_file, comment.char = "", header = TRUE, 
        row.names = 1, stringsAsFactors = FALSE, sep = "\t")
    otu <- temp[, 1:ncol(temp) - 1]
    otu <- otu_table(as.matrix(otu), taxa_are_rows = TRUE, errorIfNULL = TRUE)
    tax <- matrix(data = NA, nrow = nrow(temp), ncol = 6)
    rownames(tax) <- rownames(temp)
    colnames(tax) <- c("Kingdom", "Phylum", "Class", "Order", 
        "Family", "Genus")
    for (i in 1:nrow(temp)) {
        l <- temp[i, ncol(temp)]
        l.s <- strsplit(l, split = ";", fixed = TRUE)
        n <- length(l.s[[1]])
        for (j in 1:n) {
            tax[i, j] <- l.s[[1]][j]
        }
    }
    for (i in 1:nrow(tax)) {
        for (j in 1:ncol(tax)) {
            if (is.na(tax[i, j])) {
                t <- paste("uncl", tax[i, j - 1], sep = "_")
                for (n in j:ncol(tax)) {
                  tax[i, n] <- t
                }
            }
        }
    }
    tax <- tax_table(tax, errorIfNULL = TRUE)
    expt <- phyloseq(otu, tax)
    return(expt)
  }
}

\keyword{ iTagger }% use one of  RShowDoc("KEYWORDS")

