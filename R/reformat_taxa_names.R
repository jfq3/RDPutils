#' Reformat Taxa Names
#'
#' @param p A phyloseq object including taxa
#' @param otu_format A string specifying format
#' @details Format string is either "R" (the default) or "biom", corresponding to the formats returned by RDPTools.
#' 
#' @return A phyloseq object with reformatted taxa names
#' @export


reformat_taxa_names <- function(p, otu_format = "R") {
  taxa_names(p) <- make_otu_names(get_digits(taxa_names(p)), otu_format)
  return(p)
}