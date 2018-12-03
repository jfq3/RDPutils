#' Get digits
#'
#' @param otu.names A vector of taxa names including underscores separating characters and digits
#'
#' @return A vector of digits
#' @export
#' @details Useful in reformating OTU/taxa names, sometimes necessary to merge phyloseq objects.
#' @examples
#' otu.names <- c("cluster_1), "cluster_2", "cluster_3")
#' digits <- get_digits(otu.names)
#' make_otu_names(digits, otu_format = "R")

get_digits <- function(otu.names) {
  for (i in 1:length(otu.names)) {
    temp <- strsplit(otu.names[i], split = "_")[[1]]
    num[i] <- temp[2]
  }
  
  digits <- as.integer(num)
  
  return(digits)
}