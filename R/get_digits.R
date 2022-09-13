#' Get digits
#'
#' @param otu.names A vector of taxa names including underscores separating characters and digits
#'
#' @return A vector of digits
#' @export
#' @details Useful in reformatting OTU/taxa names, sometimes necessary to merge phyloseq objects.


get_digits <- function(otu.names) {
  num <- vector(mode = "integer", length = length(otu.names))
  for (i in 1:length(otu.names)) {
    temp <- strsplit(otu.names[i], split = "_")[[1]]
    num[i] <- temp[2]
  }
  
  digits <- as.integer(num)
  
  return(digits)
}