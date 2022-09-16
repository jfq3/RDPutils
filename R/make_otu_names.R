#' @title Make OTU Names
#' @name make_otu_names
#' @aliases make_otu_names
#' @description This function takes a vector of integers and makes a vector of OTU names.
#' @usage make_otu_names(otu_nums, otu_format="R")
#' @param otu_nums A vector of integers.
#' @param otu_format When equal to "R" (default) OTU names have the form "OTUxxxnn."  When equal to "biom", OTU names have the form "cluster_nn."
#' @details This function is used by the function clstr2otu to name the columns of the OTU table it returns, but make_otu_names can be used separately if desired.
#' @returns A vector of character strings of the form "OTU_xxxnn"" or "cluster_nn."
#' @author John Quensen
#' @export
#' @examples 
#' n <- c(1:10)
#' otu.names <- make_otu_names(n)
#' otu.names
#' @keywords RDPTools

make_otu_names <-
function(otu_nums, otu_format="R") {
  new.clstr.name <- matrix("",length(otu_nums),1)
  if(otu_format=="R") {
    n <- nchar(as.character(max(otu_nums)))
    for (i in 1:length(otu_nums)) {
      k <- nchar(as.character(otu_nums[i]))
      k <- n-k
      if (k==0) {pad<-""} else {
        pad <- ""
        for (j in 1:k) {
          pad <- ""
          for (j in 1:k){
            pad <- paste(pad, "0", sep="")
          }
        }
      }
      new.clstr.name[i,1] <- paste("OTU_", pad, as.character(otu_nums[i]), sep="")
    }
  } else if (otu_format == "biom") {
    for (i in 1:length(otu_nums)) {
      new.clstr.name[i,1] <- paste("cluster_", as.character(otu_nums[i]), sep="")
    }
  } else {
    return("otu_format not recognized")
  }

  return(new.clstr.name)
}
