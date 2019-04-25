make_unragged <- function (in.file) 
{
  class.table <- in.file
  ranks <- as.vector(colnames(class.table))
  #class.table <- class.table[order(rownames(class.table)), ]
  
  first.rank <- ranks[1]
  
  for (i in 1:nrow(class.table)) {
    if (class.table[i, 1] == "") {
      class.table[i, 1] <- base::paste("uncl_", first.rank)
    }
  }
  
  for (i in 1:nrow(class.table)) { 
    for (j in 2:ncol(class.table)) {
      if (class.table[i,j] == "") {
        if (substr(class.table[i, j-1], 1, 4) == "uncl") {
          class.table[i,j] <-  class.table[i, j-1]
        }
        else {
          class.table[i,j] <- paste("uncl", class.table[i, j-1], sep = "_")
        }
      }
    }
  }
  
  ranks <- sapply(ranks, simple_cap)
  colnames(class.table) <- ranks
  class.table <- tax_table(as.matrix(class.table), errorIfNULL = TRUE)
  return(class.table)
}
