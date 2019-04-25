make_ragged <- function(tax.table) {
  for (i in 1:nrow(tax.table)) {
    for (j in 1:ncol(tax.table)){
      tax.table[i,j] <- remove_uncl(tax.table[i, j])
    }
  }
  return(tax.table)
}
