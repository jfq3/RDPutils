trim_pre <- function(tax.table) {
  for (i in 1:nrow(tax.table)) {
    for (j in 1:ncol(tax.table)) {
      l <- nchar(tax.table[i,j])
      if (l > 0) {
        tax.table[i,j] <- substr(tax.table[i,j], 3, l)
      }
    }
  }
  return(tax.table)
}
