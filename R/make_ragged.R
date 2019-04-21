make_ragged <- function(tax.table) {
  #  tax.table <- tax.table[sort(rownames(tax.table)), ]
  #  new.table <- matrix(data = NA, nrow=nrow(tax.table), ncol=ncol(tax.table))
  #  rownames(new.table) <- row.names(tax.table)
  #  colnames(new.table) <- colnames(tax.table)
  for (i in 1:nrow(tax.table)) {
    for (j in 1:ncol(tax.table)){
      tax.table[i,j] <- remove_uncl(tax.table[i, j])
    }
  }
  return(tax.table)
}

# trim_pre <- function(tax.table) {
#   for (i in 1:nrow(tax.table)) {
#     for (j in 1:ncol(tax.table)) {
#       l <- length(tax.table[i,j])
#       if (l > 0) {
#         tax.table[i,j] <- substr(tax.table[i,j], 4, l)
#       }
#     }
#   }
#   return(tax.table)
# }
