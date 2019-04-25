get_consensus <- function(tab.a, tab.b, tab.c) {
  tab.a <- tab.a[order(rownames(tab.a)), ]
  tab.b <- tab.b[order(rownames(tab.b)), ]
  tab.c <- tab.c[order(rownames(tab.c)), ]
  if (any(rownames(tab.a) != rownames(tab.b)) || any(rownames(tab.a) != rownames(tab.c)) || any(rownames(tab.b) != rownames(tab.c))) {
    return("Taxa names do not agree")
  }
  if (any(colnames(tab.a) != colnames(tab.b)) || any(colnames(tab.a) != colnames(tab.c)) || any(colnames(tab.b) != colnames(tab.c))) {
    return("Rank names do not agree")
  }
  new.table <- matrix(data = NA, nrow=nrow(tab.a), ncol = ncol(tab.a))
  rownames(new.table) <- row.names(tab.a)
  colnames(new.table) <- colnames(tab.a)
  for (i in 1:nrow(new.table)) {
    for (j in 1:ncol(new.table)){
      tax.vec <- c(tab.a[i,j], tab.b[i,j], tab.c[i,j])
      new.table[i,j] <-vote(tax.vec)
    }
  }
  return(new.table)
}
