vote <- function(taxa){
  x <- table(taxa)
  winner <- names(which(x==max(x)))
  if (length(winner)==3) {
    return("")
  } else if (winner == "") {
    winner <- names(which(x==min(x)))
  }
  return(winner)
}

remove_uncl <- function(x){
  if (substr(x,1,4)=="uncl") {
    x <- ""
  }
  return(x)
}
