remove_uncl <- function(x){
  if (substr(x,1,4)=="uncl") {
    x <- ""
  }
  return(x)
}
