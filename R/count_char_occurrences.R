#' @title  Count Occurrences of a Character
#' @name count_char_occurrences
#' @aliases count_char_occurrences
#' @export
#' @description Count the number of occurrences of a character in a string.
#' @usage count_char_occurrences(char, strng_x)
#' @param char Character to search for
#' @param strng_x String that is searched
#' @returns An integer, the number of times a character occurs in a string.
#' @author John Quensen
#' @examples 
#' my.strng <- "abcdefabcaa"
#' count_char_occurrences("a", strng_x=my.strng)

count_char_occurrences <- function(char, strng_x) {
  s2 <- gsub(char,"",strng_x)
  return (nchar(strng_x) - nchar(s2))
}
