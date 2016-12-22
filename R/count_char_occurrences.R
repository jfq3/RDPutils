# s is a string
# Char is a character to search for in the string
# Returns the number of times char occurs in strng_x.

count_char_occurrences <- function(char, strng_x) {
  s2 <- gsub(char,"",strng_x)
  return (nchar(strng_x) - nchar(s2))
}
