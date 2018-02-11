\name{count_char_occurrences}
\alias{count_char_occurrences}
\title{
count_char_occurrences
}
\description{
Count the number of occurrences of a character in a string.
}
\usage{
count_char_occurrences(char, strng_x)
}
\arguments{
  \item{char}{
Character to search for.
}
  \item{strng_x}{
String that is searched
}
}
\value{
An integer value.
}
\author{
John Quensen
}
\examples{
my.strng <- "abcdefabcaa"
count_char_occurrences("a", strng_x=my.strng)
}

