\name{make_unragged}
\alias{make_unragged}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make Ragged
}
\description{
Convert ragged taxonomy file to an un-ragged one.
}
\usage{
make_unragged(in.file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in.file}{
A ragged taxonomy file.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A non-ragged phyloseq tax_table object.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
John Quensen
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (in.file) 
{
    class.table <- in.file
    ranks <- as.vector(colnames(class.table))
    first.rank <- ranks[1]
    for (i in 1:nrow(class.table)) {
        if (class.table[i, 1] == "") {
            class.table[i, 1] <- base::paste("uncl_", first.rank)
        }
    }
    for (i in 1:nrow(class.table)) {
        for (j in 2:ncol(class.table)) {
            if (class.table[i, j] == "") {
                if (substr(class.table[i, j - 1], 1, 4) == "uncl") {
                  class.table[i, j] <- class.table[i, j - 1]
                }
                else {
                  class.table[i, j] <- paste("uncl", class.table[i, 
                    j - 1], sep = "_")
                }
            }
        }
    }
    ranks <- sapply(ranks, simple_cap)
    colnames(class.table) <- ranks
    class.table <- tax_table(as.matrix(class.table), errorIfNULL = TRUE)
    return(class.table)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
