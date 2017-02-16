\name{approval}
\alias{approval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Approval Vote Count
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
approval(votes, mcan = 1, fsep = "\t", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{votes}{
%%     ~~Describe \code{votes} here~~
}
  \item{mcan}{
%%     ~~Describe \code{mcan} here~~
}
  \item{fsep}{
%%     ~~Describe \code{fsep} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
function (votes, mcan = 1, fsep = "\t", ...) 
{
    cat("\nApproval vote count")
    cat("\n===================\n")
    x <- prepare.votes(votes, fsep = fsep)
    cat("Number of votes cast is", nrow(x), "\n")
    x <- check.votes(x, "approval")
    res <- sum.votes(x)
    elected <- names(rev(sort(res))[1:mcan])
    cat("\nElected candidates are, in order of election: \n", 
        paste(elected, collapse = ", "), "\n")
    return(structure(list(elected = elected, totals = res), class = "vote.approval"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
