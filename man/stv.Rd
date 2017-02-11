\name{stv}
\alias{stv}
\title{
Single Transferable Vote
}
\description{
Count votes using the single transferable voting method.
}
\usage{
stv(x, mcan = NULL, oldcount = FALSE, verbose = TRUE)
}

\arguments{
  \item{x}{Matrix containing the votes. If it is a character string it is interpreted as a tab-separated file name from which the
	votes are to be read. See below for details.}
  \item{mcan}{Number of candidates to be elected. By default it is half the number of candidates standing.}
  \item{oldcount}{Logical. If it is \code{TRUE} the function will also count votes under a system that gives one vote to each of the first \code{mcan}
	preferences for each voter which will be then summed up.}
  \item{verbose}{Logical. If \code{TRUE} the progress of the count will be printed.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
