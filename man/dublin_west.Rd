\name{dublin_west}
\alias{dublin_west}

\docType{data}
\title{
Election Dataset to Dublin West Constituency
}
\description{
Dataset containing ranked votes to the Dublin West constituency in 2002, Ireland. In that election, the STV method was used and results can be viewed at \url{https://en.wikipedia.org/wiki/Dublin_West#2002_general_election}. They can be reproduced via the \code{\link{stv}} function, see the example in \code{\link{stv}}.
}
\usage{
data("dublin_west")
}

\format{
  A data frame with 29988 observations and 9 candidates. Each record corresponds to one ballot with candidates being ranked between 1 and 9 with zeros allowed. 
}

%\source{
%}

\references{
\url{https://en.wikipedia.org/wiki/Dublin_West#2002_general_election}
}

\examples{
data(dublin_west)
head(dublin_west)
}
\keyword{datasets}
