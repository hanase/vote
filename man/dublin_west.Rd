\name{dublin_west}
\alias{dublin_west}

\docType{data}
\title{
Election Dataset to Dublin West Constituency
}
\description{
Dataset containing ranked votes for the Dublin West constituency in 2002, Ireland. Results of that STV elections can be viewed at \url{https://en.wikipedia.org/wiki/Dublin_West#2002_general_election}. They can be reproduced via the \code{\link{stv}} function, see Example below.
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

\dontrun{
# produce similar results as in the Wikipedia link above
dwstv <- stv(dublin_west, nseats = 3, eps = 1, constant.quota = TRUE)

# plot results
plot(dwstv)
image(dwstv)
image(dwstv, all.pref = TRUE)}
}
\keyword{datasets}
