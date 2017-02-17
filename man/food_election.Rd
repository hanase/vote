\name{food_election}
\alias{food_election}
\docType{data}
\title{
Example Dataset
}
\description{
Dataset on food election which serves as a simple example for the STV method taken from Wikipedia.
}
\usage{data("food_election")}
\format{
  A data frame with 20 observations and 5 candidates (Oranges, Pears, Chocolate, Strawberries, Sweets). Each record corresponds to one ballot with ranking for each of the candidates.
  % \describe{
    % \item{\code{Oranges}}{a numeric vector}
    % \item{\code{Pears}}{a numeric vector}
    % \item{\code{Chocolate}}{a numeric vector}
    % \item{\code{Strawberries}}{a numeric vector}
    % \item{\code{Sweets}}{a numeric vector}
  % }
}

\source{
\url{https://en.wikipedia.org/wiki/Single_transferable_vote#Example}
}

\examples{
data(food_election)
head(food_election)
}
\keyword{datasets}
