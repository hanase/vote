\name{score}
\alias{score}
\alias{summary.vote.score}
\alias{view.vote.score}
\alias{print.summary.vote.score}
\alias{vote.score}

\title{
Score Vote Count
}
\description{
Count votes using the score (or range) method. Voters give each candidate a score, the scores are added  and the candidate(s) with the highest (or lowest) totals is/are elected.
}
\usage{
score(votes, mcan = 1, max.score = NULL, larger.wins = TRUE, fsep = "\t", ...)

\method{summary}{vote.score}(object, \dots)

\method{view}{vote.score}(object, \dots)

}

\arguments{
  \item{votes}{Matrix or data frame containing the votes which should be numbers between 0 and \code{max.score}. Rows correspond to the votes, columns correspond to the candidates. If it is a character string it is interpreted as a tab-separated file name from which the votes are to be read. Missing values (\code{NA}) are interpreted as zeros.}
  \item{mcan}{Number of candidates to be elected.}
  \item{max.score}{Maximum score allowed. It is used to remove invalid votes. If not given, the maximum value contained in the data is taken and thus, all non-negative votes are valid.}
  \item{larger.wins}{Logical argument indicating whether the winners are the candidates with the highest scores (default) or the lowest scores.}
  \item{fsep}{If \code{votes} is a file name, this argument gives the column separator in the file.}
  \item{\dots}{Not used.}
  \item{object}{Object of class \code{vote.score}.}
}

\value{
Function \code{score} returns an object of class \code{vote.score} which is a list with the following objects:
\item{elected}{Vector of names of the elected candidates in the order in which they were elected.}
\item{totals}{Vector of total votes in the same order as candidates (columns) in the input data.}
\item{larger.wins}{Input argument of the same name.}
\item{data}{Input data with invalid votes removed.}
\item{invalid.votes}{Number of invalid votes that were removed from the original dataset.}
}
\references{
\url{https://en.wikipedia.org/wiki/Range_voting}
}
\author{
Hana Sevcikova, Adrian Raftery
}

\seealso{
\code{\link{count.votes}}
}
\examples{
# Example using the IMS Council dataset modified for score voting
data(ims_score)
score.ims <- score(ims_score, max.score = 9)
summary(score.ims)
}

\keyword{ tools }% use one of  RShowDoc("KEYWORDS")

