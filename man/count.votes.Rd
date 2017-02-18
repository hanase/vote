\name{count.votes}
\alias{count.votes}
\alias{invalid.votes}
\alias{valid.votes}

\title{
Count Votes
}
\description{
Count votes using one of four methods. View valid and invalid ballots.
}
\usage{
count.votes(votes, method = c("auto", "plurality", "approval", "stv", "score"), 
    fsep = "\t", \dots)
	
invalid.votes(object)
valid.votes(object)
}

\arguments{
  \item{votes}{Matrix or data frame containing the votes. Rows correspond to the votes, columns correspond to the candidates. If it is a character string it is interpreted as a tab-separated file name from which the votes are to be read. 
}
  \item{method}{Voting method to use. If \dQuote{auto}, the input data is passed through a checker for each of the methods and the one with the largest number of valid votes is used. In case of the same number of valid votes, it goes by their ordering in the function definition. }
  \item{fsep}{If \code{votes} is a file name, this argument gives the column separator in the file.}
  \item{\dots}{Additional arguments passed to the underlying functions, e.g. \code{mcan}, \code{max.score} etc.}
  \item{object}{Object returned by one of the functions \code{\link{plurality}}, \code{\link{approval}}, \code{\link{stv}}, \code{\link{score}}.}
}

\value{
	Depending which method is used, \code{count.votes} returns an object of class \code{\link{vote.plurality}}, \code{\link{vote.approval}}, \code{\link{vote.stv}}, or \code{\link{vote.score}}.
	
	Functions \code{valid.votes} and \code{invalid.votes} return a subset of the input data with valid records and invalid records, respectively.
}

\author{
Hana Sevcikova, Bernard Silverman
}


\seealso{
\code{\link{stv}}, \code{\link{approval}}, \code{\link{score}}
}
\examples{
# Example using the IMS Council dataset modified for score voting
data(ims_score)
# should recognize that it is a dataset with score voting data
count.votes(ims_score, max.score = 9, larger.wins = FALSE)

# All records with score larger than 8 are excluded 
res <- count.votes(ims_score, method = "score", max.score = 8)
head(invalid.votes(res))

summary(res)
}

\keyword{ tools }% use one of  RShowDoc("KEYWORDS")

