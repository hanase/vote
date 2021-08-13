\name{count.votes}
\alias{count.votes}
\alias{invalid.votes}
\alias{valid.votes}
\alias{corrected.votes}

\title{
Count Votes
}
\description{
Count votes using one of five methods. View valid, invalid and corrected ballots.
}
\usage{
count.votes(votes, method = c("auto", "plurality", "approval", "stv", 
    "score", "condorcet", "tworound.runoff"), fsep = "\t", \dots)
	
invalid.votes(object)
valid.votes(object)
corrected.votes(object)
}

\arguments{
  \item{votes}{Matrix or data frame containing the votes. Rows correspond to the votes, columns correspond to the candidates. If it is a character string it is interpreted as a file name from which the votes are to be read. 
}
  \item{method}{Voting method to use. If \dQuote{auto}, the input data is passed through a checker for each of the methods and the one with the largest number of valid votes is used. In case of the same number of valid votes, it goes by their ordering in the function definition. }
  \item{fsep}{If \code{votes} is a file name, this argument gives the column separator in the file.}
  \item{\dots}{Additional arguments passed to the underlying functions, e.g. \code{nseats}, \code{max.score} etc.}
  \item{object}{Object returned by one of the functions \code{\link{plurality}}, \code{\link{approval}}, \code{\link{stv}}, \code{\link{score}}, \code{\link{condorcet}}, \code{\link{tworound.runoff}}.}
}

\value{
	Depending which method is used, \code{count.votes} returns an object of class \code{\link{vote.plurality}}, \code{\link{vote.approval}}, \code{\link{vote.stv}}, \code{\link{vote.score}},  \code{\link{vote.condorcet}}, or \code{\link{vote.tworound.runoff}}.
	
	Functions \code{valid.votes} and \code{invalid.votes} return a subset of the input data with valid records and invalid records, respectively. 
	
	Function \code{corrected.votes} can be used when votes are automatically corrected (as in \code{\link{stv}} and \code{\link{condorcet}}). It returns a list with the uncorrected votes (item \code{original}), the corrected votes (item \code{new}), and its indices within the original votes dataset (item \code{index}).
}

\author{
Hana Sevcikova, Bernard Silverman
}


\seealso{
\code{\link{stv}}, \code{\link{approval}}, \code{\link{score}}, \code{\link{condorcet}}
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

# For a corrected.votes() example see ?stv
}

\keyword{ tools }% use one of  RShowDoc("KEYWORDS")

