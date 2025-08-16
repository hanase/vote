\name{approval}
\alias{approval}
\alias{summary.vote.approval}
\alias{view.vote.approval}
\alias{print.summary.vote.approval}
\alias{vote.approval}

\alias{plurality}
\alias{summary.vote.plurality}
\alias{view.vote.plurality}
\alias{print.summary.vote.plurality}
\alias{vote.plurality}

\title{
Approval and Plurality Vote Count
}
\description{
Count votes using the approval and plurality methods. Each voter can select  candidates using  1 for a selection and 0 otherwise. In the approval method, any number of candidates can be selected by a voter, while in the plurality method only one candidate can be chosen by a voter. Thus, plurality voting is a special case of approval voting. The winner(s) in either method is/are the most-approved candidate(s). Each vote can be weighted.
}
\usage{
approval(votes, nseats = 1, fsep = "\t", weight.column = NULL, 
    quiet = FALSE, \dots)

\method{summary}{vote.approval}(object, \dots)

\method{view}{vote.approval}(object, \dots)

plurality(votes, nseats = 1, fsep = "\t", weight.column = NULL, 
    quiet = FALSE, \dots)

\method{summary}{vote.plurality}(object, \dots)

\method{view}{vote.plurality}(object, \dots)

}

\arguments{
  \item{votes}{Matrix or data frame of zeros and ones containing the votes. Rows correspond to the votes, columns correspond to the candidates. If it is a character string it is interpreted as a file name from which the votes are to be read. Missing values (\code{NA}) are interpreted as zeros. The dataset can have an extra column containing a weight for each vote. Name of this column must be passed into the argument \code{weight.column}.}
  \item{nseats}{Number of candidates to be elected.}
  \item{fsep}{If \code{votes} is a file name, this argument gives the column separator in the file.}
  \item{weight.column}{Name of a column in the \code{votes} dataset that represent weights.  If given, a weighted voting is performed.}
  \item{quiet}{If \code{TRUE} no output is printed.}
  \item{\dots}{Not used.}
  \item{object}{Object of class \code{vote.approval} or \code{vote.plurality}.}
}

\value{
Functions \code{approval} and \code{plurality} return an object of class \code{vote.approval} and \code{vote.plurality}, respectively, both of which are lists with the following objects:
\item{elected}{Vector of names of the elected candidates in the order in which they were elected.}
\item{totals}{Vector of total votes in the same order as candidates (columns) in the input data.}
\item{data}{Input data with invalid votes removed. Weights are attached as an attribute called \dQuote{weights}.}
\item{invalid.votes}{Matrix of invalid votes that were removed from the original dataset.}
}
\references{
\url{https://en.wikipedia.org/wiki/Approval_voting}

\url{https://en.wikipedia.org/wiki/Plurality_voting_method}
}
\author{
Hana Sevcikova, Adrian Raftery
}

\seealso{
\code{\link{count.votes}}
}
\examples{
# Example using the IMS Council dataset modified for approval voting
data(ims_approval)
approval(ims_approval) # Li wins

# Increase the weight of voters who did not vote for Li
weighted.approval <- cbind(ims_approval, weight = 1)
weighted.approval$weight[ims_approval$Li == 0] <- 2
approval(weighted.approval, weight.column = "weight") # now Jasper wins

# Example using the IMS Council dataset modified for plurality voting
data(ims_plurality)
pl.ims <- plurality(ims_plurality)
invalid.votes(pl.ims)

# Can we get Wang to win by increasing the weight of its votes?
weighted.plurality <- cbind(ims_plurality, weight = 1)
weighted.plurality$weight[ims_plurality$Wang == 1] <- 10
plurality(weighted.plurality, weight.column = "weight") # now Wang wins

}

\keyword{ tools }% use one of  RShowDoc("KEYWORDS")

