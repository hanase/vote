\name{stv}
\alias{stv}
\alias{summary.vote.stv}
\alias{view.vote.stv}
\alias{view}
\alias{print.summary.vote.stv}
\alias{vote.stv}

\title{
Single Transferable Vote
}
\description{
Count votes using the single transferable voting method.
}
\usage{
stv(votes, mcan = NULL, eps = 0.001, fsep = '\t', verbose = FALSE, \dots)

\method{summary}{vote.stv}(object, \dots)

\method{view}{vote.stv}(object, \dots)
}

\arguments{
  \item{votes}{Matrix or data frame containing the votes. Rows correspond to the votes,  columns correspond to the candidates. If it is a character string it is interpreted as a tab-separated file name from which the votes are to be read. See below for more details.}
  \item{mcan}{Number of candidates to be elected. By default it is half the number of candidates standing.}
  \item{eps}{Value added to the quota. I.e. the STV quota is computed as \cr
  	\code{number_of_first_preferences/(number_of_seats + 1) + eps}.}
  \item{fsep}{If \code{votes} is a file name, this argument gives the column separator in the file.}
  \item{verbose}{Logical. If \code{TRUE} the progress of the count will be printed.}
  \item{\dots}{Not used.}
  \item{object}{Object of class \code{vote.stv}.}
}
\details{
For a description of the single transferable vote system see \url{http://imstat.org/elections/stvprocess.htm}.

The input data \code{votes} is structured as follows: Row \eqn{i} contains the preferences of voter \eqn{i} numbered \eqn{1, 2, \dots, r, 0,0,0,0}, in some order. The columns correspond to the candidates. The dimnames of the columns are the names of the candidates; if these are not supplied then the candidates are lettered A, B, C, \dots. If the dataset contains missing values (\code{NA}), they are replaced by zeros. 
}

\value{
Function \code{stv} returns an object of class \code{vote.stv} which is a list with the following objects:
\item{elected}{Vector of names of the elected candidates in the order in which they were elected.}
\item{preferences}{Matrix of preferences. Columns correspond to the candidates and rows to the counts (i.e. voting rounds).}
\item{quotas}{Vector of quotas, one for each count.}
\item{elect.elim}{Matrix of the same shape as \code{preferences}. Value 1 means that the corresponding candidate was elected in that round; value -1 means an elimination.}
\item{data}{Input data with invalid votes removed.}
\item{invalid.votes}{Matrix of invalid votes that were removed from the original dataset.}
}

\references{
\url{http://imstat.org/elections/stvprocess.htm}

\url{https://en.wikipedia.org/wiki/Single_transferable_vote}
}
\author{
Bernard Silverman, Hana Sevcikova, Adrian Raftery
}


\examples{
# Reproducing example from Wikipedia
# https://en.wikipedia.org/wiki/Single_transferable_vote#Example
# Uses eps=1
data(food_election)
stv.food <- stv(food_election, mcan = 3, eps = 1)
summary(stv.food)
\dontrun{
view(stv.food)}

# Example of the IMS Council voting
data(ims_election)
stv.ims <- stv(ims_election, mcan = 5)
\dontrun{
view(stv.ims)

# write election results into a csv file
s <- summary(stv.ims)
write.csv(s, "myfile.csv")}
}

\keyword{tools }% use one of  RShowDoc("KEYWORDS")

