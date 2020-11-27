\name{condorcet}
\alias{condorcet}
\alias{summary.vote.condorcet}
\alias{view.vote.condorcet}
\alias{print.summary.vote.condorcet}
\alias{vote.condorcet}
\alias{image.vote.condorcet}

\title{
Condorcet Vote Count
}
\description{
Count votes using the Condorcet voting method.
}
\usage{
condorcet(votes, runoff = FALSE, fsep = '\t', quiet = FALSE, \dots)

\method{summary}{vote.condorcet}(object, \dots)

\method{view}{vote.condorcet}(object, \dots)

\method{image}{vote.condorcet}(x, \dots)
}

\arguments{
  \item{votes}{Matrix or data frame containing the votes. Rows correspond to the votes,  columns correspond to the candidates. If it is a character string it is interpreted as a file name from which the votes are to be read. See below for more details.}
  \item{runoff}{Logical. If \code{TRUE} and no condorcet winner exists, the election goes into a run-off, see below for details. }
  \item{fsep}{If \code{votes} is a file name, this argument gives the column separator in the file.}
  \item{quiet}{If \code{TRUE} no output is printed.}
  \item{object, x}{Object of class \code{vote.condorcet}.}
  \item{\dots}{Additional arguments passed to the underlying functions. For the \code{image} function, see arguments for \code{\link{image.vote.stv}}, especially \code{xpref}, \code{ypref}, \code{all.pref} and \code{proportion}.}
}
\details{
The Condorcet method elects the candidate that wins a majority of the ranked vote in every head-to-head election against each of the other candidates. I.e., the Condorcet winner is a candidate that beats all other candidates in pairwise comparisons. Analogously, a Condorcet loser is a candidate that loses against all other candidates.
Neither Condorcet winner nor loser might exist.

If the \code{runoff} argument is set to \code{TRUE} and no Condorcet winner exists, two or more candidates with the most pairwise wins are selected and the method is applied to such subset. If more than two candidates are in such run-off, the selection is performed repeatedly, until either a winner is selected or no more selection is possible.

The input data \code{votes} is structured the same way as for the \code{\link{stv}} method: Row \eqn{i} contains the preferences of voter \eqn{i} numbered \eqn{1, 2, \dots, r, 0,0,0,0}, in some order, while equal preferences are allowed. The columns correspond to the candidates. The dimnames of the columns are the names of the candidates; if these are not supplied then the candidates are lettered A, B, C, \dots. If the dataset contains missing values (\code{NA}), they are replaced by zeros. 

Note that if equal preferences are used, they are automatically converted into a format where for each preference \eqn{i} that does not have any duplicate, there must be exactly \eqn{i-1} preferences \eqn{j} with \eqn{0 < j < i}. It is the same ranking as one would obtain with \code{rank(x, ties.method = "min")}. If a conversion of a vote occurs, a warning is issued. That is done internally by calling the \code{\link{correct.ranking}} function.

The \code{image} function visualizes the joint distribution of two preferences (if \code{all.pref=FALSE}) given by \code{xpref} and \code{ypref}, as well as the marginal distribution of all preferences (if \code{all.pref=TRUE}). The joint distribution can be shown as proportions (if \code{proportion=TRUE}) or raw vote counts (if \code{proportion=FALSE}).
}

\value{
Function \code{condorcet} returns an object of class \code{vote.condorcet} which is a list with the following objects:
\item{elected}{The Condorcet winner if exists, otherwise \code{NULL}.}
\item{loser}{The Condorcet loser if exists, otherwise \code{NULL}.}
\item{totals}{nc x nc matrix where nc is the number of candidates. Element ij = 1 if i won against j, otherwise 0.}
\item{runoff.winner}{The run-off winner if exists and if the \code{runoff} argument was set to \code{TRUE}, otherwise \code{NULL}.}
\item{runoff.participants}{List of run-off participants if the \code{runoff} argument was set to \code{TRUE}, otherwise \code{NULL}.}
\item{data}{Input data (possibly corrected) with invalid votes removed.}
\item{invalid.votes}{Matrix of invalid votes that were removed from the original dataset.}
}

\references{
Condorcet, Marquis de (1785). Essai sur l'application de l'analyse a la probabilite des decisions rendues a la probabilite des voix. Paris: De l'imprimerie royale.

\url{https://en.wikipedia.org/wiki/Condorcet_method}

Sen A. (2017). Collective Choice and Social Welfare. Harvard University Press, Cambridge, Massachusetts (Chapter A4*).
}
\author{
Hana Sevcikova, Salvatore Barbaro
}


\examples{
data(food_election)
cdc.food <- condorcet(food_election)
summary(cdc.food)
# show the marginal distribution of the preferences
par(mai=c(1, 1.2, 0.8, 0.4)) # expand the left margin
image(cdc.food, all.pref = TRUE)

# Example with a runoff
votes <- matrix(c(2, 1, 3, 4,
                  2, 1, 3, 4,
                  4, 3, 2, 1,
                  4, 3, 2, 1,
                  1, 4, 3, 2), byrow = TRUE, nrow = 5)
colnames(votes) <- LETTERS[1:4]  
cdc.v <- condorcet(votes, runoff = TRUE)
}

\keyword{tools }% use one of  RShowDoc("KEYWORDS")

