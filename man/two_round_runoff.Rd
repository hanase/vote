\name{tworound.runoff}
\alias{tworound.runoff}
\alias{summary.vote.tworound.runoff}
\alias{view.vote.tworound.runoff}
\alias{print.summary.vote.tworound.runoff}
\alias{vote.tworound.runoff}
\alias{image.vote.tworound.runoff}

\title{
    Two-Round Runoff Vote Count
}
\description{
    Count votes using the two-round voting method with ranked ballots. If no candidate reaches the majority, the top two candidates go into a run-off.
}
\usage{
tworound.runoff(votes, fsep = '\t', seed = NULL, quiet = FALSE, \dots)
    
\method{summary}{vote.tworound.runoff}(object, \dots)
    
\method{view}{vote.tworound.runoff}(object, \dots)
    
\method{image}{vote.tworound.runoff}(x, \dots)
}

\arguments{
    \item{votes}{Matrix or data frame containing the votes. Rows correspond to the votes,  columns correspond to the candidates. If it is a character string it is interpreted as a file name from which the votes are to be read. See below for more details.}
    \item{fsep}{If \code{votes} is a file name, this argument gives the column separator in the file.}
    \item{seed}{Integer. Seed of the random number generator (RNG). Only used if there are ties either between candidates to enter the run-off, or between the two run-off contenders. If set to \code{NULL}, the RNG is not initialized.}
    \item{quiet}{If \code{TRUE} no output is printed.}
    \item{object, x}{Object of class \code{vote.tworound.runoff}.}
    \item{\dots}{Additional arguments passed to the underlying functions. For the \code{image} function, see arguments for \code{\link{image.vote.stv}}, especially \code{xpref}, \code{ypref}, \code{all.pref} and \code{proportion}.}
}
\details{
    First, the number of first preferences is counted. If there is a candidate with more than 50\%, that candidate gets elected. Otherwise, there is a runoff between the top two candidates.

    The input data \code{votes} is structured the same way as for the \code{\link{stv}} method: Row \eqn{i} contains the preferences of voter \eqn{i} numbered \eqn{1, 2, \dots, r, 0,0,0,0}, in some order. Equal preferences are not allowed. The columns correspond to the candidates. The dimnames of the columns are the names of the candidates; if these are not supplied then the candidates are lettered A, B, C, \dots. If the dataset contains missing values (\code{NA}), they are replaced by zeros. 
    
    The \code{image} function visualizes the joint distribution of two preferences (if \code{all.pref=FALSE}) given by \code{xpref} and \code{ypref}, as well as the marginal distribution of all preferences (if \code{all.pref=TRUE}). The joint distribution can be shown as proportions (if \code{proportion=TRUE}) or raw vote counts (if \code{proportion=FALSE}).
}

\value{
    Function \code{tworound.runoff} returns an object of class \code{vote.tworound.runoff} which is a list with the following objects:
        \item{elected}{The elected candidate.}
    \item{totals}{Vector of total votes in the same order as candidates (columns) in the input data.}
    \item{totals2r}{Vector of total votes from the run-off (second round).}
    \item{coin.toss.winner}{\code{TRUE} if the winner was sampled between candidates with the same score, otherwise \code{FALSE}.}
    \item{coin.toss.runoff}{\code{TRUE} if the run-off contenders were sampled from candidates with the same score. Otherwise it is \code{FALSE}.}
    \item{data}{Input data (possibly corrected) with invalid votes removed.}
    \item{invalid.votes}{Matrix of invalid votes that were removed from the original dataset.}
}

\author{
Hana Sevcikova, Salvatore Barbaro
}

\references{
Sen A. (2017). Collective Choice and Social Welfare. Harvard University Press, Cambridge, Massachusetts, Chapter 10*3 (p. 243ff).

https://en.wikipedia.org/wiki/Two-round_system
}

\examples{
data(ims_election)
trr <- tworound.runoff(ims_election)
summary(trr)
}

\keyword{tools }% use one of  RShowDoc("KEYWORDS")

