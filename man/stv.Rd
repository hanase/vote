\name{stv}
\alias{stv}
\alias{summary.vote.stv}
\alias{view.vote.stv}
\alias{view}
\alias{print.summary.vote.stv}
\alias{vote.stv}
\alias{image.vote.stv}
\alias{plot.vote.stv}
\alias{correct.ranking}

\title{
Single Transferable Vote
}
\description{
Count votes using the single transferable voting method, also known as ranked choice voting or instant runoff.
}
\usage{
stv(votes, mcan = NULL, eps = 0.001, equal.ranking = FALSE, 
  fsep = '\t', verbose = FALSE, seed = 1234, quiet = FALSE, \dots)

\method{summary}{vote.stv}(object, \dots, digits = 3)

\method{view}{vote.stv}(object, \dots)

\method{plot}{vote.stv}(x, xlab = "Count", ylab = "Preferences", point.size = 2, \dots)

\method{image}{vote.stv}(x, xpref = 2, ypref = 1, all.pref = FALSE, proportion = TRUE, \dots)

correct.ranking(votes, quiet = FALSE)
}

\arguments{
  \item{votes}{Matrix or data frame containing the votes. Rows correspond to the votes,  columns correspond to the candidates. If it is a character string it is interpreted as a file name from which the votes are to be read. See below for more details.}
  \item{mcan}{Number of candidates to be elected. By default it is half the number of candidates standing.}
  \item{eps}{Value added to the quota. I.e. the STV quota is computed as \cr
  	\code{number_of_first_preferences/(number_of_seats + 1) + eps}.}
  \item{equal.ranking}{If \code{TRUE} equal preferences are allowed, see below.}
  \item{fsep}{If \code{votes} is a file name, this argument gives the column separator in the file.}
  \item{verbose}{Logical. If \code{TRUE} the progress of the count will be printed.}
  \item{seed}{Integer. Seed of the random number generator. Only used if there are ties that cannot be resolved by the tie-breaking method. If set to \code{NULL}, the RNG is not initialized.}
  \item{quiet}{If \code{TRUE} no output is printed.}
  \item{object, x}{Object of class \code{vote.stv}.}
  \item{digits}{How many significant digits to be used in the output table.}
  \item{xlab, ylab}{Labels of the x- and y-axis.}
  \item{point.size}{Size of the points in the plot.}
  \item{xpref, ypref}{Preference for the x- and y-axis, respectively, for showing the joined distribution of the votes. It is not used if \code{all.pref} is \code{TRUE}.}
  \item{all.pref}{Logical. If \code{TRUE} the marginal distribution of all preferences is shown in the image. Otherwise, the joint distribution of \code{xpref} and \code{ypref} is shown.}
  \item{proportion}{If \code{TRUE} the preferences are shown as proportions across the x-axis, otherwise raw vote counts are shown. Only available when \code{all.pref} is \code{FALSE}.}
  \item{\dots}{Additional arguments passed to the underlying functions.}
}
\details{
For a description of the single transferable vote system see \url{https://imstat.org/elections/single-transferable-voting-system/}.

The input data \code{votes} is structured as follows: Row \eqn{i} contains the preferences of voter \eqn{i} numbered \eqn{1, 2, \dots, r, 0,0,0,0}, in some order. The columns correspond to the candidates. The dimnames of the columns are the names of the candidates; if these are not supplied then the candidates are lettered A, B, C, \dots. If the dataset contains missing values (\code{NA}), they are replaced by zeros. 

By default the preferences are not allowed to contain duplicates per vote. However, if the argument \code{equal.ranking} is set to \code{TRUE}, votes are allowed to have the same ranking for multiple candidates. The desired format is such that for each preference \eqn{i} that does not have any duplicate, there must be exactly \eqn{i-1} preferences \eqn{j} with \eqn{0 < j < i}. For example, valid ordered preferences are \eqn{1,1,3,4,\dots}, or \eqn{1,2,3,3,3,6,\dots}, but NOT \eqn{1,1,2,3,\dots}, or NOT \eqn{1,2,3,3,3,5,6,\dots}. If the data contain such invalid votes, they are automatically corrected and a warning is issued by calling the \code{correct.ranking} function. 

The \code{correct.ranking} function does the above correction for all records, regardless if they contain duplicates or not. It can either be used by calling it explicitely, otherwise it is called by \code{stv} if \code{equal.ranking = TRUE}. The function is also called from within the \code{\link{condorcet}} function.

Ties in the STV algorithm are resolved using the forwards tie-breaking method, see Newland and Briton (Section 5.2.5).

The \code{plot} function shows the evolution of the total score for each candidate as well as the quota. The \code{image} function visualizes the joint distribution of two preferences (if \code{all.pref=FALSE}) as well as the marginal distribution of all preferences (if \code{all.pref=TRUE}). The joint distribution can be shown either as proportions (if \code{proportion=TRUE}) or raw vote counts (if \code{proportion=FALSE}). 
}

\value{
Function \code{stv} returns an object of class \code{vote.stv} which is a list with the following objects:
\item{elected}{Vector of names of the elected candidates in the order in which they were elected.}
\item{preferences}{Matrix of preferences. Columns correspond to the candidates and rows to the counts (i.e. voting rounds).}
\item{quotas}{Vector of quotas, one for each count.}
\item{elect.elim}{Matrix of the same shape as \code{preferences}. Value 1 means that the corresponding candidate was elected in that round; value -1 means an elimination.}
\item{equal.pref.allowed}{Input argument \code{equal.ranking}.}
\item{data}{Input data (possibly corrected) with invalid votes removed.}
\item{invalid.votes}{Matrix of invalid votes that were removed from the original dataset.}
}

\references{

R.A. Newland and F.S. Britton (1997). How to conduct an election by the Single Transferable Vote. ERS 3rd Edition. \url{http://www.rosenstiel.co.uk/stvrules/index.html}

\url{https://imstat.org/elections/single-transferable-voting-system/}

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
plot(stv.ims)
image(stv.ims)

# write election results into a csv file
s <- summary(stv.ims)
write.csv(s, "myfile.csv")}

\dontrun{
# Example of Dublin West 2002 elections
# https://en.wikipedia.org/wiki/Dublin_West#2002_general_election
data(dublin_west)
stv(dublin_west, mcan = 3, eps = 1)}

# Example of a small committee dataset
# with four candidates (C) and four
# voting committee members (uses tie-breaking)
votes <- data.frame(C1=c(3,2,1,3), C2=c(2,1,2,4),
                    C3=c(4,3,3,1), C4=c(1,4,4,2))
stv(votes, mcan = 2, verbose = TRUE)

# Example with equal ranking and correction
votes <- data.frame(C1=c(3,2,1,3), C2=c(1,1,2,0),
                    C3=c(4,3,3,1), C4=c(1,4,2,2))
stv(votes, mcan = 2, equal.ranking = TRUE)
# vote #3 was corrected by stv which used this data:
correct.ranking(votes, quiet = TRUE)
}

\keyword{tools }% use one of  RShowDoc("KEYWORDS")

