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
\alias{complete.ranking}
\alias{complete.ranking.vote.stv}
\alias{ordered.preferences}
\alias{ordered.tiebreak}
\alias{remove.candidate}

\title{
Single Transferable Vote
}
\description{
Count votes using the single transferable voting method, also known as ranked choice voting or instant runoff. Raftery et al. (2021) describes the functionality in great detail.
}
\usage{
stv(votes, nseats = NULL, eps = 0.001, equal.ranking = FALSE, 
  fsep = '\t', ties = c("f", "b"), constant.quota = FALSE,
  quota.hare = FALSE, group.nseats = NULL, group.members = NULL,
  complete.ranking = FALSE, invalid.partial = FALSE,
  verbose = FALSE, seed = 1234, quiet = FALSE, digits = 3, \dots)

\method{summary}{vote.stv}(object, \dots, complete.ranking = FALSE, digits = 3)

\method{view}{vote.stv}(object, \dots)

\method{plot}{vote.stv}(x, xlab = "Count", ylab = "Preferences", point.size = 2, \dots)

\method{image}{vote.stv}(x, xpref = 2, ypref = 1, all.pref = FALSE, proportion = TRUE, \dots)

\method{complete.ranking}{vote.stv}(object, \dots)

correct.ranking(votes, partial = FALSE, quiet = FALSE)

remove.candidate(votes, can, quiet = TRUE)

ordered.tiebreak(vmat, seed = NULL)

ordered.preferences(vmat)
}

\arguments{
  \item{votes}{Matrix or data frame containing the votes. Rows correspond to the votes,  columns correspond to the candidates. If it is a character string it is interpreted as a file name from which the votes are to be read. See below for more details.}
  \item{nseats}{Number of candidates to be elected. By default it is half the number of candidates standing.}
  \item{eps}{Value added to the quota. I.e. the STV default Droop quota is computed as \cr
  	\code{number_of_first_preferences/(number_of_seats + 1) + eps}.}
  \item{equal.ranking}{If \code{TRUE} equal preferences are allowed, see below.}
  \item{fsep}{If \code{votes} is a file name, this argument gives the column separator in the file.}
  \item{ties}{Method used to break ties. By default the forwards tie-breaking is used (\dQuote{f}). Value \dQuote{b} invokes the backwards tie-breaking method, see O'Neill (2004).}
  \item{constant.quota}{Logical determining if the quota should be kept constant for all counts.}
  \item{quota.hare}{Changes quota calculation method from (default) Droop (\code{FALSE}) to Hare (\code{TRUE}). STV Hare quota method is computed as \cr
    \code{number_of_first_preferences/number_of_seats + eps}. The actual Hare formula would entail \code{eps = 0}.}
  \item{group.nseats}{Minimum number of candidates to be elected who are members of a given group. I.e., number of reserved seats for a subset of candidates defined by the \code{group.members} argument.}
  \item{group.members}{Vector of candidate names or indices who are eligible for reserved seats given by \code{group.nseats}. If it is a vector of indices, the order of candidates is assumed to correspond to the columns of \code{votes}.}
  \item{verbose}{Logical. If \code{TRUE} the progress of the count will be printed.}
  \item{seed}{Integer. Seed of the random number generator. Only used if there are ties that cannot be resolved by the tie-breaking method. If set to \code{NULL}, the RNG is not initialized.}
  \item{quiet}{If \code{TRUE} no output is printed.}
  \item{object, x}{Object of class \code{vote.stv}.}
  \item{complete.ranking}{Logical. If \code{TRUE} a complete ranking is generated conditioned on the number of seats \code{nseats}.}
  \item{invalid.partial}{Logical. If \code{TRUE}, partially invalid votes are corrected by removing ranking starting with the first incorrect rank, see Details below.}
  \item{digits}{How many significant digits to be used in the output table.}
  \item{xlab, ylab}{Labels of the x- and y-axis.}
  \item{point.size}{Size of the points in the plot.}
  \item{xpref, ypref}{Preference for the x- and y-axis, respectively, for showing the joined distribution of the votes. It is not used if \code{all.pref} is \code{TRUE}.}
  \item{all.pref}{Logical. If \code{TRUE} the marginal distribution of all preferences is shown in the image. Otherwise, the joint distribution of \code{xpref} and \code{ypref} is shown.}
  \item{proportion}{If \code{TRUE} the preferences are shown as proportions across the x-axis, otherwise raw vote counts are shown. Only available when \code{all.pref} is \code{FALSE}.}
  \item{\dots}{Additional arguments passed to the underlying functions.}
  \item{partial}{Logical. The same meaning as \code{invalid.partial}.}
  \item{can}{Vector of candiate name(s) or indices to be removed from the set of votes.}
  \item{vmat}{Matrix of valid votes.}
}
\details{
For a description of the single transferable vote system see \url{https://imstat.org/elections/single-transferable-voting-system/}.

The input data \code{votes} is structured as follows: Row \eqn{i} contains the preferences of voter \eqn{i} numbered \eqn{1, 2, \dots, r, 0,0,0,0}, in some order. The columns correspond to the candidates. The dimnames of the columns are the names of the candidates; if these are not supplied then the candidates are lettered A, B, C, \dots. If the dataset contains missing values (\code{NA}), they are replaced by zeros. 

By default the preferences are not allowed to contain duplicates per vote. However, if the argument \code{equal.ranking} is set to \code{TRUE}, votes are allowed to have the same ranking for multiple candidates. The desired format is such that for each preference \eqn{i} that does not have any duplicate, there must be exactly \eqn{i-1} preferences \eqn{j} with \eqn{0 < j < i}. For example, valid ordered preferences are \eqn{1,1,3,4,\dots}, or \eqn{1,2,3,3,3,6,\dots}, but NOT \eqn{1,1,2,3,\dots}, or NOT \eqn{1,2,3,3,3,5,6,\dots}. If the data contain such invalid votes, they are automatically corrected and a warning is issued by calling the \code{correct.ranking} function. 

If equal ranking is not alowed (\code{equal.ranking = FALSE}), the argument \code{invalid.partial} can be used to make ballots containing duplicates or gaps partially valid. If it is \code{TRUE}, a ballot is considered valid up to a preference that is in normal case not allowed. For example, ballots \eqn{1,2,3,4,4,6} or \eqn{1,2,3,5,6,7} would be both converted into \eqn{1,2,3,0,0,0}, because the ballots contain valid ranking only up to the third preference. 

The \code{correct.ranking} function does the above corrections for all records, regardless if they contain duplicates or not. Its argument \code{partial} determines if ballots are partially set to 0 (\code{TRUE}), or if it is complete re-ranking, as allowed when \code{equal.ranking = TRUE}. It can either be used by calling it explicitely, otherwise it is called by \code{stv} if \code{equal.ranking = TRUE} or \code{invalid.partial = TRUE}. The function is also called from within the \code{\link{condorcet}} function. The \code{remove.candidate} function removes the given candidate(s) and adjusts the ranked votes accordingly by calling the \code{correct.ranking} function.

By default, ties in the STV algorithm are resolved using the forwards tie-breaking method, see Newland and Briton (Section 5.2.5). Argument \code{ties} can be set to \dQuote{b} in order to use the backwards tie-breaking method, see O'Neill (2004). In addition, both methods are complemented by the following \dQuote{ordered} method: Prior to the STV election candidates are ordered by the number of 1st preferences. Equal ranks are resolved by moving to the number of 2nd preferences, then 3rd and so on. Remaining ties are broken by random draws. Such complete ordering is used to break any tie that cannot be resolved by the forwards or backwards method. If there is at least one tie during the processing, the output contains a row indicating in which count a tie-break happened (see the \code{ties} element in the Value section for an explanation of the symbols).

The ordered tiebreaking described above can be analysed from outside of the \code{stv} function by using the \code{ordered.tiebreak} function for viewing the a-priori ordering (the highest number is the best and lowest is the worst). Such ranking is produced by comparing candidates along the columns of the matrix returned by \code{ordered.preferences}.

The \code{plot} function shows the evolution of the total score for each candidate as well as the quota. The \code{image} function visualizes the joint distribution of two preferences (if \code{all.pref=FALSE}) as well as the marginal distribution of all preferences (if \code{all.pref=TRUE}). The joint distribution can be shown either as proportions (if \code{proportion=TRUE}) or raw vote counts (if \code{proportion=FALSE}).

Method \code{complete.ranking} produces a complete ranking of the candidates, conditioned on the number of seats selected in the \code{nseats} argument. It is called from the \code{summary} function if the \code{complete.ranking} argument is set to \code{TRUE}.

}

\value{
Function \code{stv} returns an object of class \code{vote.stv} which is a list with the following objects:
\item{elected}{Vector of names of the elected candidates in the order in which they were elected.}
\item{preferences}{Matrix of preferences. Columns correspond to the candidates and rows to the counts (i.e. voting rounds).}
\item{quotas}{Vector of quotas, one for each count.}
\item{elect.elim}{Matrix of the same shape as \code{preferences}. Value 1 means that the corresponding candidate was elected in that round; value -1 means an elimination.}
\item{equal.pref.allowed}{Input argument \code{equal.ranking}.}
\item{ties}{Character vector indicating if and what tie-break happened in each count. Possible values: \dQuote{} (no tie-break), \dQuote{f} (forward tie-breaking method only), \dQuote{fo} (forward method and ordered method), \dQuote{fos} (forward method and ordered method and sampling). If the backwards tie-breaking method is used, these values are \dQuote{b}, \dQuote{bo} and \dQuote{bos}.}
\item{data}{Input data (possibly corrected) with invalid votes removed.}
\item{invalid.votes}{Matrix of invalid votes that were removed from the original dataset.}
\item{corrected.votes}{List containing data about corrected votes if any. It has three elements, \code{original} (matrix of the raw votes that were corrected), \code{new} (the corrected values), \code{index} (index of those votes within the input \code{votes} dataset).}
\item{reserved.seats}{Number of reserved seats (\code{group.nseats}), or \code{NULL} if none.}
\item{group.members}{Vector of candidates eligible for reserved seats, or \code{NULL} if none.}

The \code{summary} function returns a data frame where columns are counts and transfers, and rows are the quota, the candidates, ties and the elected and eliminated candidates. Various attributes of the data frame contain more information about the results.

The \code{correct.ranking} function returns a matrix of votes with corrected preferences.

\code{remove.candidate} returns a matrix of votes with the given candidates removed and preferences corrected.

\code{complete.ranking} returns a data frame with a full ordering of the candidates.

\code{ordered.preferences} returns a matrix with number of preferences for each candidate and preference. These are the same values as seen by \code{image(..., all.pref = TRUE)}.

\code{ordered.tiebreak} returns the ranking for each candidate based on \code{ordered.preferences()}, with the highest number being the best and the lowest number being the worst. Its attribute \dQuote{sampled} indicates if there was random sampling involved in ranking each candidate. 
}

\references{
Raftery, A.E., Sevcikova, H. and Silverman, B.W. (2021). The vote Package: Single Transferable Vote and Other Electoral Systems in R. \href{https://arxiv.org/abs/2102.05801}{arXiv:2102.05801}.

R.A. Newland and F.S. Britton (1997). How to conduct an election by the Single Transferable Vote. ERS 3rd Edition. \url{http://www.rosenstiel.co.uk/stvrules/index.html}

\url{https://imstat.org/elections/single-transferable-voting-system/}

\url{https://en.wikipedia.org/wiki/Single_transferable_vote}

J.C. O'Neill (2004). Tie-Breaking with the Single Transferable Vote. Voting Matters, 18, 14-17. \url{https://www.votingmatters.org.uk/ISSUE18/I18P6.PDF}
}
\author{
Bernard Silverman, Hana Sevcikova, Adrian Raftery
}


\examples{
# Reproducing example from Wikipedia
# https://en.wikipedia.org/wiki/Single_transferable_vote#Example
# Uses eps=1
data(food_election)
stv.food <- stv(food_election, nseats = 3, eps = 1)
summary(stv.food)
\dontrun{
view(stv.food)}

# Example of the IMS Council voting
data(ims_election)
stv.ims <- stv(ims_election, nseats = 5)
\dontrun{
view(stv.ims)
plot(stv.ims)
image(stv.ims)

# write election results into a csv file
s <- summary(stv.ims)
write.csv(s, "myfile.csv")}

# produce complete ranking
summary(stv.ims, complete.ranking = TRUE)

\dontrun{
# Example of Dublin West 2002 elections
# https://en.wikipedia.org/wiki/Dublin_West#2002_general_election
data(dublin_west)
stv(dublin_west, nseats = 3, eps = 1)}

# Example of a small committee dataset
# with four candidates (C) and four
# voting committee members (uses tie-breaking)
votes <- data.frame(C1=c(3,2,1,3), C2=c(2,1,2,4),
                    C3=c(4,3,3,1), C4=c(1,4,4,2))
stv(votes, nseats = 2, verbose = TRUE)

# Example with equal ranking and correction
votes <- data.frame(C1=c(3,2,1,3), C2=c(1,1,2,0),
                    C3=c(4,3,3,1), C4=c(1,4,2,2))
stv(votes, nseats = 2, equal.ranking = TRUE)
# vote #3 was corrected by stv which used this data:
correct.ranking(votes, quiet = TRUE)

# Example of using reserved seats: 
# e.g. reserve two seats for students
stv(ims_election, nseats = 5, group.nseats = 2, 
    group.members = c("Declan", "Claire", "Oscar")) # students
    
# Example of removing candidates from original votes
stv(remove.candidate(ims_election, c("Jasper", "Tilmann")), nseats = 5)

# Example of accepting partially invalid ballots
res <- stv(ims_election, invalid.partial = TRUE)

# There are now 24 invalid votes instead of 29, 
# because 5 were corrected (ranking before the first 
# gap/tie is valid, after that it is 0)
corrected.votes(res)
invalid.votes(res)
}

\keyword{tools }% use one of  RShowDoc("KEYWORDS")

