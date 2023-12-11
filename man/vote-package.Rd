\name{vote-package}
\alias{vote-package}
\alias{vote}
\docType{package}
\title{
Election Vote Counting
}
\description{
Counting election votes and determining election results by different methods, including the single transferable vote (ranked choice), approval, score, plurality, condorcet and two-round runoff methods. Details about the methods and package functions can be found in Raftery et al. (2021).
}
\details{
The main function of the package is called \code{\link{count.votes}}. If no specific method is passed, it decides on the basis of the available data which method is the most appropriate. Specific methods can also be invoked explicitly. 
The following voting methods are available:
\itemize{
\item \link{stv}: Single transferable vote (STV) where voters rank candidates in order. It is also known as ranked choice voting or instant runoff.
\item \link{score}: Range voting where each voter gives each candidate a 
score within a specific range.
\item \link{approval}: Voters give each candidate one (approve) or zero (not approve).
\item \link{plurality}: Each voter chooses one candidate.
\item \link{condorcet}: Voters rank candidates in order. The winner is determined in pairwise comparisons.
\item \link{tworound.runoff}: Two-round majority system with ranked ballots. If no candidate gets the majority, there is a run-off between the top two candidates. 
}
Output of these functions can be viewed using \code{summary} methods, or in a browser using \code{view} methods. The \code{summary} methods return a data frame which can be stored in a file, see Example below. Outputs of the \code{\link{stv}} method can be plotted in a graph. The joint and marginal distributions of ranked votes (for \code{\link{stv}}, \code{\link{condorcet}} and \code{\link{tworound.runoff}}) can be visualized in an image plot. 

Functions \code{\link{invalid.votes}}, \code{\link{valid.votes}} and \code{\link{corrected.votes}} can be used to check the validity of ballots for the various methods, including corrections made within the methods. Function \code{\link{correct.ranking}} can be used to make ballot corrections to ranked data, including ballots with equal preferences.

Example datasets are included. The \code{\link{ims_election}} dataset contains anonymized ballots from a past Council election of the Institute of Mathematical Statistics (IMS) which uses the STV method. Modifications of this dataset are available (\code{\link{ims_approval}}, \code{\link{ims_score}}, \code{\link{ims_plurality}}) as examples of data required by the various methods. The \code{\link{food_election}} dataset taken from Wikipedia can be used to test the STV method. Similarly, methods for ranked voting can be applied to the \code{\link{dublin_west}} dataset which contains election ballots from the 2002 election to the Dublin West constituency in Ireland. 
}
\author{
Hana Sevcikova, Bernard Silverman, Adrian Raftery

Maintainer: Hana Sevcikova
}

\references{
Raftery, A.E., Sevcikova, H. and Silverman, B.W. (2021). The vote Package: Single Transferable Vote and Other Electoral Systems in R. The R Journal, 13(2), 673-696. \doi{10.32614/RJ-2021-086}.
}

\examples{
data(ims_election)
res <- count.votes(ims_election, method = "stv", nseats = 5)
summary(res)

# View invalid votes
invalid.votes(res)

\dontrun{
# View results in a browser
view(res)

# Write election results into a csv file
s <- summary(res)
write.csv(s, "IMSstvresults.csv")}
}

\keyword{ package }
