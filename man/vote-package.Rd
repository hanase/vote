\name{vote-package}
\alias{vote-package}
\alias{vote}
\docType{package}
\title{
Election Vote Counting
}
\description{
Counting election votes and determining election results by different methods, including the single transferable vote (ranked choice), approval, score and plurality methods.
}
\details{
The main function of the package is called \code{\link{count.votes}}. If no specific method is passed, it decides on the basis of the available data which method is the most appropriate. Specific methods can also be invoked explicitly. 
The following voting methods are available:
\itemize{
\item \link{stv}: Single transferable vote (STV) where voters rank candidates in order. It is also known as ranked choice voting or or instant runoff.
\item \link{score}: Range voting where each voter gives each candidate a 
score within a specific range.
\item \link{approval}: Voters give each candidate one (approve) or zero (not approve).
\item \link{plurality}: Each voter chooses one candidate.
}
Output of these functions can be viewed using \code{summary} methods, or in a browser using \code{view} methods. The \code{summary} methods return a data frame which can be stored in a file, see Example below. Functions \code{\link{invalid.votes}} and \code{\link{valid.votes}} can be used to check the validity of ballots for the various methods.

Example datasets are included. The \code{\link{ims_election}} dataset contains anonymized ballots from a past Council election of the Institute of Mathematical Statistics (IMS) which uses the STV method. Modifications of this dataset are available (\code{\link{ims_approval}}, \code{\link{ims_score}}, \code{\link{ims_plurality}}) as examples of data required by the various methods. The \code{\link{food_election}} dataset taken from Wikipedia can be used to test the STV method.
}
\author{
Hana Sevcikova, Bernard Silverman, Adrian Raftery

Maintainer: Hana Sevcikova
}

\examples{
data(ims_election)
res <- count.votes(ims_election, method = "stv", mcan = 5)
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
