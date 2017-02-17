\name{vote-package}
\alias{vote-package}
\alias{vote}
\docType{package}
\title{
Election Vote Counting
}
\description{
Counting election votes via different methods, including single transferable votes, approval, score and plurality voting.
}
\details{
The main function of the package is called \code{\link{count.votes}}. If no specific method is passed, it decides on the basis of the available data which method is the most appropriate. Specific methods can be also invoked explicitely. 
The following voting methods are available:
\itemize{
\item \link{stv}: Single transferable vote (STV) where voters give candidates an order.
\item \link{score}: Range voting where for each candidate voters cast a number within a specific range.
\item \link{approval}: Voters give candidates one (approve) or zero (not approve).
\item \link{plurality}: Only one candidate can be chosen by each voter.
}

Example datasets are included. The \code{\link{ims_election}} dataset contains ballots from the Council election of the Institute of Mathematical Statistics (IMS) which uses the STV method. Modifications of this dataset are available (\code{\link{ims_approval}}, \code{\link{ims_score}}, \code{\link{ims_plurality}}) as examples of data required by the various methods. Dataset \code{\link{food_election}} taken from Wikipedia can be used to test the STV method.
}
\author{
Hana Sevcikova, Bernard Silverman, Adrian Raftery

Maintainer: Hana Sevcikova
}

\examples{
data(ims_election)
res <- count.votes(ims_election, method = "stv", mcan = 5, verbose = FALSE)
summary(res)
\dontrun{
view(res)}
}

\keyword{ package }