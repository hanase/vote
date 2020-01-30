\name{ims_election}
\alias{ims_election}
\alias{ims_stv}
\alias{ims_score}
\alias{ims_approval}
\alias{ims_plurality}

\docType{data}
\title{
Datasets on IMS Election
}
\description{
Datasets containing anonymized votes for a past Council election of the Institute of Mathematical Statistics (IMS). The dataset \code{ims_election} (named also \code{ims_stv}) is the original dataset used with single transferable vote, where candidate names have been changed. Each of the other datasets is a  modified version of the original data to be used as an example for each of the other voting methods. 
}
\usage{
data("ims_election")
data("ims_stv")

data("ims_approval")
data("ims_score")
data("ims_plurality")
}

\format{
  A data frame with 620 observations and 10 candidates (names were made up). Each record corresponds to one ballot. Values depend on the voting method. The IMS Council voting is done using the STV method, and thus the \code{ims_election} dataset contains ballots with candidates being ranked between 1 and 10 with zeros allowed.
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
The original dataset (which was randomized and anonymized, with write-in votes removed) was obtained from the the Institute of Mathematical Statistics. 
}
\references{
\url{https://imstat.org/elections/single-transferable-voting-system/}
}
\examples{
data(ims_election)
head(ims_election)
}
\keyword{datasets}
