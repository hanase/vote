approval <- function(votes, mcan=1, fsep='\t', ...) {
  cat("\nApproval vote count")
  cat("\n===================\n")
  votes <- prepare.votes(votes, fsep=fsep)
  cat("Number of votes cast is", nrow(votes), "\n")
  x <- check.votes(votes, "approval")
  mcan <- check.nseats(mcan, ncol(x))
  res <- sum.votes(x)
  elected <- names(rev(sort(res))[1:mcan])
  cat("\nElected candidates are, in order of election: \n", paste(elected, collapse = ", "), "\n")
  invisible(structure(list(elected=elected, totals=res, data=x,
  					invalid.votes=nrow(votes)-nrow(x)), class="vote.approval"))
}

plurality <- function(votes, mcan=1, fsep='\t', ...) {
  cat("\nPlurality vote count")
  cat("\n====================\n")
  votes <- prepare.votes(votes, fsep=fsep)
  cat("Number of votes cast is", nrow(votes), "\n")
  x <- check.votes(votes, "plurality")
  mcan <- check.nseats(mcan, ncol(x))
  res <- sum.votes(x)
  elected <- names(rev(sort(res))[1:mcan])
  cat("\nElected candidates are, in order of election: \n", paste(elected, collapse = ", "), "\n")
  invisible(structure(list(elected=elected, totals=res, data=x,
  					invalid.votes=nrow(votes)-nrow(x)), class="vote.plurality"))
}

score <- function(votes, mcan=1, max.score=NULL, larger.wins=TRUE, fsep='\t', ...) {
  cat("\nScore voting count")
  cat("\n==================\n")
  votes <- prepare.votes(votes, fsep=fsep)
  cat("Number of votes cast is", nrow(votes), "\n")
  if(is.null(max.score) || max.score < 1) {
    max.score <- max(votes)
    warning("Invalid max.score. Set to observed maximum: ", max.score)
  }
  x <- check.votes(votes, "score", max.score)
  mcan <- check.nseats(mcan, ncol(x))
  res <- sum.votes(x)
  elected <- names(sort(res, decreasing=larger.wins)[1:mcan])
  cat("\nElected candidates are, in order of election: \n", paste(elected, collapse = ", "), "\n")
  invisible(structure(list(elected=elected, totals=res, larger.wins=larger.wins,
  						data=x, invalid.votes=nrow(votes)-nrow(x)), class="vote.score"))
}

sum.votes <- function(votes) {
  vtot <- apply(votes, 2, sum)
  return (vtot)
}

check.nseats <- function(nseats, ncandidates, default=1) {
	if(is.null(nseats)) return(default)
	if(nseats < 1 || nseats >= ncandidates) {
		nseats <- max(1, min(nseats, ncandidates-1))
		warning("Invalid number of candidates. Set to ", nseats)
	}
	return(nseats)
}

.summary.vote <- function(object, larger.wins=TRUE) {
  df <- data.frame(Candidate=names(object$totals), Total=object$totals, 
                   Elected="", stringsAsFactors=FALSE)
  df <- df[order(df$Total, decreasing=larger.wins),]
  df[object$elected, "Elected"] <- "x"
  rownames(df) <- NULL
  df <- rbind(df, c('', sum(df$Total), ''))
  rownames(df)[nrow(df)] <- "Sum"
  attr(df, "number.of.votes") <- nrow(object$data)
  attr(df, "number.of.invalid.votes") <- object$invalid.votes
  attr(df, "number.of.candidates") <- length(object$totals)
  attr(df, "number.of.seats") <- length(object$elected)
  return(df)
}

summary.vote.approval <- function(object, ...) {
  df <- .summary.vote(object)
  class(df) <- c('summary.vote.approval', class(df))
  return(df)
}

election.info <- function(x) {
	df <- data.frame(sapply(c("number.of.votes", "number.of.invalid.votes", "number.of.candidates", "number.of.seats"),
						function(a) attr(x, a)))
	rownames(df) <- c("Number of valid votes:", "Number of invalid votes:", "Number of candidates:", "Number of seats:")
	colnames(df) <- NULL
	print(df)
}

.print.summary.vote <- function(x, ...) {
	election.info(x)
  	print(kable(x, ...))
  	cat("\nElected:", paste(x$Candidate[x$Elected == "x"], collapse=", "), "\n\n")
}

print.summary.vote.approval <- function(x, ...) {
  cat("\nResults of Approval voting")
  cat("\n==========================")
  .print.summary.vote(x, ...)
}

view.vote.approval <- function(object, ...) {
  s <- summary(object)
  col_formatter <- formatter("span",
            style = x ~ style(background = ifelse(x %in% s$Candidate[s$Elected=="x"], "lightgreen", "transparent")
                              #width = "20px" # doesn't work
                              ))
  formattable(s, list(Candidate=col_formatter), ...)
}


summary.vote.plurality <- function(object, ...) {
  df <- .summary.vote(object)
  class(df) <- c('summary.vote.plurality', class(df))
  return(df)
}

print.summary.vote.plurality <- function(x, ...) {
  cat("\nResults of Plurality voting")
  cat("\n===========================")
  .print.summary.vote(x, ...)
}

view.vote.plurality <- function(object, ...) 
  view.vote.approval(object, ...)


summary.vote.score <- function(object, ...) {
  df <- .summary.vote(object, larger.wins=object$larger.wins)
  class(df) <- c('summary.vote.score', class(df))
  return(df)
}

print.summary.vote.score <- function(x, ...) {
  cat("\nResults of Score voting")
  cat("\n=======================")
  .print.summary.vote(x, ...)
}

view.vote.score <- function(object, ...) 
  view.vote.approval(object, ...)

