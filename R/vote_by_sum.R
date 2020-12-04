approval <- function(votes, mcan=1, fsep='\t', quiet = FALSE, ...) {
  votes <- prepare.votes(votes, fsep=fsep)
  x <- check.votes(votes, "approval", quiet = quiet)
  mcan <- check.nseats(mcan, ncol(x))
  res <- sum.votes(x)
  elected <- names(rev(sort(res))[1:mcan])
  result <- structure(list(elected=elected, totals=res, data=x,
  					invalid.votes=votes[setdiff(rownames(votes), rownames(x)),, drop = FALSE]), 
  					class="vote.approval")
  if(!quiet) print(summary(result))
  invisible(result)
}

plurality <- function(votes, mcan=1, fsep='\t', quiet = FALSE, ...) {
  votes <- prepare.votes(votes, fsep=fsep)
  x <- check.votes(votes, "plurality", quiet = quiet)
  mcan <- check.nseats(mcan, ncol(x))
  res <- sum.votes(x)
  elected <- names(rev(sort(res))[1:mcan])
  result <- structure(list(elected=elected, totals=res, data=x,
              invalid.votes=votes[setdiff(rownames(votes), rownames(x)),, drop = FALSE]), 
              class="vote.plurality")
  if(!quiet) print(summary(result))
  invisible(result)
}

score <- function(votes, mcan=1, max.score=NULL, larger.wins=TRUE, fsep='\t', quiet = FALSE, ...) {
  votes <- prepare.votes(votes, fsep=fsep)
  if(is.null(max.score) || max.score < 1) {
    max.score <- max(votes)
    warning("Invalid max.score. Set to observed maximum: ", max.score)
  }
  x <- check.votes(votes, "score", max.score, quiet = quiet)
  mcan <- check.nseats(mcan, ncol(x))
  res <- sum.votes(x)
  elected <- names(sort(res, decreasing=larger.wins)[1:mcan])
  result <- structure(list(elected=elected, totals=res, larger.wins=larger.wins,
                  data=x, invalid.votes=votes[setdiff(rownames(votes), rownames(x)),, drop = FALSE]), 
                  class="vote.score")
  if(!quiet) print(summary(result))
  invisible(result)
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

.summary.vote <- function(object, larger.wins=TRUE, reorder = TRUE) {
  df <- data.frame(Candidate=names(object$totals), Total=object$totals, 
                   Elected="", stringsAsFactors=FALSE)
  if(reorder) df <- df[order(df$Total, decreasing=larger.wins),]
  df[object$elected, "Elected"] <- "x"
  rownames(df) <- NULL
  df <- rbind(df, c('', sum(df$Total), ''))
  rownames(df)[nrow(df)] <- "Sum"
  attr(df, "align") <- c("l", "r", "c")
  attr(df, "number.of.votes") <- nrow(object$data)
  attr(df, "number.of.invalid.votes") <- nrow(object$invalid.votes)
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
  	print(kable(x, align = attr(x, "align"), ...))
  	cat("\nElected:", paste(x$Candidate[trimws(x$Elected) == "x"], collapse=", "), "\n\n")
}

print.summary.vote.approval <- function(x, ...) {
  cat("\nResults of Approval voting")
  cat("\n==========================")
  .print.summary.vote(x, ...)
}

view.vote.approval <- function(object, ...) {
  s <- summary(object)
  col_formatter <- formatter("span",
            style = x ~ style(background = ifelse(x %in% s$Candidate[trimws(s$Elected)=="x"], "lightgreen", "transparent")
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

