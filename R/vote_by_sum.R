approval <- function(votes, mcan=1, fsep='\t', ...) {
  cat("\nApproval vote count")
  cat("\n===================\n")
  x <- prepare.votes(votes, fsep=fsep)
  cat("Number of votes cast is", nrow(x), "\n")
  x <- check.votes(x, "approval")
  res <- sum.votes(x)
  elected <- names(rev(sort(res))[1:mcan])
  cat("\nElected candidates are, in order of election: \n", paste(elected, collapse = ", "), "\n")
  return(structure(list(elected=elected, totals=res), class="vote.approval"))
}

plurality <- function(votes, mcan=1, fsep='\t', ...) {
  cat("\nPlurality vote count")
  cat("\n====================\n")
  x <- prepare.votes(votes, fsep=fsep)
  cat("Number of votes cast is", nrow(x), "\n")
  x <- check.votes(x, "plurality")
  res <- sum.votes(x)
  elected <- names(rev(sort(res))[1:mcan])
  cat("\nElected candidates are, in order of election: \n", paste(elected, collapse = ", "), "\n")
  return(structure(list(elected=elected, totals=res), class="vote.plurality"))
}

score <- function(votes, mcan=1, max.score=NULL, fsep='\t', ...) {
  cat("\nScore voting count")
  cat("\n==================\n")
  x <- prepare.votes(votes, fsep=fsep)
  cat("Number of votes cast is", nrow(x), "\n")
  if(is.null(max.score) || max.score < 1) {
    max.score <- max(x)
    warning("Invalid max.score. Set to observed maximum: ", max.score)
  }
  x <- check.votes(x, "score", max.score)
  res <- sum.votes(x)
  elected <- names(rev(sort(res))[1:mcan])
  cat("\nElected candidates are, in order of election: \n", paste(elected, collapse = ", "), "\n")
  return(structure(list(elected=elected, totals=res), class="vote.score"))
}

sum.votes <- function(votes) {
  vtot <- apply(votes, 2, sum)
  return (vtot)
}

.summary.vote <- function(object) {
  df <- data.frame(Candidate=names(object$totals), Total=object$totals, 
                   Elected="", stringsAsFactors=FALSE)
  df <- df[order(df$Total, decreasing=TRUE),]
  df[object$elected, "Elected"] <- "x"
  rownames(df) <- NULL
  df <- rbind(df, c('', sum(df$Total), ''))
  rownames(df)[nrow(df)] <- "Sum"
  return(df)
}

summary.vote.approval <- function(object) {
  df <- .summary.vote(object)
  class(df) <- c('summary.vote.approval', class(df))
  return(df)
}

.print.summary.vote <- function(x, ...) {
  print(kable(x, ...))
  cat("\nElected:", paste(x$Candidate[x$Elected == "x"], collapse=", "), "\n\n")
}

print.summary.vote.approval <- function(x, ...) {
  cat("\nApproval vote counts")
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


summary.vote.plurality <- function(object) {
  df <- .summary.vote(object)
  class(df) <- c('summary.vote.plurality', class(df))
  return(df)
}

print.summary.vote.plurality <- function(x, ...) {
  cat("\nPlurality voting system")
  .print.summary.vote(x, ...)
}

view.vote.plurality <- function(object, ...) 
  view.vote.approval(object, ...)


summary.vote.score <- function(object) {
  df <- .summary.vote(object)
  class(df) <- c('summary.vote.score', class(df))
  return(df)
}

print.summary.vote.score <- function(x, ...) {
  cat("\nPlurality voting system")
  .print.summary.vote(x, ...)
}

view.vote.score <- function(object, ...) 
  view.vote.approval(object, ...)

