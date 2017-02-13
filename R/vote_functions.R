sum.votes <- function(votes) {
  vtot <- apply(votes, 2, sum)
  return (vtot)
}

prepare.votes <- function(data, fsep="\n") {
  if(is.character(data)) {
    data <- read.table(file = data, header = TRUE, sep=fsep, row.names = NULL)
  }
  x <- as.matrix(data)
  x[is.na(x)] <- 0
  if(is.null(colnames(x))) {
    warning("Candidate names not supplied, dummy names used instead.")
    colnames(x) <- LETTERS[1:ncol(x)]
  }
  rownames(x) <- 1:nrow(x)
  return(x)
}

approval <- function(votes, mcan=NULL, fsep='\t') {
  x <- prepare.votes(votes, fsep=fsep)
  cat("Number of votes cast is", nrow(x), "\n")
  x <- check.votes(x, "approval")
  res <- sum.votes(x)
  elected <- names(rev(sort(res))[1:mcan])
  cat("\nElected candidates are, in order of election: \n", paste(elected, collapse = ", "), "\n")
  return(structure(list(elected=elected, totals=res), class="vote.approval"))
}

summary.vote.approval <- function(object) {
  df <- data.frame(rev(sort(object$totals[])
  class(df) <- c('summary.vote.stv', class(df))
  return(df)
}

check.votes.approval <- function(record) {
  return(all(record %in% c(1,0)))
}

check.votes <- function(x, method, ...) {
  ok <- rep(TRUE, nrow(x))
  ok <- apply(x, 1, paste0("check.votes.", method), ...)
  if(any(!ok)) {
    cat("Problematic votes which will be excluded:\n")
    print(x[!ok,])
  }
  return(x[ok, ])
}