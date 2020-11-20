check.votes.stv <- function(record, equal.ranking = FALSE, ...) {
  if(any(! (record %in% 0:length(record))) || ! 1 %in% record) return(FALSE)
  if(!equal.ranking){
    z <- sort(diff(c(0, diff(sort(c(0, record))), 1)))
    return(z[length(record)] == 0 && z[length(record) + 1] == 1)
  }
  # check for equal ranking
  return(all(record[record > 0] == rank(record[record > 0], ties.method = "min")))
}

check.votes.condorcet <- function(record, ...) {
  if(any(! (record %in% 0:length(record))) || ! 1 %in% record) return(FALSE)
  should.be <- rank(record[record > 0], ties.method = "min")
  return(all(record[record > 0] == should.be))
}

check.votes.approval <- function(record, ...) {
  return(all(record %in% c(1,0)))
}

check.votes.plurality <- function(record, ...) {
  return(all(record %in% c(1,0)) && sum(record) == 1)
}

check.votes.score <- function(record, max.score, ...) {
  return(all(record %in% 0:max.score))
}

check.votes.tworound.runoff <- function(record, ...) {
  return(check.votes.stv(record, ...))
}

is.valid.vote <- function(x, method, ...) {
  return(apply(x, 1, paste0("check.votes.", method), ...))
}

check.votes <- function(x, ..., quiet = FALSE) {
  ok <- is.valid.vote(x, ...)
  if(any(!ok) && !quiet) 
    cat("Detected ", sum(!ok), "invalid votes. Number of valid votes is", sum(ok), ".\nUse invalid.votes(...) function to view discarded records.\n")
  return(x[ok, ])
}

assemble.args.for.check.score <- function(x, max.score = NULL, ...) {
  if(is.null(max.score)  || max.score < 1) max.score <- max(x)
  return(list(max.score=max.score))
}

assemble.args.for.check.stv <- function(x, equal.ranking = FALSE, ...) {
  return(list(equal.ranking=equal.ranking))
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

correct.ranking <- function(votes, quiet = FALSE){
  do.rank <- function(x){
    res <- rep(0, length(x))
    res[x > 0] <- rank(x[x > 0], ties.method = "min")
    res
  }
  v <- t(apply(votes, 1, do.rank))
  dif <- rowSums(v != votes)
  if(any(dif > 0) && !quiet) warning("Votes ", paste(which(dif>0), collapse = ", "), " were corrected to comply with the required format.\n")
  colnames(v) <- colnames(votes)
  rownames(v) <- rownames(votes)
  return(v)
}
