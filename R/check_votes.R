check.votes.stv <- function(record, ...) {
  if(! 1 %in% record) return(FALSE)
  z <- sort(diff(c(0, diff(sort(c(0, record))), 1)))
  return(z[length(record)] == 0 && z[length(record) + 1] == 1)
}

check.votes.condorcet <- function(record, ...) {
  if(any(! (record %in% 0:length(record))) || ! 1 %in% record) return(FALSE)
  # check indifferent votes
  r <- sort(record[record > 0])
  r <- c(r, length(r) + 1)
  d <- diff(c(0, r)) # zero means no increase in voting (i.e. the same ranking used for multiple candidates)
  cumszeros <- cumsum(d == 0) 
  # ranking after the gap caused by same ranking must continue as if there would be no gap
  first.after.zero <- which(diff(c(0, diff(c(0, cumszeros)))) < 0)
  if(length(first.after.zero) < 1) return(check.votes.stv(record, ...)) # no indifferent votes
  #if(all(record == c(1, 1, 3, 3, 0))) stop('')
  # check first gap
  if(d[first.after.zero[1]] != cumszeros[first.after.zero[1]] + 1) return(FALSE)
  # replace checked values in the r object with missing ranking
  r[(first.after.zero[1]- d[first.after.zero[1]]):(first.after.zero[1]- 1)] <- seq(r[(first.after.zero[1]- d[first.after.zero[1]])], length = d[first.after.zero[1]])
  # check remaining gaps
  if(length(first.after.zero) > 1) {
    for(i in 2:length(first.after.zero)){
      if((i < length(first.after.zero)) && d[first.after.zero[i]] != cumszeros[first.after.zero[i]] - sum(cumszeros[first.after.zero[-(1:(i-1))]]) + 1) 
        return(FALSE)
      r[(first.after.zero[i]- d[first.after.zero[i]]):(first.after.zero[i]- 1)] <- seq(r[(first.after.zero[i]- d[first.after.zero[i]])], length = d[first.after.zero[i]])
    }  
  }
  # Object r is the record where indifferent votes are replaced by the corresponding ranking.
  # It should now contain ordinary ranking (without gaps), just like for stv
  return(check.votes.stv(r, ...))
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

is.valid.vote <- function(x, method, ...) {
  return(apply(x, 1, paste0("check.votes.", method), ...))
}

check.votes <- function(x, ..., quiet = FALSE) {
  ok <- is.valid.vote(x, ...)
  if(any(!ok) && !quiet) 
    cat("Detected ", sum(!ok), "invalid votes. Number of valid votes is", sum(ok), ".\nUse invalid.votes(...) function to view discarded records.\n")
  return(x[ok, ])
}

assemble.args.for.check.score <- function(x, max.score=NULL, ...) {
  if(is.null(max.score)  || max.score < 1) max.score <- max(x)
  return(list(max.score=max.score))
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

