check.votes.stv <- function(record, nc, ...) {
  z <- sort(diff(c(0, diff(sort(c(0, record))), 1)))
  return(z[nc] == 0 && z[nc + 1] == 1)
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

check.votes <- function(x, ...) {
  ok <- is.valid.vote(x, ...)
  if(any(!ok)) {
    cat("Excluded votes:\n")
    print(x[!ok,])
    cat("Number of valid votes is", sum(ok), "\n")
  }
  return(x[ok, ])
}

assemble.args.for.check.stv <- function(x, nc=NULL, ...) {
  if(is.null(nc)) nc <- ncol(x)
  return(list(nc=nc))
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

