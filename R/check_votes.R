check.nseats <- function(nseats = NULL, ncandidates, default=1, mcan = NULL) {
  if(!is.null(mcan)) {
    if(is.null(nseats)) nseats <- mcan
    warning("Argument mcan is deprecated and will be removed in the future. Use nseats instead. Using nseats = ", nseats)
  }
  if(is.null(nseats)) return(default)
  if(nseats < 1 || nseats >= ncandidates) {
    nseats <- max(1, min(nseats, ncandidates-1))
    warning("Invalid number of candidates. Set to ", nseats)
  }
  return(nseats)
}

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

correct.ranking <- function(votes, partial = FALSE, quiet = FALSE){
  do.rank <- function(x){
    res <- rep(0, length(x))
    res[x > 0] <- rank(x[x > 0], ties.method = "min")
    res
  }
  do.partial <- function(x) {
    d <- diff(sort(c(0, x))) # handle gaps in ranking
    if(any(d > 1)) {
      r <- x[names(d[d > 1][1])]
      x[x >= r] <- 0
    }
    d <- which(duplicated(x) & x > 0)
    if(length(d) > 0) x[x >= min(x[d])] <- 0
    return(x)
  }
  if(partial) {
    fct <- do.partial
    wrn <- "partially corrected (some may be still invalid)"
  } else {
    fct <- do.rank
    wrn <- "corrected to comply with the required format"
  }
  v <- t(apply(votes, 1, fct))
  dif <- rowSums(v != votes)
  if(any(dif > 0) && !quiet) warning("Votes ", paste(which(dif>0), collapse = ", "), " were ", wrn, ".\n")
  colnames(v) <- colnames(votes)
  rownames(v) <- rownames(votes)
  return(v)
}

remove.candidate <- function(votes, can, quiet = TRUE){
  if(!all(can %in% colnames(votes)) || (is.numeric(can) && !all(can %in% 1:nrow(votes))))
    stop("Value(s) of can not found in the set of candidates.")
  if(is.numeric(can))
    votes <- votes[,-can, drop = FALSE]
  else
    votes <- votes[,!colnames(votes) %in% can]
  return(correct.ranking(votes, quiet = quiet))
}

impute.ranking <- function(votes, equal.ranking = FALSE, quiet = TRUE){
  median.rank.for.zeros <- function(x){
    max.rank <- sum(x != 0)
    if(max.rank == length(x)) return(x)
    med <- median(seq(max.rank+1, by = 1, length = sum(x == 0)))
    x[x == 0] <- med
    return(x)
  }
  cans <- which(apply(votes, 2, function(x) any(x < 0)))
  voters.with.conflict <- apply(votes, 1, function(x) any(x < 0))
  if(length(cans) == 0){
    if(!quiet) cat("\nNothing to impute.")
    return(votes)
  }
  # fill in the median rank for all non-used ranks, i.e. 0 ranks
  votes.for.imp <- t(apply(votes, 1, median.rank.for.zeros))
  votes.for.imp[votes.for.imp == -1] <- NA # set spots to impute to NA
  
  # rank values to impute (medians across all voters)
  impvalues <- apply(votes.for.imp, 2, median, na.rm = TRUE)
  # compute means as well in case we need to solve ordering of duplicates
  impvalues.means <- apply(votes.for.imp, 2, mean, na.rm = TRUE)
  skipped <- c()
  
  # iterate over votes that need imputation
  for(i in which(voters.with.conflict)){
    where.to.impute <- which(is.na(votes.for.imp[i, ]))
    ranks.to.impute <- impvalues[where.to.impute]
    can.order <- order(ranks.to.impute)
    if(!equal.ranking && any(duplicated(ranks.to.impute))){
        # if we got median duplicates but equal ranking is not allowed, use mean for the ordering
        can.order <- order(impvalues.means[where.to.impute])
    }
    # iterate over ordered candidates
    previous.imputed <- 0
    for(ocan in can.order){
        this.rank <- as.integer(round(ranks.to.impute[ocan]))
        if(!equal.ranking && this.rank == previous.imputed)
            this.rank <- this.rank + 1 # avoid duplicates
        if(this.rank > sum(votes[i, ] > 0) + 1){
            # if the imputed rank is larger than the maximum preference plus one, set it to 0
            votes[i, where.to.impute[ocan]] <- 0
            skipped <- c(skipped, i)
            next
        }
        ranks.to.shift <- !is.na(votes.for.imp[i, ]) & votes[i, ] >= this.rank & 
            ! seq_len(ncol(votes)) %in% where.to.impute
        votes[i, ranks.to.shift] <- votes[i, ranks.to.shift] + 1
        votes[i, where.to.impute[ocan]] <- this.rank
        previous.imputed <- this.rank
    }
  }
  if(!quiet) {
    cat("\nMedian ranks used for imputation into", sum(voters.with.conflict), "vote(s) :\n")
    print(data.frame(matrix(round(impvalues[cans], 1), byrow = TRUE, nrow = 1, 
                     dimnames = list("rank", colnames(votes)[cans]))))
    skipped <- unique(skipped)
    if(length(skipped) > 0)
      warning("Imputation skipped for votes ", paste(skipped, collapse = ", "), " due to non-missing preferences being too small.")
  }
  return(votes)
}
