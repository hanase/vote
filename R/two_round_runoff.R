tworound.runoff <- function(votes, fsep = '\t', seed = NULL, quiet = FALSE, ...) {
    do.rank <- function(x){
        res <- rep(0, length(x))
        res[x > 0] <- rank(x[x > 0], ties.method = "min")
        res
    }
    
    votes <- prepare.votes(votes, fsep=fsep)
    nc <- ncol(votes)
    cnames <- colnames(votes)
    x <-  check.votes(votes, "tworound.runoff", quiet = quiet)
    check.nseats(1, ncol(x))
    nvotes <- nrow(x)
    
    # first round
    res <- sum.votes(x == 1)
    winners <- res/nvotes > 0.5
    resoff <- NULL
    coin.toss.winner <- coin.toss.runoff <- seed.set <- FALSE
    if(sum(winners) > 1 || !any(winners)) { 
        # second round
        best <- res == max(res)
        if(sum(best) > 2) { # more than 2 candidates have the best score - sample two of them  
            if(!is.null(seed)) set.seed(seed)
            idx <- sample(which(best), 2)
            best[] <- FALSE
            best[idx] <- TRUE
            coin.toss.runoff <- TRUE
            seed.set <- TRUE
        }
        if(sum(best) < 2) {
            second.best <- which(res == max(res[res < max(res)])) # second best results
            if(length(second.best) > 1) {
                # more than 1 candidate has the second best score - sample one of them 
                if(!is.null(seed) && !seed.set) set.seed(seed)
                second.best <- sample(second.best, 1)
                coin.toss.runoff <- TRUE
                seed.set <- TRUE
            }
            best[second.best] <- TRUE
        }
        xroff <- x[,cnames[best]]
        xroff <- t(apply(xroff, 1, do.rank)) # shift ranking
        colnames(xroff) <- cnames[best]
        rownames(xroff) <- rownames(x)
        resoff <- sum.votes(xroff == 1)
        res.elect <- resoff
    } else {
        res.elect <- res
    }
    winner.index <- which(res.elect == max(res.elect))
    if(length(winner.index) > 1) {# tie
        if(!is.null(seed) && !seed.set) set.seed(seed)
        winner.index <- sample(winner.index, 1)
        coin.toss.winner <- TRUE
    }
    result <- structure(list(elected = names(res.elect)[winner.index], 
                             totals = res, totals2r = resoff, data = x, 
                             coin.toss.winner = coin.toss.winner, coin.toss.runoff = coin.toss.runoff,
                             invalid.votes = votes[setdiff(rownames(votes), rownames(x)),, drop = FALSE]),
                        class="vote.tworound.runoff")
    if(!quiet) print(summary(result))
    invisible(result)
}


summary.vote.tworound.runoff <- function(object, ...) {
    df <- .summary.vote(object, reorder = FALSE)
    df[, "Percent"] <- c(round(object$totals/sum(object$totals)*100, 1), 100)
    attr(df, "align") <- c(attr(df, "align"), "r")
    if(!is.null(object$totals2r)) {
        df$ROffTotal <- rep(0, nrow(df))
        df$ROffPercent <- rep(0, nrow(df))
        idx <- match(names(object$totals2r), df$Candidate)
        df[idx, "ROffTotal"] <- object$totals2r
        df[nrow(df), "ROffTotal"] <- sum(object$totals2r)
        df[idx, "ROffPercent"] <- round(object$totals2r/sum(object$totals2r)*100, 1)
        df[nrow(df), "ROffPercent"] <- 100
        attr(df, "align") <- c(attr(df, "align"), "r", "r")
    }
    df2 <- df[, c(setdiff(colnames(df), "Elected"), "Elected")] # reshuffle the order of columns
    # the above deleted the varios attributes, so re-attached
    for(att in setdiff(names(attributes(df)), c("names", "row.names", "class")))
        attr(df2, att) <- attr(df, att)
    attr(df2, "align") <- c(attr(df, "align")[-which(colnames(df) == "Elected")], "c")
    attr(df2, "coin.toss") <- c(winner = object$coin.toss.winner, runoff = object$coin.toss.runoff)
    class(df2) <- c('summary.vote.tworound.runoff', class(df))
    return(df2)
}

print.summary.vote.tworound.runoff <- function(x, ...) {
    cat("\nResults of two-round-runoff voting")
    cat("\n==================================")
    .print.summary.vote(x, ...)
    if(attr(x, "coin.toss")["winner"])
        cat("Winner chosen by a coin toss.\n")
    if(attr(x, "coin.toss")["runoff"])
        cat("Runoff candidates chosen by a coin toss.\n")
}

view.vote.tworound.runoff <- function(object, ...) 
    view.vote.approval(object, ...)

image.vote.tworound.runoff <- function(x, ...) 
    image.vote.stv(x, ...)
