tworound.runoff <- function(votes, fsep = '\t', quiet = FALSE, ...) {
    votes <- prepare.votes(votes, fsep=fsep)
    nc <- ncol(votes)
    cnames <- colnames(votes)
    x <-  check.votes(votes, "tworound.runoff", quiet = quiet)
    mcan <- check.nseats(1, ncol(x))
    nvotes <- nrow(x)
    
    # first round
    res <- sum.votes(x == 1)
    winners <- res/nvotes > 0.5
    resoff <- NULL
    if(sum(winners) > 1 || !any(winners)) { 
        # second round
        best <- res == max(res)
        if(sum(best) < 2)
            best <- best | res == max(res[res < max(res)]) # second best results
        xroff <- x[,cnames[best]]
        xroff <- t(apply(xroff, 1, rank)) # shift ranking
        resoff <- sum.votes(xroff == 1)
        elected <- names(rev(sort(resoff)))[1]
    } else {
        elected <- names(rev(sort(res)))[1]
    }
    result <- structure(list(elected = elected, 
                             totals = res, totals2r = resoff, data = x,
                             invalid.votes = votes[setdiff(rownames(votes), rownames(x)),, drop = FALSE]),
                        class="vote.tworound.runoff")
    if(!quiet) print(summary(result))
    invisible(result)
}


summary.vote.tworound.runoff <- function(object, ...) {
    df <- .summary.vote(object)
    df[, "Percent"] <- c(round(object$totals/sum(object$totals)*100, 1), 100)
    attr(df, "align") <- c(attr(df, "align"), "r")
    if(!is.null(object$totals2r)) {
        df$Runoff <- rep(0, nrow(df))
        idx <- match(names(object$totals2r), df$Candidate)
        df[idx, "Runoff"] <- round(object$totals2r/sum(object$totals2r)*100, 1)
        df[nrow(df), "Runoff"] <- 100
        attr(df, "align") <- c(attr(df, "align"), "r")
    }
    df2 <- df[, c(setdiff(colnames(df), "Elected"), "Elected")] # reshuffle the order of columns
    # the above deleted the varios attributes, so re-attached
    for(att in setdiff(names(attributes(df)), c("names", "row.names", "class")))
        attr(df2, att) <- attr(df, att)
    attr(df2, "align") <- c(attr(df, "align")[-which(colnames(df) == "Elected")], "c")
    class(df2) <- c('summary.vote.tworound.runoff', class(df))
    return(df2)
}

print.summary.vote.tworound.runoff <- function(x, ...) {
    cat("\nResults of two-round-runoff voting")
    cat("\n==================================")
    .print.summary.vote(x, ...)
}

view.vote.tworound.runoff <- function(object, ...) 
    view.vote.approval(object, ...)

image.vote.tworound.runoff <- function(x, ...) 
    image.vote.stv(x, ...)
