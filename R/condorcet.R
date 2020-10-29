condorcet <- function(votes, fsep = '\t', ...) {
    votes <- prepare.votes(votes, fsep=fsep)
    nc <- ncol(votes)
    cnames <- colnames(votes)
    x <- check.votes(votes, "stv")
    mcan <- check.nseats(1, ncol(x))
    points <- matrix(0, nc, nc, dimnames = list(cnames, cnames))
    for(i in 1:(nc-1)){
        for(j in ((i+1):nc)){
            points[i,j] <- sum(x[,i] < x[,j]) > sum(x[,i] > x[,j])
            points[j,i] <- sum(x[,i] < x[,j]) < sum(x[,i] > x[,j])
        }
    }
    cdc.winner <- apply(points, 1, function(p) sum(p) == nc-1)
    cdc.loser <- apply(points, 1, function(p) sum(p) == 0)
    result <- structure(list(elected = if(sum(cdc.winner) > 0) cnames[which(cdc.winner)] else NULL, 
                             totals = points, data = x,
                             invalid.votes = votes[setdiff(rownames(votes), rownames(x)),, drop = FALSE],
                             loser = if(sum(cdc.loser) > 0) cnames[which(cdc.loser)] else NULL), 
                        class="vote.condorcet")
    print(summary(result))
    invisible(result)
}

summary.vote.condorcet <- function(object, ...) {
    df <- data.frame(object$totals, stringsAsFactors=FALSE)
    df$Total <- rowSums(object$totals)
    if(!is.null(object$elected)) {
        df$Winner <- rep("", nrow(df))
        df[object$elected, "Winner"] <- "x"
    }
    if(!is.null(object$loser)) {
        df$Loser <- rep("", nrow(df))
        df[object$loser, "Loser"] <- "x"
    }
    attr(df, "number.of.votes") <- nrow(object$data)
    attr(df, "number.of.invalid.votes") <- nrow(object$invalid.votes)
    attr(df, "number.of.candidates") <- nrow(object$totals)
    attr(df, "number.of.seats") <- length(object$elected)
    attr(df, "condorcet.winner") <- object$elected
    attr(df, "condorcet.loser") <- object$loser

    class(df) <- c('summary.vote.condorcet', class(df))
    return(df)
}

print.summary.vote.condorcet <- function(x, ...) {
    cat("\nResults of Condorcet voting")
    cat("\n===========================")
    election.info(x)
    print(kable(x, ...))
    if(is.null(attr(x, "condorcet.winner")))
        cat("\nThere is no condorcet winner (no candidate won over all other candidates).")
    else
        cat("\nCondorcet winner:", attr(x, "condorcet.winner"))
    if(is.null(attr(x, "condorcet.loser")))
        cat("\nThere is no condorcet loser (no candidate lost to all other candidates).\n\n")
    else
        cat("\nCondorcet loser:", attr(x, "condorcet.loser"), "\n\n")
}

view.vote.condorcet <- function(object, ...) 
    view.vote.approval(object, ...)

