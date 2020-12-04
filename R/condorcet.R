condorcet <- function(votes, runoff = FALSE, fsep = '\t', quiet = FALSE, ...) {
    compare.two.candidates <- function(v1, v2) {
        i.wins <- sum(v1 < v2)
        j.wins <- sum(v1 > v2)
        c(i.wins > j.wins, i.wins < j.wins)
    }
    compute.wins <- function(dat, ncan, cnam){
        p <- matrix(0, ncan, ncan, dimnames = list(cnam, cnam))
        for(i in 1:(ncan-1)){
            for(j in ((i+1):ncan)){
                pair.run <- compare.two.candidates(dat[,i], dat[,j])
                p[i,j] <- pair.run[1]
                p[j,i] <- pair.run[2]
            }
        }
        p
    }
    votes <- prepare.votes(votes, fsep=fsep)
    nc <- ncol(votes)
    cnames <- colnames(votes)
    x <- check.votes(correct.ranking(votes, quiet = quiet), "condorcet", quiet = quiet)
    mcan <- check.nseats(1, ncol(x))
    x2 <- x
    x2[x2 == 0] <- max(x2) + 1 # give not-ranked candidates the worst ranking
    points <- compute.wins(x2, nc, cnames)
    cdc.winner <- apply(points, 1, function(p) sum(p) == nc-1)
    cdc.loser <- apply(points, 1, function(p) sum(p) == 0)
    runoff.winner <- ro.part <- ro.part.first <- NULL
    if(sum(cdc.winner) == 0 && runoff) { # run-off
        nwins <- rowSums(points)
        winner.exists <- FALSE
        cand.names <- cnames
        ncro <- nc 
        while(!winner.exists) {
            most.wins <- nwins == max(nwins)
            if(sum(most.wins) < 2) # second most wins
                most.wins <- most.wins | nwins == max(nwins[nwins < max(nwins)])
            ro.part <- cand.names[most.wins]
            if(is.null(ro.part.first)) ro.part.first <- ro.part # keep the list of the original run-off participants
            if(length(ro.part) == ncro) break # run-off must have less candidates than the original set
            if(sum(most.wins) == 2) { # run-off between two candidates 
                pair.run <- compare.two.candidates(x2[,which(most.wins)[1]], x2[,which(most.wins)[2]])
                runoff.winner <- cand.names[which(most.wins)[which(pair.run == TRUE)]]
            } else { # run-off between more than two candidates 
                x3 <- x2[, most.wins]
                p.runoff <- compute.wins(x3, ncol(x3), colnames(x3))
                runoff.winner <- colnames(x3)[apply(p.runoff, 1, function(p) sum(p) == ncol(x3)-1)]
            }
            if(length(runoff.winner) > 0) {
                winner.exists <- TRUE
                break
            }
            if(sum(most.wins) == 2) break
            nwins <- rowSums(p.runoff)
            x2 <- x3
            cand.names <- colnames(x2)
            ncro <- ncol(x2)
        }
    }
    result <- structure(list(elected = if(sum(cdc.winner) > 0) cnames[which(cdc.winner)] else NULL, 
                             totals = points, data = x,
                             invalid.votes = votes[setdiff(rownames(votes), rownames(x)),, drop = FALSE],
                             loser = if(sum(cdc.loser) > 0) cnames[which(cdc.loser)] else NULL,
                             runoff.winner = if(length(runoff.winner) > 0) runoff.winner else NULL, 
                             runoff.participants = ro.part.first), 
                        class="vote.condorcet")
    if(!quiet) print(summary(result))
    invisible(result)
}

summary.vote.condorcet <- function(object, ...) {
    df <- data.frame(object$totals, stringsAsFactors=FALSE)
    df$Total <- rowSums(object$totals)
    attr(df, "align") <- rep("r", ncol(df))
    if(!is.null(object$elected)) {
        df$Winner <- rep("", nrow(df))
        df[object$elected, "Winner"] <- "x"
        attr(df, "align") <- c(attr(df, "align"), "c")
    }
    if(!is.null(object$loser)) {
        df$Loser <- rep("", nrow(df))
        df[object$loser, "Loser"] <- "x"
        attr(df, "align") <- c(attr(df, "align"), "c")
    }
    if(!is.null(object$runoff.participants)) {
        df$Runoff <- rep("", nrow(df))
        df[setdiff(object$runoff.participants, object$runoff.winner), "Runoff"] <- "o"
        if(!is.null(object$runoff.winner))
            df[object$runoff.winner, "Runoff"] <- "x"
        attr(df, "align") <- c(attr(df, "align"), "c")
    }
    attr(df, "number.of.votes") <- nrow(object$data)
    attr(df, "number.of.invalid.votes") <- nrow(object$invalid.votes)
    attr(df, "number.of.candidates") <- nrow(object$totals)
    attr(df, "number.of.seats") <- length(object$elected)
    attr(df, "condorcet.winner") <- object$elected
    attr(df, "condorcet.loser") <- object$loser
    attr(df, "runoff.winner") <- object$runoff.winner
    attr(df, "runoff.participants") <- object$runoff.participants

    class(df) <- c('summary.vote.condorcet', class(df))
    return(df)
}

print.summary.vote.condorcet <- function(x, ...) {
    cat("\nResults of Condorcet voting")
    cat("\n===========================")
    election.info(x)
    print(kable(x, align = attr(x, "align"), ...))
    if(is.null(attr(x, "condorcet.winner")))
        cat("\nThere is no condorcet winner (no candidate won over all other candidates).")
    else
        cat("\nCondorcet winner:", attr(x, "condorcet.winner"))
    if(is.null(attr(x, "condorcet.loser")))
        cat("\nThere is no condorcet loser (no candidate lost to all other candidates).")
    else
        cat("\nCondorcet loser:", attr(x, "condorcet.loser"))
    if(!is.null(attr(x, "runoff.winner")))
        cat("\nRun-off winner:", attr(x, "runoff.winner"))
    cat("\n\n")
}

view.vote.condorcet <- function(object, ...) 
    view.vote.approval(object, ...)

image.vote.condorcet <- function(x, ...) 
    image.vote.stv(x, ...)

