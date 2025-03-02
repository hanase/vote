stv <- function(votes, nseats = NULL, eps = 0.001, equal.ranking = FALSE, 
                fsep = '\t', ties = c("f", "b"), constant.quota = FALSE,
                quota.hare = FALSE, group.nseats = NULL, group.members = NULL,
                complete.ranking = FALSE, invalid.partial = FALSE,
                impute.missing = FALSE, verbose = FALSE, seed = 1234, 
                quiet = FALSE, digits = 3, ...) {
	###################################
	# Single transferable vote.
	# Adopted from Bernard Silverman's code.
	# The argument votes (matrix or data frame) contains the votes themselves.
	# Row i of the matrix contains the preferences of voter i
	# numbered 1, 2, .., r, 0,0,0,0, in some order
	# The columns of the matrix correspond to the candidates.
	# The dimnames of the columns are the names of the candidates; if these
	# are not supplied then the candidates are lettered A, B, C, ...
	#
	# If votes is a character string it is interpreted as a file name from which the
	# votes are to be read. A tab delimited file produced by excel
	# will be in the right format, with the candidate names in the first row.
	#
	# The argument nseats is number of candidates to be elected.
	#
	# If nseats is not supplied it will be assumed that the number of candidates
	# to be elected is half the number of candidates standing.
	#
	# If verbose=T, the progress of the count will be printed
    # The quiet argument if set to TRUE, it shuts all outputs off.
	#
	# The program was written by Bernard Silverman for the IMS in August 2002
    #
    # Equal ranking added November 2020.
	##################################
	
    
    if(verbose && !quiet) {
        cat("\nSingle transferable vote count")
        if(equal.ranking) cat(" with equal preferences")
        cat("\n===================================")
        if(equal.ranking) cat("==================")
        cat("\n")
    }
	# Prepare by finding names of candidates and setting up
	# vector w of vote weights and list of elected candidates
	
	votes <- prepare.votes(votes, fsep=fsep)
	nc <- ncol(votes)
	cnames <- colnames(votes)
	
	nseats <- check.nseats(nseats, nc, default=floor(nc/2), ...)
	
	# check groups (if used)
	use.marking <- FALSE
	if(!is.null(group.nseats)) { # number of candidates to be elected from a group 
	    if(is.null(group.members)) stop("If group.nseats is given, argument group.members must be used to mark members of the group.")
	    if(group.nseats > nseats) {
	        warning("group.nseats must be <= nseats. Adjusting group.nseats to ", nseats, ".")
	        group.nseats <- nseats
	    }
	    if(length(group.members) < group.nseats) {
	        warning("There are less group members than group.nseats. Adjusting group.nseats to ", length(group.members), ".")
	        group.nseats <- length(group.members)
	    }
	    if(!is.numeric(group.members)) { # convert names to indices
	        gind <- match(group.members, cnames)
	        if(any(is.na(gind))) {
	            warning("Group member(s) ", paste(group.members[is.na(gind)], collapse = ", "), " not found in the set of candidates, therefore removed from the group.")
	            gind <- gind[!is.na(gind)]
	        }
	        group.members <- gind
	    } 
	    # now group memebers are given as indices
	    group.members <- unique(group.members[group.members <= nc & group.members > 0])
	    use.marking <- TRUE
	} else{
	    group.nseats <- 0
	    group.members <- c()
	}
	
	elected <- NULL
	
	#
	# The next step is to remove invalid votes. A vote is invalid if
	# the preferences are not numbered in consecutively increasing order.
	# A warning is printed out for each invalid vote, but the votes are
	# not counted. If necessary, it is possible to correct errors in the
	# original x matrix.
	# If x is generated from an excel spreadsheet, then the jth vote will
	# be in row (j-1) of the spreadsheet.
	#
	
	if(verbose && !quiet) cat("Number of votes cast is", nrow(votes), "\n")
	corvotes <- votes
	corrected.votes <- NULL
	
	if(impute.missing){
	    to.impute <- votes == -1
	    corvotes[to.impute] <- 0
	}
	if(equal.ranking) 
	    corvotes <- correct.ranking(corvotes, partial = FALSE, quiet = quiet)
	else {
	    if(invalid.partial)
	        corvotes <- correct.ranking(corvotes, partial = TRUE, quiet = quiet)
	}
	if(impute.missing && any(to.impute)){
	    corvotes[to.impute] <- -1
	    corvotes <- impute.ranking(corvotes, equal.ranking = equal.ranking, quiet = quiet)
	    imputed <- votes
	    imputed[] <- NA
	    imputed[to.impute] <- corvotes[to.impute]
	    if(equal.ranking) # rerun the correction in case something got shifted out of range
	        corvotes <- correct.ranking(corvotes, partial = FALSE, quiet = TRUE)
	}
    x <-  check.votes(corvotes, "stv", equal.ranking = equal.ranking, quiet = quiet)

	corrected <- which(rowSums(corvotes != votes) > 0 & rownames(votes) %in% rownames(x))
	if(length(corrected) > 0) {
	    corrected.votes <- list(original = votes[corrected,])
	    if(impute.missing && any(to.impute))
	        corrected.votes$imputed <- imputed[corrected,]
	    corrected.votes <- c(corrected.votes, 
	                         list(new = corvotes[corrected, ], 
	                               index = as.numeric(corrected)))
	}
	nvotes <- nrow(x)
	if(is.null(nvotes) || nvotes == 0) stop("There must be more than one valid ballot to run STV.")
	w <- rep(1, nvotes)
	
	# Create elimination ranking
	tie.method <- match.arg(ties)
	tie.method.name <- c(f = "forwards", b = "backwards")
	otb <- ordered.tiebreak(x, seed) 

	if(use.marking) {
	    if(verbose && !quiet) {
	        cat("Number of reserved seats is", group.nseats, "\n")
	        cat("Eligible for reserved seats:",  paste(cnames[group.members], collapse = ", "), "\n")
	    }
	    group.nseats.orig <- group.nseats
	}
	
	# initialize results
	result.pref <- result.elect <- matrix(NA, ncol=nc, nrow=0, 
	                                       dimnames=list(NULL, cnames))
	result.quota <- result.ties <- c()
	orig.x <- x
	#
	# the main loop
	#
	if(verbose && !quiet) cat("\nList of 1st preferences in STV counts: \n")
	
	count <- 0
	while(nseats > 0) {
		#
		# calculate quota and total first preference votes
		#
	    count <- count + 1
	    A <- (x == 1)/rowSums(x == 1) # splits 1st votes if there are more than one first ranking per vote
	    A[is.na(A)] <- 0
        uij <- w * A
		vcast <- apply(uij, 2, sum)
		names(vcast) <- cnames
		if(!constant.quota || count == 1) 
		    # Quota calculation via either Hare (quota.hare is TRUE) or Droop (FALSE) method 
		    quota <- if(quota.hare) sum(vcast)/nseats + eps else sum(vcast)/(nseats + 1) + eps

		result.quota <- c(result.quota, quota)
		result.pref <- rbind(result.pref, vcast)
		result.elect <- rbind(result.elect, rep(0,nc))
		tie <- 0
		if(verbose && !quiet) {
		    cat("\nCount:" , count, "\n")
		    df <- data.frame(QUOTA=round(quota, 3), t(round(vcast[vcast != 0], 3)))
		    rownames(df) <- count
		    print(df)
		}
		
		# if leading candidate exceeds quota, declare elected and adjust weights
		# mark candidate for elimination in subsequent counting
		#
		# if the number of remaining candidates is smaller equal the number of seats, 
		# then select the one with the largest vcast, no matter if quota is exceeded
		#
		vmax <- max(vcast)
		ic <- (1:nc)[vcast == vmax]
		D <- colSums(abs(result.elect)) == 0 # set of hopeful candidates
		if(use.marking){
		    Dm <- D
		    Dm[-group.members] <- FALSE # set of hopeful marked candidates
		}
		if((vmax >= quota && !(! any(ic %in% group.members) && nseats == group.nseats) || 
		     (constant.quota && sum(D) <= nseats)) || # with constant.quota elected candidates may not need to reach quota
		     (use.marking && any(ic %in% group.members) && (sum(Dm) <= group.nseats || sum(D) - sum(Dm) == 0))) { 
		    if(use.marking && length(ic) > 1 && sum(Dm) <= group.nseats) # if a tiebreak, choose marked candidates if needed
		        ic <- ic[ic %in% group.members]
			if(length(ic) > 1) {# tie
			    ic <- solve.tiebreak(tie.method, result.pref, ic, otb, elim = FALSE)
			    tie <- 1
			    tie <- tie + (attr(ic, "ordered") == TRUE)
			    tie <- tie + (attr(ic, "sampled") == TRUE)
			}
			surplus <- if(vmax > quota) (vmax - quota)/vmax else 0
			index <- (x[, ic] == 1) # ballots where ic has the first preference
			w[index] <- uij[index, ic] * surplus # update weights
			if(equal.ranking) w[index] <- w[index]  + rowSums(uij[index, , drop = FALSE]) - uij[index, ic]
			# reduce number of seats available
			nseats <- nseats - 1
			if(use.marking && ic %in% group.members)
			    group.nseats <- group.nseats - 1
			elected <- c(elected, cnames[ic])
			result.elect[count,ic] <- 1
			if(verbose && !quiet) {
			    cat("Candidate", cnames[ic], "elected ")
			    if(tie > 0) {
			        cat("using", tie.method.name[tie.method])
			        if(tie == 2) cat(" & ordered")
			        cat(" tie-breaking method ")
			        if(tie > 2) cat("(sampled)")
			    }
			    cat("\n")
			}
		} else {
			# if no candidate reaches quota, mark lowest candidate for elimination
		    elim.select <- D
		    if(use.marking && (nseats == group.nseats || sum(Dm) <= group.nseats)) elim.select <- elim.select & !Dm
			vmin <- min(vcast[elim.select])
			ic <- (1:nc)[vcast == vmin & elim.select]
			if(length(ic) > 1) {# tie
			    ic <- solve.tiebreak(tie.method, result.pref, ic, otb, elim = TRUE)
			    tie <- 1
			    tie <- tie + (attr(ic, "ordered") == TRUE)
			    tie <- tie + (attr(ic, "sampled") == TRUE)
			}
			result.elect[count,ic] <- -1
			if(verbose && !quiet) {
			    cat("Candidate", cnames[ic], "eliminated ")
			    if(tie > 0) {
			        cat("using", tie.method.name[tie.method])
			        if(tie == 2) cat(" & ordered")
			        cat(" tie-breaking method ")
			        if(tie > 2) cat("(sampled)")
			    }
			    cat("\n")
			}
		}
		result.ties <- c(result.ties, tie)
		# shift votes for voters who voted for ic
		jp <- x[, ic]
		for(i in which(jp > 0)) {
			index <- x[i, ] > jp[i]
			x[i, index] <- x[i, index] - 1
		}
		x[, ic] <- 0
	}
	rownames(result.pref) <- 1:count
	result <- structure(list(elected = elected, preferences = result.pref, quotas = result.quota,
	               elect.elim = result.elect, equal.pref.allowed = equal.ranking, 
	               ties = translate.ties(result.ties, tie.method), data = orig.x, 
	               invalid.votes = votes[setdiff(rownames(votes), rownames(x)),,drop = FALSE],
	               corrected.votes = corrected.votes,
	               reserved.seats = if(use.marking) group.nseats.orig else NULL,
	               group.members = if(use.marking) group.members else NULL),
	               class = "vote.stv")
	if(!quiet) print(summary(result, complete.ranking = complete.ranking, digits = digits))
	invisible(result)
}

translate.ties <- function(ties, method){
    ties.char <- ifelse(ties == 0, "", method)
    ties.char <- ifelse(ties > 1, paste0(ties.char, "o"), ties.char)
    ties.char <- ifelse(ties > 2, paste0(ties.char, "s"), ties.char)
    names(ties.char) <- 1:length(ties)
    return(ties.char)
}

solve.tiebreak <- function(method, prefs, icans, ordered.ranking = NULL, elim = TRUE){
    if(method == "f") # forwards
        ic <- forwards.tiebreak(prefs, icans, elim = elim)
    else { # backwards
        ic <- backwards.tiebreak(prefs, icans, elim = elim)
    }
    # solve remaining ties by ordered ranking
    sampled <- FALSE
    ordered <- FALSE
    if(length(ic) > 1) {
        ic <- ic[if(elim) which.min(ordered.ranking[ic]) else which.max(ordered.ranking[ic])]
        sampled <- attr(ordered.ranking, "sampled")[ic]
        ordered <- TRUE
    }
    attr(ic, "sampled") <- sampled
    attr(ic, "ordered") <- ordered
    return(ic)
}

ordered.preferences <- function(vmat) {
    sapply(1:ncol(vmat), function(pref) apply(vmat, 2, function(f) sum(f == pref)))
}

ordered.tiebreak <- function(vmat, seed = NULL) {
    # Create elimination ranking using ordered tie-breaking
    # element ij in matrix nij is the number of j-th preferences
    # for candidate i
    nij <- ordered.preferences(vmat)
    # ranking for each preference
    nij.ranking <- apply(nij, 2, rank, ties.method="min")
    rnk <- nij.ranking[,1]
    dpl <- duplicated(rnk) | duplicated(rnk, fromLast = TRUE)
    sampled <- rep(FALSE, length(rnk))
    # resolve ranking duplicates by moving to the next column
    if(any(dpl)) {
        if(!is.null(seed)) set.seed(seed)
        for(pref in 1:ncol(vmat)) {
            if(! pref %in% rnk[dpl]) next
            j <- 2
            rnk[rnk == pref] <- NA
            while(any(is.na(rnk))) {
                # which candidates to resolve
                in.game <- is.na(rnk)
                # if we moved across all columns and there are 
                # still duplicates, determine the rank randomly
                if(j > ncol(nij)) { 
                    rnk[in.game] <- sample(sum(in.game)) + pref - 1
                    sampled <- sampled | in.game
                    break
                }
                rnk[in.game] <- rank(nij.ranking[in.game, j], ties.method="min") + pref - 1
                dplj <- rnk == pref & (duplicated(rnk) | duplicated(rnk, fromLast = TRUE))
                rnk[dplj] <- NA
                j <- j + 1
            }
        }
    }
    attr(rnk, "sampled") <- sampled
    return(rnk)
}

forwards.tiebreak <- function(prefs, icans, elim = TRUE) {
    if(!elim) prefs <- -prefs
    if(is.null(dim(prefs))) dim(prefs) <- c(1, length(prefs))
    rnk <- t(apply(prefs, 1, rank, ties.method="min"))
    if(is.null(dim(rnk))) dim(rnk) <- c(1, length(rnk))
    i <- 0
    icv <- rep(FALSE, ncol(prefs))
    while(i < nrow(rnk) && length(icans) > 1){
        icv[] <- FALSE
        icv[icans] <- TRUE
        i <- i + 1
        ic.rnk <- rnk[i, icans]
        icans <- which(icv & (rnk[i, ] == min(ic.rnk)))
    }
    return(icans)
}


backwards.tiebreak <- function(prefs, icans, elim = TRUE) {
    if(!elim) prefs <- -prefs
    if(is.null(dim(prefs))) dim(prefs) <- c(1, length(prefs))
    rnk <- t(apply(prefs, 1, rank, ties.method="min"))
    if(is.null(dim(rnk))) dim(rnk) <- c(1, length(rnk))
    i <- nrow(rnk)
    icv <- rep(FALSE, ncol(prefs))
    while(i > 1 && length(icans) > 1){
        icv[] <- FALSE
        icv[icans] <- TRUE
        i <- i - 1
        ic.rnk <- rnk[i, icans]
        icans <- which(icv & (rnk[i, ] == min(ic.rnk)))
    }
    return(icans)
}

summary.vote.stv <- function(object, ..., complete.ranking = FALSE, digits = 3) {
    decimalplaces <- function(x) {
        ifelse(abs(x - round(x)) > .Machine$double.eps^0.5,
               nchar(sub('^\\d+\\.', '', sub('0+$', '', as.character(x)))),
               0)
    }
  ncounts <- nrow(object$preferences)
  df <- data.frame(matrix(NA, nrow=ncol(object$preferences)+4, ncol=2*ncounts-1),
                   stringsAsFactors = FALSE)
  rownames(df) <- c("Quota", colnames(object$preferences), "Tie-breaks", "Elected", "Eliminated")
  colnames(df)[1] <- 1
  idxcols <- 1
  if(ncounts > 1) {
    colnames(df)[2:ncol(df)] <- paste0(rep(2:ncounts, each=2), c("-trans", ""))
    idxcols <- c(idxcols, seq(3,ncol(df), by=2))
  }
  df["Quota", idxcols] <- object$quotas
  df[2:(nrow(df)-3), idxcols] <- t(object$preferences)
  # calculate transfers
  pref <- object$preferences
  # remove quotas for winners and compute difference
  where.winner <- which(rowSums(object$elect.elim==1)==1)
  pref[where.winner,] <- pref[where.winner,] - object$elect.elim[where.winner,]*object$quotas[where.winner]
  if(ncounts > 1) {
    tmp <- t(object$preferences[2:nrow(object$preferences),] - pref[1:(nrow(pref)-1),])
    if(nrow(tmp) == 1) tmp <- as.numeric(tmp) # because of R weirdness with vectors and matrices (when there are just two counts)
    df[2:(nrow(df)-3), seq(2,ncol(df), by=2)] <- tmp
  }
  # format the right number of digits
  df[1:(nrow(df)-3),] <- apply(df[1:(nrow(df)-3),, drop = FALSE], 2, 
                               function(d) 
                                   ifelse(!is.na(d), 
                                        format(round(d, digits), 
                                                nsmall = min(digits, max(decimalplaces(round(d[!is.na(d)], digits))))), 
                                        ""))
  where.elim <- which(rowSums(object$elect.elim==-1)==1)
  cnames <- colnames(object$elect.elim)
  for(i in 1:ncounts) {
    if (i %in% where.winner) {
      elected <- cnames[which(object$elect.elim[i,]==1)]
      df["Elected", idxcols[i]] <- paste(elected, collapse=", ")
      for(can in elected) {
      	if(idxcols[i]+2 <=  ncol(df)) df[can, (idxcols[i]+2):ncol(df)] <- NA
      }
    }
    if (i %in%  where.elim) {
      eliminated <-cnames[which(object$elect.elim[i,]==-1)]
      df["Eliminated", idxcols[i]] <- paste(eliminated, collapse=", ")
      for(can in eliminated) {
      	if(idxcols[i]+2 <=  ncol(df)) df[can, (idxcols[i]+2):ncol(df)] <- NA
      }
    }
  }
  if(any(object$ties != "")) 
      df["Tie-breaks", seq(1, ncol(df), by = 2)] <- object$ties
  else df <- df[-which(rownames(df) == "Tie-breaks"),, drop = FALSE]
  if(!is.null(object$reserved.seats))
      rownames(df)[object$group.members + 1] <- paste0(rownames(df)[object$group.members + 1], "*")
      
  df[is.na(df)] <- ""
  class(df) <- c('summary.vote.stv', class(df))
  attr(df, "number.of.votes") <- nrow(object$data)
  attr(df, "number.of.invalid.votes") <- nrow(object$invalid.votes)
  attr(df, "number.of.candidates") <- ncol(object$preferences)
  attr(df, "number.of.seats") <- length(object$elected)
  if(!is.null(object$reserved.seats)) {
      attr(df, "reserved.seats") <- object$reserved.seats
      attr(df, "reservation.eligible") <- object$group.members
  }
  attr(df, "equal.pref.allowed") <- object$equal.pref.allowed
  if(complete.ranking) 
      attr(df, "complete.ranking")  <- complete.ranking(object)
  return(df)
}

print.summary.vote.stv <- function(x, ...) {
  cat("\nResults of Single transferable vote")
  if(attr(x, "equal.pref.allowed")) cat(" with equal preferences")
  cat("\n===================================")
  if(attr(x, "equal.pref.allowed")) cat("=======================")
  election.info(x)
  if(!is.null(attr(x, "reserved.seats"))){
      cat("Number of reserved seats:\t", attr(x, "reserved.seats"), "\n")
      cat("Eligible for reserved seats:\t", length(attr(x, "reservation.eligible")), "\n")
  }
  print(kable(x, align='r', ...))
  if(!is.null(attr(x, "complete.ranking"))) {
      cat("\nComplete Ranking")
      cat("\n================")
      print(kable(attr(x, "complete.ranking"), align = c("r", "l", "c")))
  }
  cat("\nElected:", paste(x['Elected', x['Elected',] != ""], collapse=", "), "\n\n")
}

"view" <- function(object, ...) UseMethod("view")
view.vote.stv <- function(object, ...) {
 s <- summary(object)
 formatter <- list(area(row=2:(nrow(s)-2), col=seq(1,ncol(s), by=2)) ~ color_text("red", "red"),
 					area(row=1, col=seq(1,ncol(s), by=2)) ~ color_text("blue", "blue")
 					#Quota=color_text("blue", "blue")
 					)
 formattable(s, formatter, ...)
}

image.vote.stv <- function(x, xpref = 2, ypref = 1, all.pref = FALSE, proportion = TRUE, ...) {
    voter <- rank <- NULL # to avoid warnings of the CRAN check
    xd <- x$data
    nc <- ncol(xd)
    if(all.pref) {
        nij <- ordered.preferences(xd)[nc:1,]
        image.plot(x = 1:nc, y = 1:nc, t(nij), axes = FALSE, xlab = "", ylab = "",
                   col = hcl.colors(12, "YlOrRd", rev = TRUE), ...)
        axis(3, at = 1:nc, labels = 1:nc)
        axis(2, at = 1:nc, labels = rev(colnames(xd)), tick = FALSE, las = 2)
        mtext("Ranking", side = 1, line = 0.5)
    } else {
        xdt <- data.table(xd)
        xdt[, voter := 1:nrow(xd)]
        xdtl <- melt(xdt, id.vars = "voter", variable.name = "candidate", value.name = "rank")
        xdtl <- xdtl[rank %in% c(xpref, ypref)]
        if(sum(duplicated(xdtl[, c("voter", "rank"), with = FALSE])) > 0)
            stop("Sorry, the image function is not available for ballots with equal preferences.")
        xdtw <- dcast(xdtl, voter ~ rank, value.var = "candidate")
        setnames(xdtw, as.character(xpref), "xpref")
        setnames(xdtw, as.character(ypref), "ypref")
        ctbl <- table(xdtw[, ypref], xdtw[, xpref])
        if(proportion) {
            ctbl <- ctbl/rowSums(ctbl)
            ctbl[is.na(ctbl)] <- 0
        }
        image.plot(x = 1:nc, y = 1:nc, t(ctbl[nc:1,]), axes = FALSE, xlab = "", ylab = "", 
              col = hcl.colors(12, "YlOrRd", rev = TRUE), ...)
        axis(2, at = nc:1, labels = rownames(ctbl), tick = FALSE, las = 1)
        text(1:nc, y = par("usr")[4], labels = colnames(ctbl), xpd = NA, srt = 45, adj = 0)
        mtext(paste("Preference", ypref), side = 4, line = 0.1)
        mtext(paste("Preference", xpref), side = 1, line = 0.5)
    }
}

plot.vote.stv <- function(x, xlab = "Count", ylab = "Preferences", point.size = 2, ...) {
    stopifnot(requireNamespace("ggplot2", quietly = TRUE))
    Count <- value <- selection <- i.value <- count.select <- Candidate <- i.Count <- NULL # to avoid warnings of the CRAN check
    # Plot evolution of the preferences
    # prepare data in the long format
    df <- data.table(x$preferences)
    df[, Count := 1:nrow(df)]
    dfl <- melt(df, id.vars = "Count", variable.name = "Candidate")
    dfl <- rbind(dfl, dfl[Count == 1][, Count := 0]) # add Count 0 with initial values
    
    # dataset for plotting the quota
    dfq <- data.table(Count = 1:length(x$quotas), value = x$quotas, Candidate = "Quota")
    
    # dataset for plotting points of elected and eliminated candidates
    dfe <- melt(data.table(Count = 1:nrow(x$elect.elim), x$elect.elim), id.vars = "Count", variable.name = "Candidate")
    dfe <- dfe[value != 0]
    dfe[, selection := ifelse(value > 0, "elected", "eliminated")]
    dfe <- dfe[dfl, value := i.value, on = c("Count", "Candidate")]
    
    # remove data after candidates are selected
    dfl[dfe, count.select := i.Count, on = "Candidate"]
    dfl <- dfl[is.na(count.select) | Count <= count.select]
    
    # create plots
    g <- ggplot2::ggplot(dfl, ggplot2::aes(x = as.factor(Count), y = value, color = Candidate, group = Candidate)) + ggplot2::geom_line()
    g <- g + ggplot2::geom_line(data = dfq, ggplot2::aes(x = as.factor(Count)), color = "black") + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)
    g <- g + ggplot2::geom_point(data = dfe, ggplot2::aes(shape = selection), size = point.size) + ggplot2::ylim(range(0, max(dfl$value, dfq$value)))
    g <- g + ggplot2::annotate(geom="text", x=as.factor(1), y=dfq[Count == 1, value], label="Quota", hjust = "right")
    g
}

"complete.ranking" <- function(object, ...) UseMethod("complete.ranking")
complete.ranking.vote.stv <- function(object, ...){
    result <- data.frame(Rank = 1:length(object$elected), Candidate = object$elected, Elected = "x")
    cand.in.play <- colSums(abs(object$elect.elim)) == 0
    if(any(cand.in.play)){ # for neither elected not eliminated candidates look at the position in the last round
        rnk <- rank(- object$preferences[nrow(object$preferences), cand.in.play], ties.method = "random")
        result <- rbind(result, data.frame(Rank = seq(max(result$Rank) + 1, length = length(rnk)),
                                           Candidate = colnames(object$preferences)[cand.in.play][order(rnk)],
                                           Elected = ""))
    }
    if(any(object$elect.elim < 0)) { # eliminated candidates
        elims <- c()
        for(i in rev(which(apply(object$elect.elim, 1, function(x) any(x < 0))))) { # iterate over counts backwards
            elims <- c(elims, colnames(object$elect.elim)[object$elect.elim[i,] < 0])
        }
        result <- rbind(result, data.frame(Rank = seq(max(result$Rank) + 1, length = length(elims)),
                                           Candidate = elims, Elected = ""))
    }
    return(result)
}

