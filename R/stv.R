stv <- function(votes, mcan = NULL, eps = 0.001, equal.ranking = FALSE, 
                fsep = '\t', ties = c("f", "b"), constant.quota = FALSE,
                verbose = FALSE, seed = 1234, quiet = FALSE, digits = 3, ...) {
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
	# The argument mcan is number of candidates to be elected.
	#
	# If mcan is not supplied it will be assumed that the number of candidates
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
	
	mcan <- check.nseats(mcan, nc, default=floor(nc/2))	
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
	if(equal.ranking) 
	    votes <- correct.ranking(votes, quiet = quiet)
    x <-  check.votes(votes, "stv", equal.ranking = equal.ranking, quiet = quiet)
	
	nvotes <- nrow(x)
	w <- rep(1, nvotes)
	
	# Create elimination ranking using forwards tie-breaking
	tie.method <- match.arg(ties)
	tie.method.name <- c(f = "forwards", b = "backwards")
	
	if(tie.method == "f") {
	    ftb <- ranking.forwards.tiebreak(x, nc, seed)
	    tie.elim.rank <- ftb[[1]]
	    sampled <- ftb[[2]]
	} else {
	    if(!is.null(seed)) set.seed(seed)
	}
	
	# initialize results
	result.pref <- result.elect <- matrix(NA, ncol=nc, nrow=0, 
	                                       dimnames=list(NULL, cnames))
	result.quota <- c()
	orig.x <- x
	#
	# the main loop
	#
	if(verbose && !quiet) cat("\nList of 1st preferences in STV counts: \n")
	
	count <- 0
	first.vcast <- NULL
	while(mcan > 0) {
		#
		# calculate quota and total first preference votes
		#
	    count <- count + 1
	    A <- (x == 1)/rowSums(x == 1) # splits 1st votes if there are more than one first ranking per vote
	    A[is.na(A)] <- 0
        wD <- w * A
		vcast <- apply(wD, 2, sum)
		names(vcast) <- cnames
		if(!constant.quota || count == 1)
		    quota <- sum(vcast)/(mcan + 1) + eps
		result.quota <- c(result.quota, quota)
		result.pref <- rbind(result.pref, vcast)
		result.elect <- rbind(result.elect, rep(0,nc))
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
		if(vmax >= quota || sum(vcast > 0) <= mcan) {
			ic <- (1:nc)[vcast == vmax]
			tie <- FALSE
			if(length(ic) > 1) {# tie
			    if(tie.method == "f") 
			        iic <- which.max(tie.elim.rank[ic])
			    else {
			        iica <- backwards.tiebreak(result.pref, ic, elim = FALSE)
			        iic <- iica[[1]]
			        sampled <- iica[[2]]
			    }
			    ic <- ic[iic]
			    tie <- TRUE
			}
			index <- (x[, ic] == 1)
			surplus <- if(sum(vcast > 0) > mcan) (vmax - quota)/vmax else 0
			# The two ways of computing weights should be equivalent, but the first way could be faster.
			w[index] <- if(!equal.ranking) w[index] * surplus
			                else rowSums(wD[index, ]) - wD[index, ic] + wD[index, ic] * surplus
			mcan <- mcan - 1
			elected <- c(elected, cnames[ic])
			result.elect[count,ic] <- 1
			if(verbose && !quiet) {
			    cat("Candidate", cnames[ic], "elected ")
			    if(tie) {
			        cat("using", tie.method.name[tie.method], "tie-breaking method ")
			        if(sampled[ic]) cat("(sampled)")
			    }
			    cat("\n")
			}
		} else {
			# if no candidate reaches quota, mark lowest candidate for elimination
            #
		    if(is.null(first.vcast)) { # first time elimination happens
		        # keep initial count of first preferences after redistributing
                first.vcast <- apply(w * (x == 1), 2, sum)
                names(first.vcast) <- cnames
                first.vcast[colSums(result.elect) > 0] <- 1 # don't allow zeros for already elected candidates
            }
		    # if there are candidates with zero first votes, eliminate those first
		    zero.eliminated <- FALSE
		    if(any(first.vcast == 0)) { 
		        vmin <- min(first.vcast)
		        ic <- (1:nc)[first.vcast == vmin]
		        zero.eliminated <- TRUE
		    } else {
			    vmin <- min(vcast[vcast > 0])
			    ic <- (1:nc)[vcast == vmin]
		    }
			tie <- FALSE
			if(length(ic) > 1) {# tie
			    if(tie.method == "f")
			        iic <- which.min(tie.elim.rank[ic])
			    else { # backwards
			        iica <- backwards.tiebreak(result.pref, ic)
			        iic <- iica[[1]]
			        sampled <- iica[[2]]
			    }
			    ic <- ic[iic]
			    tie <- TRUE
			}
			result.elect[count,ic] <- -1
			if(verbose && !quiet) {
			    cat("Candidate", cnames[ic], "eliminated ")
			    if(zero.eliminated) cat("due to zero first preferences ")
			    if(tie) {
			        cat("using", tie.method.name[tie.method], "tie-breaking method ")
			        if(sampled[ic]) cat("(sampled)")
			    }
			    cat("\n")
			}
			if(zero.eliminated)	first.vcast[ic] <- 1
		}
		# redistribute votes
		for(i in (1:nvotes)) {
			jp <- x[i, ic]
			if(jp > 0) {
				index <- (x[i, ] > jp)
				x[i, index] <- x[i, index] - 1
				x[i, ic] <- 0
			} 
		}
	}
	rownames(result.pref) <- 1:count
	result <- structure(list(elected = elected, preferences=result.pref, quotas=result.quota,
	               elect.elim=result.elect, equal.pref.allowed = equal.ranking, data=orig.x, 
	               invalid.votes=votes[setdiff(rownames(votes), rownames(x)),,drop = FALSE]), 
	               class="vote.stv")
	if(!quiet) print(summary(result, digits = digits))
	invisible(result)
}

ranking.forwards.tiebreak <- function(x, nc, seed = NULL) {
    # Create elimination ranking using forwards tie-breaking
    # element ij in matrix nij is the number of j-th preferences
    # for candidate i
    nij <- sapply(1:nc, function(pref) apply(x, 2, function(f) sum(f == pref)))
    # ranking for each preference
    nij.ranking <- apply(nij, 2, rank, ties.method="min")
    rnk <- nij.ranking[,1]
    dpl <- duplicated(rnk) | duplicated(rnk, fromLast = TRUE)
    sampled <- rep(FALSE, length(rnk))
    # resolve ranking duplicates by moving to the next column
    if(any(dpl)) {
        if(!is.null(seed)) set.seed(seed)
        for(pref in 1:nc) {
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
    return(list(rnk, sampled))
}

backwards.tiebreak <- function(prefs, ic, elim = TRUE) {
    if(!elim) prefs <- -prefs
    if(is.null(dim(prefs))) dim(prefs) <- c(1, length(prefs))
    sampled <- rep(FALSE, ncol(prefs))
    rnk <- t(apply(prefs, 1, rank, ties.method="min"))
    if(is.null(dim(rnk))) dim(rnk) <- c(1, length(rnk))
    i <- nrow(rnk)
    ic.rnk <- rnk[i, ic]
    ic.rnk.sort <- sort(ic.rnk)
    icv <- rep(FALSE, ncol(prefs))
    icv[ic] <- TRUE
    while(i > 1 && length(ic.rnk) > 1 && ic.rnk.sort[1] == ic.rnk.sort[2]){
        ic <- which(icv & (rnk[i, ] == ic.rnk.sort[1]))
        i <- i - 1
        ic.rnk <- rnk[i, ic]
        ic.rnk.sort <- sort(ic.rnk)
    }
    if(i == 1 && length(ic.rnk) > 1 && ic.rnk.sort[1] == ic.rnk.sort[2]) { # need sampling
        selected <- sample(1:length(ic), 1)
        sampled[ic[selected]] <- TRUE
    } else selected <- which.min(ic.rnk)
    return(list(selected, sampled))
}

summary.vote.stv <- function(object, ..., digits = 3) {
    decimalplaces <- function(x) {
        ifelse(abs(x - round(x)) > .Machine$double.eps^0.5,
               nchar(sub('^\\d+\\.', '', sub('0+$', '', as.character(x)))),
               0)
    }
  ncounts <- nrow(object$preferences)
  df <- data.frame(matrix(NA, nrow=ncol(object$preferences)+3, ncol=2*ncounts-1),
                   stringsAsFactors = FALSE)
  rownames(df) <- c("Quota", colnames(object$preferences), "Elected", "Eliminated")
  colnames(df) <- c(1, paste0(rep(2:ncounts, each=2), c("-trans", "")))
  idxcols <- c(1, seq(3,ncol(df), by=2))
  df["Quota", idxcols] <- object$quotas
  df[2:(nrow(df)-2), idxcols] <- t(object$preferences)
  # calculate transfers
  pref <- object$preferences
  # remove quotas for winners and compute difference
  where.winner <- which(rowSums(object$elect.elim==1)==1)
  pref[where.winner,] <- pref[where.winner,] - object$elect.elim[where.winner,]*object$quotas[where.winner]
  tmp <- t(object$preferences[2:nrow(object$preferences),] - pref[1:(nrow(pref)-1),])
  if(nrow(tmp) == 1) tmp <- as.numeric(tmp) # because of R weirdness with vectors and matrices (when there is just one round)
  df[2:(nrow(df)-2), seq(2,ncol(df), by=2)] <- tmp
  # format the right number of digits
  df[1:(nrow(df)-2),] <- apply(df[1:(nrow(df)-2),], 2, 
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
  df[is.na(df)] <- ""
  class(df) <- c('summary.vote.stv', class(df))
  attr(df, "number.of.votes") <- nrow(object$data)
  attr(df, "number.of.invalid.votes") <- nrow(object$invalid.votes)
  attr(df, "number.of.candidates") <- ncol(object$preferences)
  attr(df, "number.of.seats") <- length(object$elected)
  attr(df, "equal.pref.allowed") <- object$equal.pref.allowed
  return(df)
}

print.summary.vote.stv <- function(x, ...) {
  cat("\nResults of Single transferable vote")
  if(attr(x, "equal.pref.allowed")) cat(" with equal preferences")
  cat("\n===================================")
  if(attr(x, "equal.pref.allowed")) cat("=======================")
  election.info(x)
  print(kable(x, align='r', ...))
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
        nij <- sapply(1:nc, function(pref) apply(xd, 2, function(f) sum(f == pref)))[nc:1,]
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

