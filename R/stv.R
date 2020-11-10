stv <- function(votes, mcan = NULL, eps=0.001, fsep='\t', verbose = FALSE, seed = 1234, quiet = FALSE, ...) {
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
	# The argument mcan is number of candidates to be elected
	#
	# If mcan is not supplied it will be assumed that the number of candidates
	# to be elected is half the number of candidates standing.
	#
	# If verbose=T, the progress of the count will be printed
	#
	# The program was written by Bernard Silverman for the IMS in August 2002
	##################################
	
  if(verbose) {
    cat("\nSingle transferable vote count")
    cat("\n==============================\n")
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
	x <- check.votes(votes, "stv", quiet = quiet)
	nvotes <- nrow(x)
	w <- rep(1, nvotes)
	
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
	tie.elim.rank <- rnk
	
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
		vcast <- apply(w * (x == 1), 2, sum)
		names(vcast) <- cnames
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
		#
		# if leading candidate exceeds quota, declare elected and adjust weights
		# mark candidate for elimination in subsequent counting
		#
		vmax <- max(vcast)
		if(vmax >= quota) {
			ic <- (1:nc)[vcast == vmax]
			tie <- FALSE
			if(length(ic) > 1) {# tie
			    iic <- which.max(tie.elim.rank[ic])
			    ic <- ic[iic]
			    tie <- TRUE
			}
			index <- (x[, ic] == 1)
			w[index] <- (w[index] * (vmax - quota))/vmax
			mcan <- mcan - 1
			elected <- c(elected, cnames[ic])
			result.elect[count,ic] <- 1
			if(verbose && !quiet) {
			    cat("Candidate", cnames[ic], "elected ")
			    if(tie) {
			        cat("using forwards tie-breaking method ")
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
			    iic <- which.min(tie.elim.rank[ic])
			    ic <- ic[iic]
			    tie <- TRUE
			}
			result.elect[count,ic] <- -1
			if(verbose && !quiet) {
			    cat("Candidate", cnames[ic], "eliminated ")
			    if(zero.eliminated) cat("due to zero first preferences ")
			    if(tie) {
			        cat("using forwards tie-breaking method ")
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
	               elect.elim=result.elect, data=orig.x, 
	               invalid.votes=votes[setdiff(rownames(votes), rownames(x)),,drop = FALSE]), 
	               class="vote.stv")
	if(!quiet) print(summary(result))
	invisible(result)
}

summary.vote.stv <- function(object, ...) {
  ncounts <- nrow(object$preferences)
  df <- data.frame(matrix(NA, nrow=ncol(object$preferences)+3, ncol=2*ncounts-1),
                   stringsAsFactors = FALSE)
  rownames(df) <- c("Quota", colnames(object$preferences), "Elected", "Eliminated")
  colnames(df) <- c(1, paste0(rep(2:ncounts, each=2), c("-trans", "")))
  idxcols <- c(1, seq(3,ncol(df), by=2))
  df["Quota", idxcols] <- round(object$quotas,3)
  df[2:(nrow(df)-2), idxcols] <- round(t(object$preferences),3)
  # calculate transfers
  pref <- object$preferences
  # remove quotas for winners and compute difference
  where.winner <- which(rowSums(object$elect.elim==1)==1)
  pref[where.winner,] <- pref[where.winner,] - object$elect.elim[where.winner,]*object$quotas[where.winner]
  tmp <- t(round(object$preferences[2:nrow(object$preferences),] - pref[1:(nrow(pref)-1),], 3))
  if(nrow(tmp) == 1) tmp <- as.numeric(tmp) # because of R weirdness with vectors and matrices (when there is just one round)
  df[2:(nrow(df)-2), seq(2,ncol(df), by=2)] <- tmp
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
  return(df)
}

print.summary.vote.stv <- function(x, ...) {
  cat("\nResults of Single transferable vote")
  cat("\n===================================")
  election.info(x)
  print(kable(x, align='r', ...))
  #cat("\nElected:", paste(x$Elected[x$Elected != ""], collapse=", "), "\n\n")
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

image.vote.stv <- function(object, xpref = 1, ypref = 2, all.pref = FALSE, ...) {
    nc <- ncol(object$preferences)
    x <- object$data
    if(all.pref) {
        nij <- sapply(1:nc, function(pref) apply(x, 2, function(f) sum(f == pref)))
        image(x = 1:nc, y = 1:nc, nij[,nc:1], axes = FALSE, xlab = "", ylab = "ranking")
        axis(2, at = 1:nc, labels = nc:1)
        axis(3, at = 1:nc, labels = FALSE, tick = FALSE)
        text(1:nc, y = par("usr")[4], labels = colnames(object$preferences), xpd = NA, srt = 45, adj = 0)
    } else {
        xdt <- data.table(x)
        xdt[, voter := 1:nrow(x)]
        xdtl <- melt(xdt, id.vars = "voter", variable.name = "candidate", value.name = "rank")
        xdtl <- xdtl[rank %in% c(xpref, ypref)]
        xdtw <- dcast(xdtl, voter ~ rank, value.var = "candidate")
        colnames(xdtw)[2:3] <- c("xpref", "ypref")
        ctbl <- table(xdtw[, ypref], xdtw[, xpref])
        image(x = 1:nc, y = 1:nc, t(ctbl[nc:1,]), axes = FALSE, xlab = paste("rank", xpref), ylab = paste("rank", ypref))
        axis(2, at = nc:1, labels = rownames(ctbl), tick = FALSE, las = 1)
        axis(3, at = 1:nc, tick = FALSE, labels = FALSE)
        text(1:nc, y = par("usr")[4], labels = colnames(ctbl), xpd = NA, srt = 45, adj = 0)
    }
}

plot.vote.stv <- function(object, xlab = "Count", ylab = "Preferences", point.size = 2, ...) {
    stopifnot(require("ggplot2"))
    # Plot evolution of the preferences
    # prepare data in the long format
    df <- data.table(object$preferences)
    df[, Count := 1:nrow(df)]
    dfl <- melt(df, id.vars = "Count", variable.name = "Candidate")
    # remove zeros from elected and eliminated candidates
    dfl <- dfl[!(Candidate %in% names(colSums(abs(object$elect.elim)) > 0) & value == 0)] 
    
    # dataset for plotting the quota
    dfq <- data.table(Count = 1:length(object$quotas), value = object$quotas, Candidate = "Quota")
    
    # dataset for plotting points of elected and eliminated candidates
    dfe <- melt(data.table(Count = 1:nrow(object$elect.elim), object$elect.elim), id.vars = "Count", variable.name = "Candidate")
    dfe <- dfe[value != 0]
    dfe[, selection := ifelse(value > 0, "elected", "eliminated")]
    dfe <- dfe[dfl, value := i.value, on = c("Count", "Candidate")]
    
    # create plots
    g <- ggplot(dfl, aes(x = as.factor(Count), y = value, color = Candidate, group = Candidate)) + geom_line()
    g <- g + geom_line(data = dfq, aes(x = Count), color = "black") + xlab(xlab) + ylab(ylab)
    g <- g + geom_point(data = dfe, aes(shape = selection), size = point.size) + ylim(range(0, max(dfl$value, dfq$value)))
    g
}


stvi <- function(votes, mcan = NULL, allow.indf = FALSE, eps=0.001, fsep='\t', verbose = FALSE, seed = 1234, ...) {
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
    # The argument mcan is number of candidates to be elected
    #
    # If mcan is not supplied it will be assumed that the number of candidates
    # to be elected is half the number of candidates standing.
    #
    # If verbose=T, the progress of the count will be printed
    #
    # The program was written by Bernard Silverman for the IMS in August 2002
    ##################################
    
    if(verbose) {
        cat("\nSingle transferable vote count")
        cat("\n==============================\n")
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
    
    if(verbose) cat("Number of votes cast is", nrow(votes), "\n")
    x <- if(allow.indf) check.votes(votes, "condorcet") else check.votes(votes, "stv")
    nvotes <- nrow(x)
    w <- rep(1, nvotes)
    
    freqfcn <- function(r) {
        y <- table(r)
        y[as.character(r)]
    }
    freq <- t(apply(x, 1, freqfcn))
    colnames(freq) <- cnames
    
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
    tie.elim.rank <- rnk
    
    # initialize results
    result.pref <- result.elect <- matrix(NA, ncol=nc, nrow=0, 
                                        dimnames=list(NULL, cnames))
    wA <- wAtmp <- prev.wA <- matrix(0, ncol=nc, nrow=nvotes, 
                         dimnames=list(NULL, cnames))
    Dmat <- matrix(FALSE, ncol=nc, nrow=nvotes, 
                   dimnames=list(NULL, cnames))
    result.quota <- c()
    orig.x <- x
    #
    # the main loop
    #
    if(verbose) cat("\nList of 1st preferences in STV counts: \n")
    
    count <- 0
    first.vcast <- NULL
    cumw <- rep(0, nvotes)
    
    prev.wA[] <- 1
    wAadd <- wA
    wAadd[] <- 0
    winner.index <- loser.index <- NULL
    A <- (x == 1)/freq
    wAadd[A < 1] <- A[A < 1]
    wAtmp[] <- 1
    while(mcan > 0) {
        #
        # calculate quota and total first preference votes
        #
        count <- count + 1
        wA[] <- 1
        #if(count > 1) stop("")
        #wA <- if(count == 2) A*Dmat else wA + wA*Dmat # keep previous weighted A matrix
        #wd <- w*A
        #overfl <- overfl + w*
        #Dmat <- (rowSums(A) < 1) * A > 0
        A <- (x == 1)/rowSums(x == 1)
        #if(count == 4) stop('')
        #stop("")
        #vcast <- apply(w*A + wA, 2, sum)
        if(!is.null(winner.index)) {
            if(count > 1 && any(Dmat)) {
                A[apply(Dmat, 1, any), ic] <- 1
                A <- A/rowSums(A)
            }
            #wi <- which(winner.index)
            distr <- (vmax - quota)/vmax
            #for(j in 1:length(wi)) {
            #    recp <- A[wi[j],] > 0
            #    wA[wi[j], recp] <- wA[wi[j], recp] * distr
            #}
            #wA[winner.index, ] <- wA[winner.index, ] * distr * (A[winner.index,] > 0) + wA[winner.index, ] * (A[winner.index,] == 0)
            wAadd[winner.index, ] <- (wAadd[winner.index, ] + distr * A[winner.index,]) * wAtmp[winner.index,]
            #wd <- wA * A
            #stop('')
        } else {
            if(length(loser.index) > 0) {
                part1 <- loser.index & apply(Dmat, 1, any) # first position did not move
                part2 <- loser.index & !apply(Dmat, 1, any) # second position became first position
                wAadd[part1, ] <- wAadd[part1, ] + wAadd[part1, ]*Dmat[part1,]/rowSums(Dmat[part1,,drop = FALSE])
                wAadd[part2, ] <- wAadd[part2, ] + A[part2, ]
            }
        }

        wA[wAadd > 0] <- wAadd[wAadd > 0]
        
        #wA <- wA + prev.wA*Dmat
        prev.wA <- wA
        vcast <- apply(wA * wAtmp * (A > 0), 2, sum)
        #vcast <- apply(wA * A, 2, sum)
        #vcast <- apply(cumw * (x == 1), 2, sum)
        names(vcast) <- cnames
        quota <- sum(vcast)/(mcan + 1) + eps
        result.quota <- c(result.quota, quota)
        result.pref <- rbind(result.pref, vcast)
        result.elect <- rbind(result.elect, rep(0,nc))
        #this.cumw <- rep(0, nvotes)
        if(verbose) {
            cat("\nCount:" , count, "\n")
            #df <- data.frame(QUOTA=round(quota, 3), t(round(vcast[vcast != 0], 3)))
            df <- data.frame(QUOTA=round(quota, 3), mcan = mcan, t(round(vcast, 3)))
            rownames(df) <- count
            print(df)
            cat("\nData:\n" )
            print(x)
            #cat("\nWeights:\n" )
            #print(w)
            cat("\nwA:\n" )
            print(wA * wAtmp)
            cat("\nWeighted data:\n" )
            print(wA * wAtmp* (A > 0))
            cat("\nA:\n" )
            print(A)
            cat("\nDmat:\n" )
            print(1*(Dmat))
            #cat("\nprev.wd * Dmat:\n" )
            #print(prev.wd * Dmat)
            #cat("\nprev.wd:\n" )
            #print(prev.wd)
        }
        #
        # if leading candidate exceeds quota, declare elected and adjust weights
        # mark candidate for elimination in subsequent counting
        #
        winner.index <- NULL
        #prev.wd <- wA * A + prev.wd * Dmat
        vmax <- max(vcast)
        if(vmax >= quota) {
            ic <- (1:nc)[vcast == vmax]
            tie <- FALSE
            if(length(ic) > 1) {# tie
                iic <- which.max(tie.elim.rank[ic])
                ic <- ic[iic]
                tie <- TRUE
            }
            index <- (x[, ic] == 1)
            #this.cumw[index] <- this.cumw[index] + (vmax - quota)/vmax
            w[index] <- (w[index] * (vmax - quota))/vmax
            #if(count > 0)stop('')
            winner.index <- index
            #w[index] <- w[index] + (vmax - quota)/vmax
            mcan <- mcan - 1
            elected <- c(elected, cnames[ic])
            result.elect[count,ic] <- 1
            if(verbose) {
                cat("Candidate", cnames[ic], "elected ")
                if(tie) {
                    cat("using forwards tie-breaking method ")
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
                iic <- which.min(tie.elim.rank[ic])
                ic <- ic[iic]
                tie <- TRUE
            }
            result.elect[count,ic] <- -1
            if(verbose) {
                cat("Candidate", cnames[ic], "eliminated ")
                if(zero.eliminated) cat("due to zero first preferences ")
                if(tie) {
                    cat("using forwards tie-breaking method ")
                    if(sampled[ic]) cat("(sampled)")
                }
                cat("\n")
            }
            if(zero.eliminated)	first.vcast[ic] <- 1
            loser.index <- (x[, ic] == 1)
        }
        # redistribute votes
        #first.votes <- x[,ic] == 1
        Dmat[] <- FALSE
        for(i in (1:nvotes)) {
            jp <- x[i, ic]
            if(jp > 0) {
                index <- (x[i, ] > jp)
                index.distr <- x[i, ] == jp    # for indifferent votes
                x[i, index] <- x[i, index] - 1
                x[i, ic] <- 0
                Dmat[i, index.distr & x[i, ] == jp] <- TRUE # record if ordering did not change after redistribution
                #prev.wd[i, ic] <- 0
                wAtmp[i, ic] <- 0
            } 
        }
    }
    rownames(result.pref) <- 1:count
    #cat("\nElected candidates are, in order of election: \n", paste(elected, collapse = ", "), "\n")
    result <- structure(list(elected = elected, preferences=result.pref, quotas=result.quota,
                             elect.elim=result.elect, data=orig.x, 
                             invalid.votes=votes[setdiff(rownames(votes), rownames(x)),,drop = FALSE]), 
                        class="vote.stv")
    print(summary(result))
    invisible(result)
}

