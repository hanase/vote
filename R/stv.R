stv <- function(votes, mcan = NULL, eps=0.001, fsep='\t', verbose = FALSE, ...) {
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
	x <- check.votes(votes, "stv")
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
	# resolve ranking duplicates by moving to the next column
    if(any(dpl)) {
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
	
	#
	# the main loop
	#
	if(verbose) cat("\nList of 1st preferences in STV counts: \n")
	
	count <- 0
	first.vcast <- apply(w * (x == 1), 2, sum)
	names(first.vcast) <- cnames
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
		if(verbose) {
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
			if(verbose) {
			    cat("Candidate", cnames[ic], "elected ")
			    if(tie) cat("using forwards tie-breaking method")
			    cat("\n")
			}
		} else {
			# if no candidate reaches quota, mark lowest candidate for elimination
		    zero.eliminated <- FALSE
		    # if there are candidates with zero first votes, eliminate those first
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
			    if(tie) cat("using forwards tie-breaking method")
			    cat("\n")
			}
			if(zero.eliminated)	first.vcast[ic] <- 1
		}
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
	#cat("\nElected candidates are, in order of election: \n", paste(elected, collapse = ", "), "\n")
	result <- structure(list(elected = elected, preferences=result.pref, quotas=result.quota,
	               elect.elim=result.elect, data=x, 
	               invalid.votes=votes[setdiff(rownames(votes), rownames(x)),]), 
	               class="vote.stv")
	print(summary(result))
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
  df[2:(nrow(df)-2), seq(2,ncol(df), by=2)] <- t(round(
      object$preferences[2:nrow(object$preferences),] - pref[1:(nrow(pref)-1),], 3))
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
