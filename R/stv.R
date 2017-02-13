stv <- function(votes, mcan = NULL, eps=0.001, fsep='\t', verbose = TRUE) {
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
	
	
	# Prepare by finding names of candidates and setting up
	# vector w of vote weights and list of elected candidates
	
  x <- prepare.votes(votes, fsep=fsep)
	nc <- ncol(x)
	cnames <- colnames(x)
	
	if(is.null(mcan)) {
		mcan <- floor(nc/2)
		cat("Number of candidates to be elected not specified.\nDefault value of ", mcan, "used instead.\n")
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
	
	cat("Number of votes cast is", nrow(x), "\n")
	x <- check.votes(x, "stv", nc=nc)
	nvotes <- nrow(x)
	w <- rep(1, nvotes)
	cat("Number of valid votes is", nvotes, "\n")
	#
	# Calculate results under old counting system
	#
	
	# if(oldcount) {
	# 	vtot <- apply(x <= mcan & x != 0, 2, sum)
	# 	names(vtot) <- cnames
	# 	cat("\nUnder old counting system totals would be\n")
	# 	print(rev(sort(vtot)))
	# }
	
	# initialize results
	result.pref <- result.elect <- result.elim <- matrix(NA, ncol=nc, nrow=0, 
	                                                     dimnames=list(NULL, cnames))
	result.quota <- c()
	
	#
	# the main loop
	#
	if(verbose) cat("\nList of 1st preferences in STV counts: \n")
	
	count <- 0
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
			ic <- max((1:nc)[vcast == vmax])
			index <- (x[, ic] == 1)
			w[index] <- (w[index] * (vmax - quota))/vmax
			mcan <- mcan - 1
			elected <- c(elected, cnames[ic])
			result.elect <- rbind(result.elect, rep(FALSE, nc))
			result.elect[nrow(result.elect),ic] <- TRUE
			rownames(result.elect)[nrow(result.elect)] <- count
			if(verbose) cat("Candidate", cnames[ic], "elected \n")
		} else {
			#
			# if no candidate reaches quota, mark lowest candidate for elimination
			vmin <- min(vcast[vcast > 0])
			ic <- min((1:nc)[vcast == vmin])
			result.elim <- rbind(result.elim, rep(FALSE, nc))
			result.elim[nrow(result.elim),ic] <- TRUE
			rownames(result.elim)[nrow(result.elim)] <- count
			if(verbose) cat("Candidate", cnames[ic], "eliminated \n")
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
	cat("\nElected candidates are, in order of election: \n", paste(elected, collapse = ", "), "\n")
	invisible(structure(list(elected = elected, preferences=result.pref, quotas=result.quota,
	               elect=result.elect, elim=result.elim), class="vote.stv"))
}

summary.vote.stv <- function(object) {
  ncounts <- nrow(object$preferences)
  df <- data.frame(matrix(NA, ncol=ncol(object$preferences)+5, nrow=2*ncounts-1),
                   stringsAsFactors = FALSE)
  colnames(df) <- c("Count", "Quota", "Type", colnames(object$preferences), "Elected", "Eliminated")
  idxrows <- c(1, seq(3,nrow(df), by=2))
  df[,"Count"] <- c(1, rep(2:ncounts, each=2))
  df[idxrows,"Quota"] <- round(object$quotas,3)
  df[c(1,seq(3,nrow(df), by=2)),4:(ncol(df)-2)] <- object$preferences
  # calculate transfers
  pref <- object$preferences
  # remove quotas for winners and compute difference
  pref[rownames(object$elect),] <- pref[rownames(object$elect),] - object$elect*object$quotas[as.integer(rownames(object$elect))]
  df[seq(2,nrow(df), by=2),4:(ncol(df)-2)] <- round(
      object$preferences[2:nrow(object$preferences),] - pref[1:(nrow(pref)-1),], 3)
  df[,"Type"] <- c("Total",rep(c("Transfer", "Total"), ncounts-1))
  for(i in 1:ncounts) {
    ichar <- as.character(i)
    if (ichar %in% rownames(object$elect)) {
      elected <- colnames(object$elect)[object$elect[ichar,]]
      df[idxrows[i],"Elected"] <- paste(elected, collapse=", ")
    }
    if (ichar %in% rownames(object$elim)) {
      eliminated <- colnames(object$elim)[object$elim[ichar,]]
      df[idxrows[i],"Eliminated"] <- paste(eliminated, collapse=", ")
    }
  }
  df[is.na(df)] <- ""
  class(df) <- c('summary.vote.stv', class(df))
  return(df)
}

print.summary.vote.stv <- function(x, ...) {
  print(knitr::kable(x, ...))
  cat("\nElected:", paste(x$Elected[x$Elected != ""], collapse=", "), "\n")
}

"view" <- function(object, ...) UseMethod("view")
view.vote.stv <- function(object, ...) {
 s <- summary(object)
 formatter <- list(area(col=4:(ncol(s)-2), row=c(1,seq(3,nrow(s), by=2))) ~ color_text("red", "red"))
 formattable(s, formatter, ...)
}

check.votes.stv <- function(record, nc) {
  z <- sort(diff(c(0, diff(sort(c(0, record))), 1)))
  return(z[nc] == 0 && z[nc + 1] == 1)
}