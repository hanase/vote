stv <- function(x, mcan = NULL, oldcount = FALSE, verbose = TRUE) {
	###################################
	# Single transferable vote.
	# Adopted from Bernard Silverman's code.
	# The data matrix x contains the votes themselves.
	# Row i of the matrix contains the preferences of voter i
	# numbered 1, 2, .., r, 0,0,0,0, in some order
	# The columns of the matrix correspond to the candidates.
	# The dimnames of the columns are the names of the candidates; if these
	# are not supplied then the candidates are lettered A, B, C, ...
	#
	# If x is a character string it is interpreted as a file name from which the
	# votes are to be read. A tab delimited file produced by excel
	# will be in the right format, with the candidate names in the first row.
	#
	# The argument mcan is number of candidates to be elected
	#
	# If mcan is not supplied it will be assumed that the number of candidates
	# to be elected is half the number of candidates standing.
	#
	# If oldcount=T, the results under the old system of counting will
	# be calculated, ie give one vote to each of the first mcan
	# preferences for each voter and just add up
	#
	# If verbose=T, the progress of the count will be printed
	#
	# The program was written by Bernard Silverman for the IMS in August 2002
	##################################
	
	
	# Prepare by finding names of candidates and setting up
	# vector w of vote weights and list of elected candidates
	
	if(is.character(x)) {
		x <- read.table(file = x, header = TRUE, row.names = NULL)
		x <- as.matrix(x)
	}
	nc <- ncol(x)
	cnames <- colnames(x)
	
	if(length(cnames) != nc) {
		cat("Warning: Candidate names not supplied, dummy names used instead.\n")
		cnames <- LETTERS[1:nc]
	}
	
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
	
	cat("Number of votes cast is", nrow(x), "\nChecking if these are valid ... \n")
	ok <- rep(TRUE, nrow(x))
	
	check.votes <- function(record) {
		z <- sort(diff(c(0, diff(sort(c(0, record))), 1)))
		return(z[nc] == 0 && z[nc + 1] == 1)
	}
	ok <- apply(check.votes, 2, check.votes)
	if(any(!ok)) {
		cat("Problematic votes which will be excluded:\n")
		cat(x[!ok,])
	}
	
	x <- x[ok, ]
	nvotes <- nrow(x)
	w <- rep(1, nvotes)
	cat("Number of valid votes is ", nvotes, "\n")
	
	#
	# Calculate results under old counting system
	#
	
	if(oldcount) {
		vtot <- apply(x <= mcan & x != 0, 2, sum)
		names(vtot) <- cnames
		cat("\nUnder old counting system totals would be\n")
		print(rev(sort(vtot)))
	}
	
	#
	# the main loop
	#
	
	cat("\nCounting the votes by STV ... \n")
	
	while(mcan > 0) {
		#
		# calculate quota and total first preference votes
		#
		vcast <- apply(w * (x == 1), 2, sum)
		names(vcast) <- cnames
		quota <- sum(vcast)/(mcan + 1)
		if(verbose) {
			cat("\nFirst preferences are now \n")
			print(round(vcast[vcast != 0], 1))
			cat("Quota is ", round(quota, 2), "\n") 
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
			if(verbose) cat("Candidate", cnames[ic], "elected \n")
		} else {
			#
			# if no candidate reaches quota, mark lowest candidate for elimination
			vmin <- min(vcast[vcast > 0])
			ic <- min((1:nc)[vcast == vmin])
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
	
	cat("\nElected candidates are, in order of election: \n", paste(elected, collapse = ", "), "\n")
	invisible() 
}
