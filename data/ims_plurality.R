ims_plurality <- local({
	ims <- read.table("ims_election.txt", header=TRUE, sep="\t")
	ims[ims > 1] <- 0
	ims
})
