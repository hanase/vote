ims_score <- local({
	ims <- read.table("ims_election.txt", header=TRUE, sep="\t")
	change.records <- !is.na(ims) & ims > 0
	ims[change.records] <- 10-ims[change.records]
	ims
})
