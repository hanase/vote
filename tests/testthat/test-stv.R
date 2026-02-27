test_that('Wikipedia example works', {
    # version from Sep 2019
    # https://en.wikipedia.org/w/index.php?title=Single_transferable_vote&oldid=917689122
    data(food_election)
    stv.food <- stv(food_election, nseats = 3, eps = 1, quiet = TRUE)
    # check winners
    expect_equal(stv.food$elected, c("Chocolate", "Oranges", "Strawberries"))
    # Strawberries was elected on 4th count
    s <- summary(stv.food)
    expect_true("4" %in% names(s))
    expect_true("Strawberries" %in% s[["4"]])
    view(stv.food)
    }
)

test_that('IMS Council example works', {
    # Example of the IMS Council voting
    data(ims_election)
    stv.ims <- stv(ims_election, nseats = 5, quiet = TRUE)
    filename <- tempfile()
    png(filename=filename)
    view(stv.ims)
    plot(stv.ims)
    image(stv.ims)
    dev.off()
    expect_true(file.info(filename)['size'] > 0)
    unlink(filename)
    # there should be 29 invalid votes
    expect_equal(nrow(stv.ims$invalid.votes), 29)
    expect_equal(nrow(stv.ims$data), nrow(ims_election) - 29)
    expect_equal(stv.ims$elected[1], "Jasper")
    
})

test_that('Dublin West 2002 example works', {
    skip_on_cran() # takes longer to run
    # Example of Dublin West 2002 elections
    # https://en.wikipedia.org/wiki/Dublin_West#2002_general_election
    data(dublin_west)
    stv.dublin <- stv(dublin_west, nseats = 3, eps = 1, quiet = TRUE)
    expect_equal(stv.dublin$elected, c("Lenihan", "Higgins", "Burton"))
    expect_in(c("Ryan", "Morrissey", "Smyth"), 
              colnames(stv.dublin$elect.elim)[colSums(stv.dublin$elect.elim < 0) == 1])
})

test_that('Example of Small Committee works', {
    # Example of a small committee dataset
    # with four candidates (C) and four
    # voting committee members (uses tie-breaking)
    votes <- data.frame(C1=c(3,2,1,3), C2=c(2,1,2,4),
                        C3=c(4,3,3,1), C4=c(1,4,4,2))
    stv.small <- stv(votes, nseats = 2, quiet = TRUE)
    expect_equal(stv.small$elected, c("C4", "C2"))
    expect_in(stv.small$ties[c(1, 3)],  "fo") # forward tiebreak on two counts
})

test_that('Equal ranking and correction works', {
    votes <- data.frame(C1=c(3,2,1,3), C2=c(1,1,2,0),
                        C3=c(4,3,3,1), C4=c(1,4,2,2))
    expect_warning((stv.corr <- stv(votes, nseats = 2, equal.ranking = TRUE)))
    expect_equal(stv.corr$elected, c("C2", "C1"))
    # vote #3 was corrected by stv (C3 changed to rank 4)
    cor.votes <- correct.ranking(votes, quiet = TRUE)
    expect_equal(cor.votes[3, "C3"], 4)
})

test_that('Votes are weighted correctly', {
    # Example of weighted votes
    votes <- data.frame(C1=c(3,2,1,3), C2=c(2,1,2,4),
                        C3=c(4,3,3,1), C4=c(1,4,4,2),
                        weight = c(1, 2, 1, 2.5)
                        )
    stv.weight <- stv(votes, nseats = 2, weight.column = "weight", quiet = TRUE)
    # check that the first round is multiplied by the weights
    expect_equal(stv.weight$preferences[1,], colSums(votes[, "weight"]*(votes[, 1:4] == 1)))
})


test_that('Votes with equat ranking are weighted correctly', {
    # Example of weighted votes
    votes <- data.frame(C1=c(3,2,1,3), C2=c(1,1,2,0),
                        C3=c(4,3,3,1), C4=c(1,4,2,2),
                        weight = c(1, 2, 1, 4)
                        )
    expect_warning((stv.eq.weight <- stv(votes, nseats = 2, 
                                      weight.column = "weight", 
                                      equal.ranking = TRUE)))
    cor.votes <- correct.ranking(votes[, 1:4], quiet = TRUE)
    expect_equal(as.vector(stv.eq.weight$preferences[1,]), 
                 c(1, # from voter 3
                   2.5, # 0.5 from voter 1 (shared between C2 and C4) + 2 from voter 2
                   4, # from voter 4
                   0.5 # from voter 1 (shared between C2 and C4)
                   ))
    
})

test_that('Preferences are imputed correctly', {
    # Example of imputing preferences
    # (third voter has a conflict of interest with candidate C2)
    votes <- data.frame(C1=c(3,2,1,3), C2=c(2,1,-1,0),
                        C3=c(4,3,3,1), C4=c(1,4,2,2))
    res <- stv(votes, nseats = 2, impute.missing = TRUE, quiet = TRUE)
    cor.votes <- corrected.votes(res)
    expect_equal(as.vector(cor.votes$imputed), c(NA, 2, NA, NA))
    expect_equal(as.vector(cor.votes$new), c(1, 2, 4, 3))
    # imputed rank 2, as it is the median(c(2, 1, 4))
    # where the last 4 was derived as the median of missing ranks 
    # in vote four. The imputation can be also performed via impute.ranking()
    expect_true(all(impute.ranking(votes) == res$data))
    
    # check that weighting of imputed data works
    wvotes <- cbind(votes, weight = c(1, 2, 1, 2.5))
    wres <- stv(wvotes, nseats = 2, impute.missing = TRUE, 
                weight.column = "weight", quiet = TRUE)
    cvotes <- correct.ranking(votes, quiet = TRUE)
    expect_equal(wres$preferences[1,], 
                 colSums(wvotes[, "weight"]*(cvotes == 1)))
})

test_that('Reserving seats works', {
    # Example of using reserved seats: 
    # e.g. reserve two seats for students
    data(ims_election)
    students <- c("Declan", "Claire", "Oscar")
    stv.ims <- stv(ims_election, nseats = 5, group.nseats = 2, 
            group.members = students, quiet = TRUE) # students
    expect_true(sum(stv.ims$elected %in% students) >= 2)
    
    # check weighting with reserved seats
    votes <- cbind(ims_election, weight = runif(nrow(ims_election), 0.5, 10))
    stv.wims <- stv(votes, nseats = 5, group.nseats = 2, quiet = TRUE,
                    group.members = students, weight.column = "weight")
    expect_equal(stv.wims$preferences[1,], 
                 colSums(votes[-as.integer(rownames(stv.wims$invalid.votes)), "weight"]*(stv.wims$data == 1)))
    expect_equal(as.vector(attr(stv.wims$data, "weights")), 
                 votes[-as.integer(rownames(stv.wims$invalid.votes)), "weight"])
    expect_equal(as.vector(attr(stv.wims$invalid.votes, "weights")), 
                 votes[as.integer(rownames(stv.wims$invalid.votes)), "weight"])
    expect_true(sum(stv.wims$elected %in% students) >= 2)
})

test_that('Removing candidates and partially invalidating ballots works', {
    # Example of removing candidates from original votes
    data(ims_election)
    updated.votes <- remove.candidate(ims_election, c("Jasper", "Tilmann"))
    res <- stv(updated.votes, nseats = 5, quiet = TRUE)
    expect_false(any(c("Jasper", "Tilmann") %in% res$elected))
    
    # the same with weighting
    wvotes <- cbind(updated.votes, weight = runif(nrow(updated.votes), 0.5, 10))
    wres <- stv(wvotes, nseats = 5, weight.column = "weight", quiet = TRUE)
    expect_false(any(c("Jasper", "Tilmann") %in% wres$elected))
    expect_equal(wres$preferences[1,], 
                 colSums(wvotes[-as.integer(rownames(wres$invalid.votes)), "weight"]*(wres$data == 1)))
    
    # Example of accepting partially invalid ballots
    expect_warning((res.part <- stv(ims_election, invalid.partial = TRUE)))

    # There are now 24 invalid votes instead of 29, 
    # because 5 were corrected (ranking before the first 
    # gap/tie is valid, after that it is 0)
    cor.votes <- corrected.votes(res.part)
    expect_equal(nrow(cor.votes$new), 5)
    expect_equal(nrow(res.part$invalid.votes), 24)
    expect_equal(nrow(invalid.votes(res.part)), 24)
    expect_equal(nrow(res.part$data), nrow(ims_election) - 24)
    
    # the same with weighting
    wvotes <- cbind(ims_election, weight = runif(nrow(updated.votes), 0.5, 10))
    expect_warning((wres.part <- stv(wvotes, invalid.partial = TRUE, weight.column = "weight")))
    expect_equal(wres.part$preferences[1,], 
                 colSums(wvotes[-as.integer(rownames(wres.part$invalid.votes)), "weight"]*(wres.part$data == 1)))
    expect_equal(as.vector(attr(wres.part$data, "weights")), 
                 wvotes[-as.integer(rownames(wres.part$invalid.votes)), "weight"])
    expect_equal(as.vector(attr(wres.part$invalid.votes, "weights")), 
                 wvotes[as.integer(rownames(wres.part$invalid.votes)), "weight"])
})