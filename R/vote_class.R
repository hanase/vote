# custom class with with weights as attribute
vote.matrix <- function(data) {
    mat <- as.matrix(data)
    attr(mat, "weights") <- rep(1, nrow(mat))
    if(!inherits(mat, "vote.matrix")) class(mat) <- c("vote.matrix", class(mat))
    mat
}

vote.matrix.mat <- function(mat, w) {
    stopifnot(is.matrix(mat))
    attr(mat, "weights") <- w
    names(attr(mat, "weights")) <- rownames(mat)
    if(!inherits(mat, "vote.matrix")) class(mat) <- c("vote.matrix", class(mat))
    mat
}

# subsetting method for `[` that preserves the weights
`[.vote.matrix` <- function(x, i, j, drop = TRUE) {
    # Perform regular subsetting
    res <- NextMethod("[")
    
    if (missing(i)) {
        i_idx <- seq_len(nrow(x))
    } else {
        i_idx <- i
    }
    
    # Subset the attribute only if 'i' is indexing rows
    if (!is.null(attr(x, "weights"))) {
        attr(res, "weights") <- attr(x, "weights")[i_idx]
    }
    
    # Preserve the class only if still a matrix
    if (is.matrix(res)) {
        class(res) <- class(x)
    } else {
        # If not a matrix anymore, drop the special class
        class(res) <- setdiff(class(res), "vote.matrix")
    }
    res
}

get.vote.weights <- function(votes){
    if(is.null((w <- attr(votes, "weights")))) return(rep(1, nrow(votes)))
    return(w)   
} 

are.votes.weighted <- function(votes){
    w <- get.vote.weights(votes)
    return(any(w != 1))
}