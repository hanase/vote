count.votes <- function(votes, method=c("auto", "plurality", "approval", "stv", "score", 
                                        "condorcet", "tworound.runoff"), 
            fsep='\t', ...) {
  # Main function for counting votes. 
  # If method is "auto" it determines the right method depending on the 
  # number of valid records.
  x <- prepare.votes(votes, fsep=fsep)
  method <- match.arg(method)
  if(method == "auto") {
    # extract method names from the method argument
    all.methods <- eval(formals()[["method"]])[-1] 
    # count valid records for all methods
    valid <- rep(0, length(all.methods))
    names(valid) <- all.methods
    for (meth in all.methods) {
      assembly.fun <- paste0("assemble.args.for.check.", meth) 
      args <- if(exists(assembly.fun)) do.call(assembly.fun, list(x, ...)) else list()
      valid[meth] <- sum(do.call(is.valid.vote, c(list(x, method=meth), args)))
    }
    method <- names(valid)[which.max(valid)]
  }
  return(do.call(method, list(x, ...)))
}

invalid.votes <- function(object) {
  return(object$invalid.votes)
}

valid.votes <- function(object) {
  return(object$data)
}

corrected.votes <- function(object) {
  return(object$corrected.votes)
}
