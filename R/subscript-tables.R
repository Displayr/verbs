#' @export
`[.QTable` <- function(x, ..., drop) {
    # Use sys.call as match.call captures the quoted arguments as names
    if (!all(names(sys.call()) %in% c("", "drop")))
        throwErrorDropOnlyNamed()
    called.args <- match.call(expand.dots = FALSE)
    names.called.args <- names(called.args)
    if (names.called.args[3L] == "drop") {
        empty.ind <- TRUE
    } else if (names.called.args[3L] == "...") {
        empty.ind <- isEmptyList(called.args[3L])
    } else {
        empty.ind <- FALSE
    }
    # Catch empty input e.g. x[] or x[drop = TRUE/FALSE] (when ... is empty)
    if (empty.ind) {
        y <- NextMethod(x)
        class(y) <- c("QTable", class(y))
        return(y)
    }
    x.dim <- dim(x)
    n.dim <- length(x.dim)
    n.index.args <- nargs() - 1L - !missing(drop)
    # Throw a nicer error if the indexing is not appropriate
    if (n.index.args != 1 && n.dim != n.index.args)
        throwErrorTableIndexInvalid(substitute(x), n.dim, n.index.args)
    y <- NextMethod(x)
    class(y) <- c("QTable", class(y))
    # Update Attributes here
    y
}

isEmptyList <- function(x) {
    isTRUE(all.equal(x, quote(as.pairlist(alist())())))
}

throwErrorTableIndexInvalid <- function(x, n.dim, n.index.args) {
    stop("The Table ", sQuote(x), " has ", n.dim, " array dimensions in R. ",
         "However, the call to [ specified ",
         if (n.index.args < n.dim) "only ", n.index.args, " index arguments.")
}

throwErrorDropOnlyNamed <- function() {
    stop("Only the ", sQuote("drop"), " argument can be a named argument to ",
         sQuote("["))
}
