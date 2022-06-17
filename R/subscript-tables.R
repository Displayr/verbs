#' @export
`[.QTable` <- function(x, ..., drop) {
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
    if (missing.drop)
        drop <- if (empty.ind) TRUE else length(dim(x)) == 1
    # Catch empty input e.g. x[] or x[drop = TRUE/FALSE] (when ... is empty)
    if (empty.ind) {
        y <- .subset(x, drop = drop)
        class(y) <- class(x)
        return(y)
    } # From here the ... args should have input
    x.dim <- dim(x)
    n.dim <- length(x.dim)
    n.index.args <- nargs() - 1L - !missing.drop
    if (n.dim != n.index.args) # Throw a nicer error if the indexing is not appropriate
        throwErrorTableIndexInvalid(substitute(x), n.dim, n.index.args)
    y <- .subset(x, ..., drop = drop)
    class(y) <- class(x)
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

#' @export
`[.Table<-` <- function() {
    browser()

throwErrorDropOnlyNamed <- function() {
    stop("Only the ", sQuote("drop"), " argument can be a named argument to ",
         sQuote("["))
}
