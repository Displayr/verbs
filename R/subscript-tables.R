#' @export
`[.QTable` <- function(x, ..., drop = TRUE) {
    # Use sys.call as match.call captures the quoted arguments as names
    if (!validArgumentNames(names(sys.call()), "drop"))
        throwErrorOnlyNamed("drop", "[")
    called.args <- match.call(expand.dots = FALSE)
    empty.ind <- providedArgumentEmpty(called.args, optional.arg = "drop")
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

validArgumentNames <- function(x, optional.arg = NULL) {
    all(x %in% c("", optional.arg))
}

providedArgumentEmpty <- function(called.args, optional.arg) {
    named.args <- names(called.args)
    named.args[3L] == optional.arg || isEmptyList(called.args[3L])
}

isEmptyList <- function(x) x == quote(as.pairlist(alist())())

throwErrorTableIndexInvalid <- function(x, n.dim, n.index.args) {
    stop("The Table ", sQuote(x), " has ", n.dim, " array dimensions in R. ",
         "However, the call to [ specified ",
         if (n.index.args < n.dim) "only ", n.index.args, " index arguments.")
}

throwErrorDropOnlyNamed <- function() {
    stop("Only the ", sQuote("drop"), " argument can be a named argument to ",
         sQuote("["))

throwErrorOnlyNamed <- function(named.arg, function.name) {
    stop("Only the ", sQuote(named.arg), " argument can be a named argument to ",
         sQuote(function.name))
}
