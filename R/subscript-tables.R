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

#' @export
`[[.QTable` <- function(x, ..., exact = TRUE) {
    # Use sys.call as match.call captures the quoted arguments as names
    system.call <- sys.call()
    if (!validArgumentNames(names(system.call), "exact"))
        throwErrorOnlyNamed("exact", "[[")
    called.args <- match.call(expand.dots = FALSE)
    empty.ind <- providedArgumentEmpty(called.args, optional.arg = "exact")
    x.dim <- dim(x)
    if (empty.ind)
        throwErrorEmptyDoubleIndex(substitute(x), x.dim)
    n.dim <- length(x.dim)
    n.index.args <- nargs() - 1L - !missing(exact)
    correct.n.args <- n.index.args == n.dim
    all.args.length.one <- allArgsLengthOne(system.call)
    if (!(correct.n.args && all.args.length.one))
        throwErrorTableDoubleIndex(substitute(x), n.dim)
    y <- NextMethod(x)
    class(y) <- c("QTable", class(y))
    # Update Attributes here
    y
}

allArgsLengthOne <- function(system.call) {
    relevant.args <- names(system.call[-(1:2)]) == ""
    all(vapply(system.call[relevant.args], length, integer(1L) == 1L))
}

validArgumentNames <- function(x, optional.arg = NULL) {
    all(x %in% c("", optional.arg))
}

providedArgumentEmpty <- function(called.args, optional.arg) {
    named.args <- names(called.args)
    named.args[3L] == optional.arg || isEmptyList(called.args[3L])
}

isEmptyList <- function(x) x == quote(as.pairlist(alist())())

describeTableMsg <- function(x.name, x.dim) {
    type <- if (n.dim == 2) "a matrix" else "an array"
    dim.string <- if (n.dim == 1) "dimension" else "dimensions"
    paste0("The Table (", x.name, ") is ", type, " in R with ", n.dim, " ", dim.string)
}

determineValidDoubleInd <- function(x.dim) {
    vapply(x.dim, function(x) max(x %/% 2, 1), numeric(1L))
}

throwErrorTableIndexInvalid <- function(x, n.dim, n.index.args) {
    table.description <- describeTableMsg(x, n.dim)
    stop(table.description,
         "However, the call to [ specified ",
         if (n.index.args < n.dim) "only ", n.index.args, " index arguments.")
}

suggestedDoubleIndex <- function(x.name, x.dim) {
    valid.inds <- determineValidDoubleInd(x.dim)
    single.syntax <- paste0("For example, ", x.name, " can be subscriptted with, ",
                            x.name, "[[", valid.inds[1], "]]")
    all.syntax <- if (length(x.dim) > 1) {
        paste0(" or ", x.name, "[[", paste0(valid.inds, collapse = ", "), "]].")
    }
    paste0("When using the [[ subscript, there either needs to be a single integer ",
           "(or string) reference, or one for each dimension. ", single.syntax, all.syntax)
}

generalDoubleIndexMsg <- function(x.name, x.dim) {
    table.description <- describeTableMsg(x.name, x.dim)
    suggested.syntax <- suggestedDoubleIndex(x.name, x.dim)
    paste0(table.description, ". ", suggested.syntax)
}

throwErrorTableDoubleIndex <- function(x.name, x.dim) {
    error.message <- generalDoubleIndexMsg(x.name, x.dim)
    stop(error.message)
}

throwErrorEmptyDoubleIndex <- function(x.name, x.dim) {
    general.error.message <- generalDoubleIndexMsg(x.name, x.dim)
    empty.bad.message <- paste0(x.name, "[[]] cannot be used. ")
    stop(empty.bad.message, general.error.message)
}

throwErrorOnlyNamed <- function(named.arg, function.name) {
    stop("Only the ", sQuote(named.arg), " argument can be a named argument to ",
         sQuote(function.name))
}
