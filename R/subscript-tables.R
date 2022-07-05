#' @export
`[.qTable` <- function(x, ..., drop = TRUE) {
    # Use sys.call as match.call captures unmatched named arguments into ...
    used.arguments <- names(sys.call())
    input.name <- attr(x, "name")
    if (!validArgumentNames(used.arguments, "drop"))
        throwErrorOnlyNamed("drop", "[")
    if ("drop" %in% used.arguments && !is.logical(drop))
        stop("drop argument should be TRUE or FALSE")
    called.args <- match.call(expand.dots = FALSE)
    empty.ind <- providedArgumentEmpty(called.args, optional.arg = "drop")
    # Catch empty input e.g. x[] or x[drop = TRUE/FALSE] (when ... is empty)
    if (empty.ind) return(x)
    x.dim <- dim(x)
    n.dim <- length(x.dim)
    n.index.args <- nargs() - 1L - !missing(drop)
    # Throw a nicer error if the indexing is not appropriate
    if (n.index.args != 1 && n.dim != n.index.args)
        throwErrorTableIndexInvalid(input.name, x.dim)
    y <- NextMethod(`[`)
    called.args <- as.list(called.args[["..."]])

    ## Need to evaluate the arguments here to alleviate possible NSE issues; c.f.:
    ## http://adv-r.had.co.nz/Computing-on-the-language.html#calling-from-another-function
    evaluated.args <- called.args
    for (i in seq_along(called.args))
        if (!identical(as.character(called.args[[i]]), ""))
            evaluated.args[[i]] <- eval(called.args[[i]], parent.frame())

    # Update Attributes here
    y <- updateTableAttributes(y, x, called.args, evaluated.args)
    y
}

#' @export
`[[.qTable` <- function(x, ..., exact = TRUE) {
    # Use sys.call as match.call captures the quoted arguments as names
    used.arguments <- names(sys.call())
    input.name <- attr(x, "name")
    if (!validArgumentNames(used.arguments, "exact"))
        throwErrorOnlyNamed("exact", "[[")
    if ("exact" %in% used.arguments && !is.logical(exact))
        stop("exact argument should be TRUE or FALSE")
    called.args <- match.call(expand.dots = FALSE)
    empty.ind <- providedArgumentEmpty(called.args, optional.arg = "exact")
    x.dim <- dim(x)
    if (empty.ind)
        throwErrorEmptyDoubleIndex(input.name, x.dim)
    n.dim <- length(x.dim)
    n.index.args <- nargs() - 1L - !missing(exact)
    correct.n.args <- n.index.args == n.dim
    called.args <- as.list(called.args[["..."]])
    all.unit.length <- all(lengths(called.args) == 1L)
    if (!(correct.n.args && all.unit.length))
        throwErrorTableDoubleIndex(input.name, x.dim)
    y <- NextMethod(x)
    # Update Attributes here
    y <- updateTableAttributes(y, x, called.args)
    y
}

validArgumentNames <- function(arg.names, optional.arg = NULL) {
    all(arg.names %in% c("", optional.arg))
}

providedArgumentEmpty <- function(called.args, optional.arg) {
    named.args <- names(called.args)
    named.args[3L] == optional.arg || isEmptyList(called.args[3L])
}

isEmptyList <- function(x) x == quote(as.pairlist(alist())())

generalInvalidSubscriptMsg <- function(x.name) {
    paste0("The supplied subscripts on the Table (", x.name, ") are invalid.")
}

determineValidDoubleInd <- function(x.dim) {
    vapply(x.dim, function(x) max(x %/% 2, 1), numeric(1L))
}

determineValidSingleInd <- function(x.dim) {
    vapply(x.dim,
           function(x) if (x == 1 || x %/% 2 == 1) as.character(x) else paste0("1:", x %/% 2),
           character(1L))
}

throwErrorTableIndexInvalid <- function(x, x.dim) {
    general.msg <- generalInvalidSubscriptMsg(x)
    suggested <- suggestedSingleIndex(x, x.dim)
    stop(general.msg, " ", suggested)
}

suggestedSingleIndex <- function(x.name, x.dim) {
    valid.inds <- determineValidSingleInd(x.dim)
    required <- "When using the [ subscript, either reference values with integers (or strings)."
    suggested <- paste0("For example, ", x.name, " can be subscriptted with, ",
                        x.name, "[", valid.inds[1], "].")
    if (length(x.dim) > 1) {
        required <- sub(".$", ", or provide references for each dimension.", required)
        extra.suggestion <- paste0(" or ", x.name, "[", paste0(valid.inds, collapse = ", "), "].")
        suggested <- sub(".$", extra.suggestion, suggested)
    }
    paste(required, suggested)

}

suggestedDoubleIndex <- function(x.name, x.dim) {
    valid.inds <- determineValidDoubleInd(x.dim)
    required <- paste0("When using the [[ subscript, there either needs to be a single integer ",
                       "(or string) reference.")
    suggested <- paste0("For example, ", x.name, " can be subscriptted with, ",
                        x.name, "[[", valid.inds[1], "]].")
    if (length(x.dim) > 1) {
        required <- sub(".$", ", or one for each dimension.", required)
        extra.suggestion <- paste0(" or ", x.name, "[[", paste0(valid.inds, collapse = ", "), "]].")
        suggested <- sub(".$", extra.suggestion, suggested)
    }
    paste(required, suggested)
}

generalDoubleIndexMsg <- function(x.name, x.dim) {
    general.msg <- generalInvalidSubscriptMsg(x.name)
    suggested.syntax <- suggestedDoubleIndex(x.name, x.dim)
    paste(general.msg, suggested.syntax)
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

isBasicAttribute <- function(attribute.names, basic.attr = c("dim", "names", "dimnames", "class")) {
    attribute.names %in% basic.attr
}

updateTableAttributes <- function(y, x, called.args, evaluated.args) {
    class(y) <- c("qTable", class(y))
    y.attributes <- attributes(y)
    x.attributes <- attributes(x)
    y.required.attributes <-  isBasicAttribute(names(y.attributes))
    x.optional.attributes <- !isBasicAttribute(names(x.attributes))
    mostattributes(y) <- c(attributes(y)[y.required.attributes], # Attributes that define the structure of y
                           attributes(x)[x.optional.attributes]) # Attributes that enhance y as a QTable
    attr.names <- names(attributes(y))
    names.needing.update <- !isBasicAttribute(attr.names)
    names(attributes(y))[names.needing.update] <- paste0("original.", attr.names[names.needing.update])
    y <- updateSpanIfNecessary(y, x.attributes, evaluated.args)
    attr(y, "name") <- paste0(x.attributes[["name"]], "[",
                              paste(as.character(called.args), collapse = ","), "]")
    y
}

subscriptSpanDF <- function(span.attr, idx) {
    if (NCOL(span.attr) == 1L) return(invisible())
    if (is.character(idx))
        idx <- which(span.attr[[2]] %in% idx)
    out <- span.attr[idx, , drop = FALSE]
    if (all(is.na(out[[1L]]))) invisible() else out
}

updateSpanIfNecessary <- function(y, x.attributes, evaluated.args) {
    span.attribute <- x.attributes[["span"]]
    if (is.null(span.attribute)) return(y)
    dim.length <- length(x.attributes[["dim"]])
    if (dim.length > 1L && length(evaluated.args) == 1L) return(y)
    if (dim.length > 2L)
        evaluated.args <- evaluated.args[1:2]
    span.df <- mapply(subscriptSpanDF, span.attribute, evaluated.args, SIMPLIFY = FALSE)
    span.df <- Filter(Negate(is.null), span.df)
    if (length(span.df))
        attr(y, "span") <- span.df
    y
}
