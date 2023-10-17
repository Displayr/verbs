#' @export
`[.QTable` <- function(x, ..., drop = TRUE) {
    if (!qTableSubscriptingPermitted())
        return(NextMethod(`[`, x))
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

    # Force array class for custom QTable subscripting code
    input.is.not.array <- !is.array(x)
    if (input.is.not.array)
        x <- as.array(x)

    DUPLICATE.LABEL.SUFFIX  <- "_@_"
    x <- deduplicateQTableLabels(x, DUPLICATE.LABEL.SUFFIX)

    x.dim <- dim(x)
    n.dim <- length(x.dim)
    if (n.dim > 0 && !is.null(dimnames(x)) && is.null(names(dimnames(x))))
        x <- nameDimensionAttributes(x)

    n.index.args <- nargs() - 1L - !missing(drop)
    # Throw a nicer error if the indexing is not appropriate
    if (n.index.args != 1 && n.dim != n.index.args)
        throwErrorTableIndexInvalid(input.name, x.dim, dimnames(x))

    missing.names <- is.null(dimnames(x))
    if (missing.names)  # Add names for subsetting QStatisticsTestingInfo
        dimnames(x) <- makeNumericDimNames(dim(x))

    y <- NextMethod(`[`, x)

    called.args <- as.list(called.args[["..."]])
    ## Need to evaluate the arguments here to alleviate possible NSE issues; c.f.:
    ## http://adv-r.had.co.nz/Computing-on-the-language.html#calling-from-another-function
    evaluated.args <- called.args
    for (i in seq_along(called.args))
        if (!identical(as.character(called.args[[i]]), ""))
            evaluated.args[[i]] <- eval(called.args[[i]], parent.frame())

    # Update Attributes here
    y <- updateTableAttributes(y, x, called.args, evaluated.args, drop = drop,
                               missing.names, DUPLICATE.LABEL.SUFFIX)
    y <- updateNameAttribute(y, attr(x, "name"), called.args, "[")
    throwWarningIfDuplicateLabels(x, evaluated.args, sep = DUPLICATE.LABEL.SUFFIX)
    y <- removeDeduplicationSuffixFromLabels(y, DUPLICATE.LABEL.SUFFIX)

    if (missing.names)
        y <- unname(y)

    if (input.is.not.array && is.array(y))
        y <- dropTableToVector(y)

    y
}

#' @export
`[[.QTable` <- function(x, ..., exact = TRUE) {
    if (!qTableSubscriptingPermitted())
        return(NextMethod(`[[`, x))
    # Use sys.call as match.call captures the quoted arguments as names
    used.arguments <- names(sys.call())
    input.name <- attr(x, "name")
    if (!validArgumentNames(used.arguments, "exact"))
        throwErrorOnlyNamed("exact", "[[")
    if ("exact" %in% used.arguments && !is.logical(exact))
        stop("exact argument should be TRUE or FALSE")
    called.args <- match.call(expand.dots = FALSE)
    empty.ind <- providedArgumentEmpty(called.args, optional.arg = "exact")
    # Force array class for custom QTable subscripting code
    input.is.not.array <- !is.array(x)
    if (input.is.not.array)
        x <- as.array(x)

    DUPLICATE.LABEL.SUFFIX  <- "_@_"
    x <- deduplicateQTableLabels(x, DUPLICATE.LABEL.SUFFIX)

    x.dim <- dim(x)

    if (empty.ind)
        throwErrorEmptyDoubleIndex(input.name, x.dim)

    n.dim <- length(x.dim)
    n.index.args <- nargs() - 1L - !missing(exact)
    correct.n.args <- n.index.args == n.dim
    called.args <- as.list(called.args[["..."]])

    ## Need to evaluate the arguments here to alleviate possible NSE issues; c.f.:
    ## http://adv-r.had.co.nz/Computing-on-the-language.html#calling-from-another-function
    evaluated.args <- called.args
    for (i in seq_along(called.args))
        if (!identical(as.character(called.args[[i]]), ""))
            evaluated.args[[i]] <- eval(called.args[[i]], parent.frame())

    single.arg <- length(evaluated.args) == 1L
    all.unit.length <- all(lengths(evaluated.args) == 1L)
    valid.args <- all.unit.length && (single.arg || correct.n.args)

    if (!valid.args)
        throwErrorTableDoubleIndex(input.name, x.dim)

    if (n.dim > 0 && !is.null(dimnames(x)) && is.null(names(dimnames(x))))
        x <- nameDimensionAttributes(x)

    missing.names <- is.null(dimnames(x))
    if (missing.names)
        dimnames(x) <- makeNumericDimNames(dim(x))

    y <- NextMethod(`[[`, x)

    # Update Attributes here
    y <- updateTableAttributes(y, x, called.args, evaluated.args, drop = TRUE, missing.names)
    y <- updateNameAttribute(y, attr(x, "name"), called.args, "[[")
    y <- removeDeduplicationSuffixFromLabels(y, DUPLICATE.LABEL.SUFFIX)
    if (missing.names)
        y <- unname(y)
    y
}

dropTableToVector <- function(x) {
    old.x <- x
    old.x.attributes <- attributes(x)
    x <- as.vector(x)
    attributes(x) <- old.x.attributes[!names(old.x.attributes) %in% c("dim", "dimnames", "class")]
    names(x) <- names(old.x)
    class(x) <- c("qTable", "QTable", class(x))
    x
}

updateNameAttribute <- function(y, original.name, called.args, subscript.type = "[")
{
    end.char <- if (subscript.type == "[") "]" else "]]"
    subscript.args <- paste0(subscript.type, paste(as.character(called.args), collapse = ","), end.char)
    attr(y, "name") <- paste0(original.name, subscript.args)
    y
}

# named arguments to [ or [[ are the input itself (x) and i and j references
# There is an optional argument of drop for `[` and exact for `[[`
default.args <- c("", "x", "i", "j")

validArgumentNames <- function(arg.names, optional.arg = NULL) {
    all(arg.names %in% c(default.args, optional.arg))
}

providedArgumentEmpty <- function(called.args, optional.arg) {
    named.args <- names(called.args)
    named.args[3L] == optional.arg || isEmptyList(called.args[3L])
}

isEmptyArg <- function(x) length(x) == 1L && x == alist(, )[1L][[1L]]
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

throwErrorTableIndexInvalid <- function(x, x.dim, x.dimnames) {
    general.msg <- generalInvalidSubscriptMsg(x)
    suggested <- suggestedSingleIndex(x, x.dim, x.dimnames)
    stop(general.msg, " ", suggested)
}

suggestedSingleIndex <- function(x.name, x.dim, x.dimnames) {
    valid.inds <- determineValidSingleInd(x.dim)
    required <- "When using the [ subscript, either reference values with integers (or strings)."
    suggested <- paste0("For example, ", x.name, " can be subscripted with, ",
                        x.name, "[", valid.inds[1], "].")
    if (length(x.dim) > 1) {
        required <- sub(".$", ", or provide references for each dimension.", required)
        extra.suggestion <- paste0(" or ", x.name, "[", paste0(valid.inds, collapse = ", "), "].")
        suggested <- sub(".$", extra.suggestion, suggested)
        dim.names <- names(x.dimnames)
        if (!is.null(dim.names))
            suggested <- paste0(suggested, " The indices should be specified in the following order: ",
                                paste0(tolower(dim.names), collapse = ", "), ".")
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

#' Check character vector contains Q Table attribute names.
#' @param attribute.names The character vector of attributes to inspect
#' @param qtable.attrs Optional character vector to check attribute names against.
#'                     Defaults to the know names of attributes for QTables.
#' @return A logical vector if the input character vector contains Q Tabel attribute names
#' @export
IsQTableAttribute <- function(attribute.names,
                              qtable.attrs = c("statistic", "dim", "dimnames", "mapped.dimnames",
                                               "dimnets", "dimduplicates", "span",
                                               "basedescriptiontext", "basedescription",
                                               "QStatisticsTestingInfo", "questiontypes",
                                               "footerhtml", "name", "questions",
                                               "is.subscripted", "is.transposed")) {
    attribute.names %in% qtable.attrs
}

updateTableAttributes <- function(y, x, called.args, evaluated.args, drop = TRUE,
                                  original.missing.names = FALSE, sep = "_@_") {
    class(y) <- c("QTable", class(y))
    y.attributes <- attributes(y)
    x.attributes <- attributes(x)
    y.required.attributes <-  isBasicAttribute(names(y.attributes))
    x.optional.attributes <- !isBasicAttribute(names(x.attributes))
    mostattributes(y) <- c(attributes(y)[y.required.attributes], # Attributes that define the structure of y
                           attributes(x)[x.optional.attributes]) # Attributes that enhance y as a QTable

    y <- updateAttributeNames(y)
    y <- updateStatisticAttr(y, x.attributes, evaluated.args, drop = drop)
    # Ensure the question types match the dimensions of the QTable
    x.attributes <- updateQuestionTypesIfDoesntMatchDim(x.attributes)
    y <- updateQuestionTypesAttr(y, x.attributes, evaluated.args, drop = drop)
    y <- updateQStatisticsTestingInfo(y, x.attributes, evaluated.args, original.missing.names, sep)
    y <- updateNameDimensionAttr(y, x.attributes[["dim"]])
    y <- updateSpanIfNecessary(y, x.attributes, evaluated.args)
    y <- updateIsSubscriptedAttr(y, x)
    y <- keepMappedDimnames(y)
    y
}

updateNameDimensionAttr <- function(y, x.dim) {
    if (!is.null(dimnames(y)) && length(dim(y)) < length(x.dim))
        y <- nameDimensionAttributes(y)
    y
}

updateAttributeNames <- function(y) {
    attr.names <- names(attributes(y))
    ## Don't rename the following Attributes
    ### 1. statistic attribute, since it only appears on 1-stat QTables and won't change
    ### 2. QStatisticsTestingInfo to save storage
    ### 3. questions as the input questions to original table dont change
    DONT.RENAME.ATTRS <- c("statistic", "QStatisticsTestingInfo", "questions")
    qtable.attr.names <- setdiff(eval(formals(IsQTableAttribute)$qtable.attrs),
                                 DONT.RENAME.ATTRS)
    names.needing.update <- IsQTableAttribute(attr.names, qtable.attr.names) &
                            !isBasicAttribute(attr.names)
    names(attributes(y))[names.needing.update] <- paste0("original.", attr.names[names.needing.update])
    y
}

updateIsSubscriptedAttr <- function(y, x) {
    is.subscripted.attr <- attr(y, "is.subscripted")
    if (isTRUE(is.subscripted.attr)) return(y)
    attr(y, "is.subscripted") <- !(identical(dim(y), dim(x)) &&
                                   identical(dimnames(y), dimnames(x)) &&
                                   identical(as.vector(y), as.vector(x)))
    y
}

keepMappedDimnames <- function(x) {
    attr(x, "mapped.dimnames") <- dimnames(x)
    names(dimnames(x)) <- NULL
    x
}

assignStatisticAttr <- function(y, stat.attr) {
    attr(y, "statistic") <- stat.attr
    y
}

recycleArray <- function(x, required.dim) {
    if (identical(dim(x), required.dim)) return(x)
    array(x, dim = required.dim)
}

determineStatisticDimension <- function(x.attr) {
    # Table hasn't been transposed, should be last dim
    n.transposes <- x.attr[["is.transposed"]]
    x.dim <- x.attr[["dim"]]
    if (is.null(n.transposes)) {
        return(length(x.dim))
    }
    x.dimnames <- x.attr[["dimnames"]]
    # If dimnames entry is entirely NULL, then it is a side effect of transpose
    null.dimnames <- vapply(x.dimnames, is.null, logical(1L))
    if (any(null.dimnames))
        return(which(!null.dimnames))
    # Otherwise the dimnames are complete (has row and col)
    1L + (n.transposes %% 2L == 0L)
}

updateStatisticAttr <- function(y, x.attr, evaluated.args, drop = TRUE) {
    if (!is.null(x.attr[["statistic"]])) {
        attr(y, "statistic") <- x.attr[["statistic"]]
        return(y)
    }
    if (!drop) return(y)
    statistic.dim <- determineStatisticDimension(x.attr)
    x.dim <- x.attr[["dim"]]
    x.dimnames <- x.attr[["dimnames"]]
    stat.names <- x.dimnames[[statistic.dim]]
    # Avoid assigning statistic if dimnames are the fallback integers as chars
    if (any(grepl(r"(^[0-9]*$)", stat.names)))
        return(y)
    single.arg <- length(evaluated.args) == 1L && !isEmptyArg(evaluated.args[[1L]])
    if (single.arg && !is.array(evaluated.args[[1L]]))
        evaluated.args[[1L]] <- arrayInd(evaluated.args[[1L]], .dim = x.dim)
    # Handle array referencing
    if (single.arg && is.array(evaluated.args[[1L]])) {
        arg <- evaluated.args[[1L]]
        if (is.logical(arg)) {
            arg <- recycleArray(arg, required.dim = x.dim)
            arg <- which(arg, arr.ind = TRUE)
        }
        stats.referenced <- unique(arg[, ncol(arg)])
        if (length(stats.referenced) > 1L) return(y)
        attr(y, "statistic") <- stat.names[stats.referenced]
        return(y)
    }
    n.statistics <- length(stat.names)
    has.single.stat <- n.statistics == 1L
    if (has.single.stat) {
        y <- assignStatisticAttr(y, stat.names)
        return(y)
    }
    empty.arg <- isEmptyArg(evaluated.args[[statistic.dim]])
    stat.arg <- if (empty.arg) x.dimnames[[statistic.dim]] else evaluated.args[[statistic.dim]]
    statistics <- if (is.character(stat.arg)) {
        stat.names[which(stat.names == stat.arg)]
    } else if (is.logical(stat.arg)) {
        stat.arg <- recycleArray(stat.arg, x.dim[statistic.dim])
        stat.arg <- unique(which(stat.arg))
        stat.names[stat.arg]
    } else {
        stat.names[unique(stat.arg)]
    }
    if (length(statistics) == 1L)
        y <- assignStatisticAttr(y, statistics)
    y
}

makeNumericDimNames <- function(dim)
{
    n.digits <- nchar(max(dim))
    dim.names <- lapply(dim, seq_len)
    dim.names <- lapply(dim.names, sprintf, fmt = paste0("%0", n.digits, "i"))
    return(nameDimensionAttributes(dim.names))
}

updateQStatisticsTestingInfo <- function(y, x.attributes, evaluated.args,
                                         orig.missing.names, sep = "_@_")
{
    q.test.info <- x.attributes[["QStatisticsTestingInfo"]]
    if (is.null(q.test.info))
        return(y)

    if (prod(dim(y)) == 0L) {
        attr(y, "QStatisticsTestingInfo") <- NULL
        return(y)
    }

    dim.x <- x.attributes[["dim"]]
    dimnames.x <- x.attributes[["dimnames"]]

    dim.len <- length(dim.x)
    has.been.transposed <- !is.null(x.attributes[["is.transposed"]])
    is.multi.stat <- is.null(x.attributes[["statistic"]])
    is.multi.stat.transposed <- has.been.transposed && is.multi.stat && length(dim.x) > 1L
    # If the table is a transposed vector, then a dimnames element will be NULL
    null.dimnames <- vapply(dimnames.x, is.null, logical(1L))
    is.transposed.vector <- has.been.transposed && any(null.dimnames)

    if (is.multi.stat.transposed || is.transposed.vector)
        return(determineQStatInfoForTransposedTable(y, x.attributes, evaluated.args))

    if (is.multi.stat && length(evaluated.args) > 1)
    {
        evaluated.args <- evaluated.args[-length(evaluated.args)]
        dimnames.x <- dimnames.x[-dim.len]
        dim.x <- dim.x[-dim.len]
        dim.len <- dim.len - 1
    }
    qtypes <- x.attributes[["questiontypes"]]

    vector.or.single.dim.output <- is.null(dim(y)) || length(y) == 1L || getDimensionLength(y) == 1L
    if (vector.or.single.dim.output)
    {
        keep.rows <- getQTestInfoIndexForVectorOutput(
            evaluated.args =  evaluated.args,
            dimnames.x = dimnames.x,
            qtypes = qtypes,
            has.multi.stat.dim = is.multi.stat && length(evaluated.args) == 1L,
            q.stat.info.len = nrow(q.test.info)
        )
        attr(y, "QStatisticsTestingInfo") <- q.test.info[keep.rows, , drop = FALSE]
        return(y)
    }
    idx.array <- array(FALSE, dim = dim.x, dimnames = dimnames.x)
    idx.array <- do.call(`[<-`, c(list(idx.array), evaluated.args, value = TRUE))
    if (!is.array(idx.array)) # Need an array for aperm
        idx.array <- as.array(idx.array)
    perm <- rowMajorDimensionPermutation(dim.len, qtypes)
    idx.array <- aperm(idx.array, perm)  # match(seq_len(dim.len), perm)

    keep.rows <- which(idx.array)

    q.test.info <- addArrayIndicesIfMissing(q.test.info, y, dimnames.x, qtypes, sep)
    q.test.info <- q.test.info[keep.rows, ]

    ## Drop columns from array indices corresponding to dropped dimensions of table
    dim.names.names <- names(dimnames(y))
    dropped.dim <- if (!is.null(dim.names.names)) {
        !names(dimnames.x) %in% dim.names.names
    } else {
        vapply(
            q.test.info[, names(dimnames.x)],
            function(col) all(col == col[1L]),
            logical(1L)
        )
    }

    if (!is.null(dimnames(y)) && length(dim(y)) < length(dim.x) + is.multi.stat)
        y <- nameDimensionAttributes(y)

    updated.qtypes <- attr(y, "questiontypes")
    dim.y <- dim(y)
    new.dim.len <- length(dim.y)
    new.dim.names.names <- names(dimnames(y))
    updated.is.multistat <- !is.null(new.dim.names.names) &&
        new.dim.names.names[new.dim.len] == "Statistic"
    if (updated.is.multistat)
    {
        dim.y <- dim.y[new.dim.len]
        new.dim.len <- new.dim.len - 1
        new.dim.names.names <- new.dim.names.names[-length(new.dim.names.names)]
    }
    if (any(dropped.dim))
    {
        q.test.info <- q.test.info[, !colnames(q.test.info) %in% names(dimnames.x)[dropped.dim]]
        if (!all(dropped.dim))
            colnames(q.test.info)[seq_len(new.dim.len)] <- new.dim.names.names
    }
    if (length(y) > 1 && !all(dropped.dim))
        for (i in seq_len(new.dim.len))
            q.test.info[, i] <- droplevels(q.test.info[, i])

    ## Reorder q.test.info to be row-major by forming (correctly-ordered) indices
    ##  for output table and finding matches in original array indices
    perm <- rowMajorDimensionPermutation(new.dim.len, updated.qtypes)
    if (new.dim.len > 1)
    {
        new.df.ord <- expand.grid(dimnames(y)[perm])[, new.dim.names.names]
        new.idx.str <- apply(new.df.ord, 1, paste0, collapse = "")
        curr.idx.str <- apply(q.test.info[, seq_len(ncol(new.df.ord))], 1, paste0,
                              collapse = "")
        new.ord <- match(new.idx.str, curr.idx.str)
        q.test.info <- q.test.info[new.ord, ]
    }else if (is.multi.stat && !is.null(q.test.info[["Row"]]))
    {
        new.idx.str <- dimnames(y)[[1]]
        curr.idx.str <- q.test.info[, "Row"]
        new.ord <- match(new.idx.str, curr.idx.str)
        q.test.info <- q.test.info[new.ord, ]
    }

    ##  if no labels on original table, create new numeric indices based on new dimensions
    if (orig.missing.names && any(colnames(q.test.info) %in% new.dim.names.names))
        q.test.info[, new.dim.names.names] <-
        expand.grid(makeNumericDimNames(dim.y)[perm])[, new.dim.names.names]

    attr(y, "QStatisticsTestingInfo") <- q.test.info
    y
}

#' Output a numeric vector, say perm,  with length equal to dim.len such that
#'   for any single-stat. QTable, x, with z-Statistics in cells:
#'   as.vector(aperm(x, perm)) == q.test.info[,"zstatistic"]
#' @noRd
rowMajorDimensionPermutation <- function(dim.len, qtypes)
{
    grid.types <- c("PickAnyGrid", "PickOneMulti", "NumberGrid")
    grid.in.cols <- length(qtypes) > 1 && qtypes[2] %in% grid.types

    if (grid.in.cols)
        perm <- switch(dim.len, NaN, 2:1, c(3, 1, 2), c(4, 2, 1, 3), c(4, 2, 1, 3, 5))
    else
        perm <- dim.len:1
    return(perm)
}

getQTestInfoIndexForVectorOutput <- function(evaluated.args, dimnames.x, qtypes,
                                             has.multi.stat.dim, q.stat.info.len)
{
    dim.x <- vapply(dimnames.x, length, 1L)
    dim.len <- length(dim.x)
    # 1. Form array of column-major indices and subset it using the evaluated.args
    idx.array.cmajor <- array(1:prod(dim.x), dim = dim.x)
    dimnames(idx.array.cmajor) <- dimnames.x
    kept.idx <- do.call(`[`, c(list(idx.array.cmajor), evaluated.args, drop = FALSE))

    ## need to compute kept.idx on full table, but in multi-stat case, q.test.info will
    ## only have prod(dim.x[-dim.len]) rows (stats are always in last dim.), so ignore
    ## indices greater than this
    if (has.multi.stat.dim && dim.len > 1L)
    {
        idx.array.cmajor <- array(seq_len(q.stat.info.len), dim = dim.x[-dim.len])
        perm <- rowMajorDimensionPermutation(dim.len - 1, qtypes)  # perm[perm != max(perm)]
    }else
        perm <- rowMajorDimensionPermutation(dim.len, qtypes)

    ## 2. undo previous aperm call so attribute retains row-major order
    if (!is.null(dim(kept.idx)))
        kept.idx <- as.vector(aperm(kept.idx, match(seq_len(dim.len), perm)))

    kept.idx <- kept.idx[kept.idx <= q.stat.info.len]

    q.test.info.rmajor.idx <- as.vector(aperm(idx.array.cmajor, perm))
    q.test.info.rmajor.idx <- q.test.info.rmajor.idx[q.test.info.rmajor.idx <= q.stat.info.len]

    ## 3. Subset data.frame attr, keeping rows from rmajor.idx that are still in
    ##   cmajor.idx after subsetting
    df.idx <- match(kept.idx, q.test.info.rmajor.idx)
    return(df.idx)
}

QTABLE.DIM.NAMES.ALLOWED <- c("Row", "Column", "Inner Row", "Outer Column",
                              "Outer Row", "Inner Column")


addArrayIndicesIfMissing <- function(q.test.info, y, dim.names, qtypes, sep = "_@_")
{
    ## if multi-stat table subsetted to a vector, no need to add indices
    if (length(dim.names) == 1L && length(dim.names[[1L]]) > nrow(q.test.info))
        return(q.test.info)

    orig.questions <- attr(y, "original.questions")
    is.raw.table <- !is.null(orig.questions) && "RAW DATA" %in% orig.questions
    if (is.raw.table)
        return(q.test.info)

    col.idx <- colnames(q.test.info) %in% QTABLE.DIM.NAMES.ALLOWED
    indices.already.present <- any(col.idx)
    if (indices.already.present)
        return(q.test.info)
    dim.len <- length(dim.names)
    is.multi.stat <- !is.null(names(dim.names)) && names(dim.names)[dim.len] == "Statistic"
    if (is.multi.stat)
    {
        dim.len <- dim.len - 1
        dim.names <- dim.names[-length(dim.names)]
    }
    perm <- rowMajorDimensionPermutation(dim.len, qtypes)
    arr.idx <- expand.grid(lapply(dim.names[perm], make.unique, sep = sep))
    if (NCOL(arr.idx) > 1)
        arr.idx <- arr.idx[, names(dim.names)]
    return(cbind(arr.idx, q.test.info))
}

findReferencedSlices <- function(evaluated.arg, x.attributes, arg.to.inspect) {
    dim.x <- x.attributes[["dim"]]
    dimnames.x <- x.attributes[["dimnames"]]
    result <- if (isEmptyArg(evaluated.arg)) {
        seq_len(dim.x[arg.to.inspect])
    } else if (is.character(evaluated.arg)) {
        which(dimnames.x[[arg.to.inspect]] %in% evaluated.arg)
    } else if (is.logical(evaluated.arg)) {
        which(evaluated.arg)
    } else
        evaluated.arg
    if (anyDuplicated(result))
        result <- unique(result)
    result
}

determineQStatInfoForTransposedTable <- function(y, x.attributes, evaluated.args) {
    q.stat <- x.attributes[["QStatisticsTestingInfo"]]
    dim.x <- x.attributes[["dim"]]
    dimnames.x <- x.attributes[["dimnames"]]
    null.dimnames <- vapply(dimnames.x, is.null, logical(1L))
    if (all(null.dimnames)) return(y)
    is.row.vector <- null.dimnames[1L]
    is.multi.stat <- is.null(x.attributes[["statistic"]])
    single.arg <- length(evaluated.args) == 1L
    relevant.indices <- if (single.arg) {
        arg <- evaluated.args[[1L]]
        if (isEmptyArg(arg)) return(y)
        if (!is.row.vector && !is.multi.stat) {
            relevant.indices <- findReferencedSlices(arg, x.attributes, 1L)
            attr(y, "QStatisticsTestingInfo") <- q.stat[relevant.indices, ]
            return(y)
        }
        arg.is.matrix <- is.matrix(arg)
        if (arg.is.matrix && is.integer(arg) && ncol(arg) == 2L) {
            new.mat <- array(FALSE, dim = dim.x)
            new.mat[arg] <- TRUE
            arg <- new.mat
        }
        if (!(arg.is.matrix && is.logical(arg) && identical(dim(arg), dim.x))) {
            attr(y, "QStatisticsTestingInfo") <- NULL
            return(y)
        }
        # Must be logical matrix here
        references <- consistentReferences(arg)
        if (is.null(references)) {
            attr(y, "QStatisticsTestingInfo") <- NULL
            return(y)
        }
        references
    } else {
        arg.to.inspect <- if (is.multi.stat || is.row.vector) 2L else 1L
        findReferencedSlices(evaluated.args[[arg.to.inspect]], x.attributes, arg.to.inspect)
    }
    q.stat <- q.stat[relevant.indices, ]
    attr(y, "QStatisticsTestingInfo") <- q.stat
    y
}

consistentReferences <- function(logical.matrix) {
    col.refs <- split(logical.matrix, row(logical.matrix))
    col.refs <- Filter(any, col.refs)
    if (length(col.refs) == 1L || Reduce(identical, col.refs)) return(col.refs[[1L]])
    row.refs <- split(logical.matrix, col(logical.matrix))
    row.refs <- Filter(any, row.refs)
    if (length(row.refs) == 1L || Reduce(identical, row.refs)) return(row.refs[[1L]])
    NULL
}

qTableDimensionNames <- function(dim.len, q.types = NULL, is.multi.stat = FALSE)
{
    if (dim.len < 0 || dim.len > 5)
        return(dim.len)
    if (dim.len <= 1L)
    {
        if (is.multi.stat)
            return("Statistic")
        return("Row")
    }

    if (!is.null(q.types)) {
        q.dims <- questionDimension(q.types)
        q.dims.string <- paste0(q.dims, collapse = "")
        dim.names <- switch(q.dims.string,
                            "1" = "Row",
                            "2" = c("Row", "Column"),
                            "11" = c("Row", "Column"),
                            "12" = c("Inner Row", "Outer Row", "Column"),
                            "21" = c("Row", "Outer Column", "Inner Column"),
                            "22" = c("Inner Row", "Outer Column", "Outer Row", "Inner Column"))
        if (length(dim.names) != dim.len - is.multi.stat)
            dim.names <- c("Row", "Column")[1:(dim.len - is.multi.stat)]
    } else {
        dim.names <- switch(dim.len - is.multi.stat,
                            "Row",
                            c("Row", "Column"),
                            c("Inner Row", "Outer Row", "Inner Column"),
                            c("Inner Row", "Outer Column", "Outer Row", "Inner Column"))
    }
    if (is.multi.stat)
        dim.names <- c(dim.names, "Statistic")

    dim.names
}

subscriptSpanDF <- function(span.attr, idx) {
    if (is.null(span.attr) || NROW(span.attr) == 0L) return(span.attr)
    if (isEmptyArg(idx)) return(span.attr)
    if (is.character(idx))
        idx <- which(span.attr[[NCOL(span.attr)]] %in% idx)
    out <- span.attr[idx, , drop = FALSE]
    if (all(is.na(out[[1L]]))) out[-1L] else out
}

updateSpanIfNecessary <- function(y, x.attributes, evaluated.args) {
    span.attribute <- x.attributes[["span"]]
    if (is.null(span.attribute)) return(y)
    if (all(vapply(span.attribute, length, 0L) == 0L))
        return(structure(y, span = span.attribute))
    x.dim <- x.attributes[["dim"]]
    dim.length <- length(x.dim)
    # Span will be dropped if single indexing argument (vector or matrix etc) used on an array
    # with more than 1 dimension. The dimension isn't retained on base R here and the spans lose utility
    if (dim.length > 1L && length(evaluated.args) == 1L) return(y)
    original.q.types <- x.attributes[["questiontypes"]]
    total.original.q.dim <- sum(questionDimension(original.q.types))
    if (length(original.q.types) == 1L || total.original.q.dim <= 2)
    {
        if (length(evaluated.args) > 2L) evaluated.args <- evaluated.args[1:2]
        span.df <- mapply(subscriptSpanDF, span.attribute, evaluated.args, SIMPLIFY = FALSE)
        span.df <- Filter(ncol, span.df)
        if (length(span.df)) attr(y, "span") <- span.df
        return(y)
    }
    question.types <- attr(y, "questiontypes")
    if (is.null(question.types)) return(y)
    dim.names <- attr(y, "dimnames")
    question.dims <- questionDimension(question.types) |> paste0(collapse = "")
    new.row.span <- switch(question.dims,
                           "12" = dim.names[["Inner Row"]],
                           "21" = dim.names[["Row"]],
                           "22" = dim.names[["Inner Row"]])
    new.col.span <- switch(question.dims,
                           "12" = dim.names[["Outer Row"]],
                           "21" = dim.names[["Outer Column"]],
                           "22" = dim.names[["Outer Column"]])

    attr(y, "span") <- list("rows" = data.frame(new.row.span, fix.empty.names = FALSE),
                            "columns" = data.frame(new.col.span, fix.empty.names = FALSE))
    y
}

#' @param x A QTable of a list of dimnames of a QTable
#' @noRd
nameDimensionAttributes <- function(x)
{
    dim.len <- ifelse(is.list(x), length(x), length(dim(x)))
    if (dim.len == 0 || dim.len > 5)
        return(x)
    is.multi.stat <- !is.list(x) && is.null(attr(x, "statistic"))
    q.types <- attr(x, "questiontypes")
    dim.names <- qTableDimensionNames(dim.len, q.types, is.multi.stat)

    if (is.list(x))
    {
        names(x) <- dim.names
        return(x)
    }

    dimnames.x <- dimnames(x)
    if (!is.null(dimnames.x))
    {
        names(dimnames.x) <- dim.names
        dimnames(x) <- dimnames.x
    }
    return(x)
}

is2DQuestion <- function(x) x %in% c("PickOneMulti", "PickAnyGrid", "NumberGrid")

# Return the number of dimensions for a given question type.
# This can either be 1 or 2.
#' @param question.types A vector of characters corresponding to question types
#' @noRd
questionDimension <- function(question.types) {
    q.dims <- rep(1, length(question.types))
    q.dims[is2DQuestion(question.types)] <- 2
    q.dims
}

isArgDroppingDim <- function(arg, x.dim) {
    if (is.symbol(arg)) return(x.dim == 1L)
    if (is.numeric(arg) && any(arg < 0))
        arg <- setdiff(seq_len(x.dim), -arg)
    length(arg) == 1L

}

singleUnique <- function(x) all(x == x[1L])

isSingleArgDroppingDim <- function(arg, x.dim) {
    if (!is.array(arg)) {
        if (is.logical(arg)) {
            arg <- recycleArray(arg, x.dim)
            arg <- which(arg, arr.ind = TRUE)
        }
        if (is.character(arg)) return(length(arg) == 1L)
        return(apply(arrayInd(arg, .dim = x.dim), 2,
                     singleUnique,
                     simplify = TRUE))
    }
    if (is.logical(arg))
        arg <- which(arg, arr.ind = TRUE)
    apply(arg, 2L, singleUnique, simplify = TRUE)
}

updateQuestionTypesFromArgs <- function(dropped.dims, question.type) {
    if (all(dropped.dims)) return(NULL)
    if (!is2DQuestion(question.type)) return(question.type)
    if (any(dropped.dims)) return(dropQuestionType(question.type))
    question.type
}

getFallbackQuestionType <- function(statistics)
{
    if (!is.null(statistics) && any(endsWith(statistics, "%")))
        return("PickAny")
    else return("Number")
}

dropQuestionType <- function(question.type) {
    if (startsWith(question.type, "Pick")) "PickAny" else "NumberMulti"
}

# To avoid problems that can arrise if a Rule has
# modified a QTable and provided incorrect
# questiontypes attribute.
updateQuestionTypesIfDoesntMatchDim <- function(x.attr) {
    is.multi.stat <- is.null(x.attr[["statistic"]])
    q.types <- x.attr[["questiontypes"]]
    q.type.dims <- questionDimension(q.types)
    implied.dim <- sum(q.type.dims)
    actual.dim <- length(x.attr[["dim"]]) - is.multi.stat
    if (implied.dim > actual.dim) {
        # identify which question type is too large to accomodate the table dimension
        deficit <- implied.dim - actual.dim
        if (deficit == 1) {
            drop.idx <- which.max(q.type.dims)
            q.types[drop.idx] <- dropQuestionType(q.types[drop.idx])
        } else if (deficit == 2 && length(q.types) == 2) {
            q.types <- vapply(q.types, FUN = dropQuestionType, FUN.VALUE = character(1))
        } else {
            # Fallback option
            q.types <- rep("PickAny", actual.dim)
        }
        x.attr[["questiontypes"]] <- q.types
    }
    x.attr
}

updateQuestionTypesAttr <- function(y, x.attr, evaluated.args, drop = TRUE) {
    x.question.types <- x.attr[["questiontypes"]]
    if (is.null(x.question.types))
        return(y)
    if (!drop) {
        attr(y, "questiontypes") <- x.question.types
        return(y)
    }
    if (identical(x.question.types, character(0L)) || any(startsWith(x.question.types, "Text"))) {
        attr(y, "questiontypes") <- x.question.types
        return(y)
    }

    if (identical(x.question.types, rep("Number", 2L))) {
        attr(y, "questiontypes") <- x.question.types
        return(y)
    }

    if ("Number" %in% x.question.types && all(!is2DQuestion(x.question.types))) {
        attr(y, "questiontypes") <- x.question.types
        return(y)
    }
    x.dim <- x.attr[["dim"]]
    y.dimnames <- dimnames(y)
    x.dimnames <- x.attr[["dimnames"]]
    # Check if dim same after accounting for multi stat
    is.multi.stat <- is.null(x.attr[["statistic"]])
    relevant.x.dim <- x.dim
    single.arg <- length(evaluated.args) == 1L
    x.is.vector <- length(x.dim) == 1L
    if (is.multi.stat && !x.is.vector) { # Remove the multiple statistics from this
        relevant.x.dim <- relevant.x.dim[-length(x.dim)]
        if (!single.arg)
            evaluated.args <- evaluated.args[-length(x.dim)]
    }
    if (identical(names(y.dimnames), names(x.dimnames))) { # Nothing to do
        attr(y, "questiontypes") <- x.question.types
        return(y)
    }
    if (single.arg) {
        arg <- evaluated.args[[1L]]
        dropped.dims <- isSingleArgDroppingDim(arg, relevant.x.dim)
        if (is.multi.stat && is.array(arg))
            dropped.dims <- dropped.dims[-length(dropped.dims)]
    } else
        dropped.dims <- mapply(isArgDroppingDim, evaluated.args, relevant.x.dim, SIMPLIFY = TRUE)
    # For each dimension, does it correspond to question 1 (rows) or
    # question 2 (columns)
    q.numbers.per.dim <- rep(seq_along(x.question.types), questionDimension(x.question.types))
    new.question.types <- if (length(dropped.dims) == length(q.numbers.per.dim)) {
        dims.used.per.q <- split(dropped.dims, q.numbers.per.dim)
        unlist(mapply(updateQuestionTypesFromArgs,
                      dims.used.per.q, x.question.types,
                      SIMPLIFY = TRUE, USE.NAMES = FALSE))
    }
    if (is.null(new.question.types))
    {
        stat.names <- if (is.multi.stat)
                          x.dimnames[[length(x.dimnames)]]
                      else x.attr[["statistic"]]
        new.question.types <- getFallbackQuestionType(stat.names)
    }
    attr(y, "questiontypes") <- new.question.types
    y
}

#' @export
summary.QTable <- function(object, ...)
    summary(unclass(object), ...)

deduplicateQTableLabels <- function(x, sep = "_@_")
{
    original.names <- dimnames(x)
    if (is.null(original.names))
        return(x)
    new.names <- lapply(original.names,
                        function(x)
                        {
                            if (is.character(x))
                                make.unique(x, sep  = sep)
                            else x
                        })

    dimnames(x) <- new.names
    return(x)
}

removeDeduplicationSuffixFromLabels <- function(x, sep = "_@_")
{
    original.names <- dimnames(x)
    if (is.null(original.names))
        return(x)
    dimnames(x) <- lapply(original.names, sub, pattern = paste0(sep, "[0-9]+$"),
                          replacement = "")
    return(x)
}

throwWarningIfDuplicateLabels <- function(x, evaluated.args, sep = "_@")
{
    checkDim <- function(labels, subscript, sep = "_@_")
    {
        if (!is.character(subscript))
            return(NULL)
        patt <- paste0(sep, "[0-9]+$")
        duplicated <- grepl(patt, labels)
        intersect(sub(patt, "", labels[duplicated]), subscript)
    }
    duplicate.labels <- unlist(mapply(checkDim, dimnames(x), evaluated.args,
                                      MoreArgs = list(sep = sep)))
    if (length(duplicate.labels))
    {
        label.str <- paste0(unique(duplicate.labels), collapse = ", ")
        warning("Duplicate ", ngettext(length(duplicate.labels), "label", "labels"),
                " present in the input table: ", label.str, ".")
    }
    return(invisible())
}


qTableSubscriptingPermitted <- function() {
    q.function <- get0("allowQTableSubscripting", envir = .GlobalEnv, mode = "function")
    if (!is.null(q.function)) {
        result <- q.function()
        return(is.logical(result) && result)
    }
    productNameOrIsNotQ()
}

productNameOrIsNotQ <- function() {
    product.name <- get0("productName", envir = .GlobalEnv)
    if (is.null(product.name)) return(TRUE)
    length(product.name) == 1L && product.name != "Q"
}
