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
    if (n.dim > 0 && !is.null(dimnames(x)) && is.null(names(dimnames(x))))
        x <- nameDimensionAttributes(x)

    n.index.args <- nargs() - 1L - !missing(drop)
    # Throw a nicer error if the indexing is not appropriate
    if (n.index.args != 1 && n.dim != n.index.args)
        throwErrorTableIndexInvalid(input.name, x.dim)

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
    y <- updateTableAttributes(y, x, called.args, evaluated.args, drop = drop)
    if (missing.names)
    {
        y <- unname(y)
        if (is.null(attr(y, "questiontypes")))
            names(dim(y)) <- NULL
    }

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

    missing.names <- is.null(dimnames(x))
    if (missing.names)
        dimnames(x) <- makeNumericDimNames(dim(x))

    y <- NextMethod(`[`, x)
    # Update Attributes here
    y <- updateTableAttributes(y, x, called.args, drop = TRUE)
    if (missing.names)
    {
        y <- unname(y)
        if (is.null(attr(y, "questiontypes")))
            names(dim(y)) <- NULL
    }

    y
}

validArgumentNames <- function(arg.names, optional.arg = NULL) {
    all(arg.names %in% c("", optional.arg))
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

isQTableAttribute <- function(attribute.names,
                              qtable.attrs = c("statistic", "dim", "dimnames",
                                               "dimnets", "dimduplicates", "span",
                                               "basedescriptiontext", "basedescription",
                                               "QStatisticsTestingInfo", "questiontypes",
                                               "footerhtml", "name", "questions")) {
    attribute.names %in% qtable.attrs
}

updateTableAttributes <- function(y, x, called.args, evaluated.args, drop = TRUE) {
    class(y) <- c("qTable", class(y))
    y.attributes <- attributes(y)
    x.attributes <- attributes(x)
    y.required.attributes <-  isBasicAttribute(names(y.attributes))
    x.optional.attributes <- !isBasicAttribute(names(x.attributes))
    mostattributes(y) <- c(attributes(y)[y.required.attributes], # Attributes that define the structure of y
                           attributes(x)[x.optional.attributes]) # Attributes that enhance y as a QTable

    # Ensure y retains its array structure, as subscripting assumes the input is an array
    if (!is.array(y))
        y <- as.array(y)
    attr.names <- names(attributes(y))

    ## Don't rename statistic attribute, since it only appears on 1-stat QTables
    ##  and won't change and don't rename QStatisticsTestingInfo to save storage
    DONT.RENAME.ATTRS <- c("statistic", "QStatisticsTestingInfo")
    qtable.attr.names <- setdiff(eval(formals(isQTableAttribute)$qtable.attrs),
                                 DONT.RENAME.ATTRS)
    names.needing.update <- isQTableAttribute(attr.names, qtable.attr.names) &
                                !isBasicAttribute(attr.names)
    names(attributes(y))[names.needing.update] <- paste0("original.", attr.names[names.needing.update])
    y <- updateSpanIfNecessary(y, x.attributes, evaluated.args)
    attr(y, "name") <- paste0(x.attributes[["name"]], "[",
                              paste(as.character(called.args), collapse = ","), "]")
    attr(y, "questiontypes") <- getUpdatedQuestionTypes(y, x)
    y <- updateStatisticAttr(y, x.attributes, evaluated.args, drop = drop)
    y <- updateQStatisticsTestingInfo(y, x.attributes, evaluated.args)
    if (!is.null(dimnames(y)) && length(dim(y)) < length(x.attributes[["dim"]]))
        y <- nameDimensionAttributes(y)

    y
}

assignStatisticAttr <- function(y, stat.attr) {
    attr(y, "statistic") <- stat.attr
    y
}

recycleArray <- function(x, required.dim) {
    if (identical(dim(x), required.dim)) return(x)
    array(x, dim = required.dim)
}

updateStatisticAttr <- function(y, x.attr, evaluated.args, drop = TRUE) {
    if (!is.null(x.attr[["statistic"]])) {
        attr(y, "statistic") <- x.attr[["statistic"]]
        return(y)
    }
    if (!drop) return(y)
    single.arg <- length(evaluated.args) == 1L && !isEmptyArg(evaluated.args[[1L]])
    x.dim <- x.attr[["dim"]]
    n.dim <- length(x.dim)
    x.dimnames <- x.attr[["dimnames"]]
    stat.names <- x.dimnames[[n.dim]]
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
    n.statistics <- x.dim[n.dim]
    has.single.stat <- n.statistics == 1L
    if (has.single.stat) {
        y <- assignStatisticAttr(y, stat.names)
        return(y)
    }
    empty.arg <- isEmptyArg(evaluated.args[[n.dim]])
    last.arg <- if (empty.arg) x.dimnames[[n.dim]] else evaluated.args[[n.dim]]
    if (is.character(last.arg)) {
        statistics <- stat.names[which(stat.names == last.arg)]
    } else if (is.logical(last.arg)) {
        last.arg <- recycleArray(last.arg, x.dim[n.dim])
        last.arg <- unique(which(last.arg))
        statistics <- stat.names[last.arg]
    } else {
        statistics <- stat.names[unique(last.arg)]
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

updateQStatisticsTestingInfo <- function(y, x.attributes, evaluated.args)
{
    q.test.info <- x.attributes[["QStatisticsTestingInfo"]]
    if (is.null(q.test.info))
        return(y)

    dim.x <- x.attributes[["dim"]]
    dimnames.x <- x.attributes[["dimnames"]]

    missing.names <- is.null(dimnames.x)
    if (missing.names)
    {

        dimnames.x <- makeNumericDimNames(dim.x)
        dimnames(y) <- makeNumericDimNames(dim(y))
    }
    dim.len <- length(dim.x)
    is.multi.stat <- is.null(x.attributes[["statistic"]])

    if (is.multi.stat && length(evaluated.args) > 1)
    {
        evaluated.args <- evaluated.args[-length(evaluated.args)]
        dimnames.x <- dimnames.x[-dim.len]
        dim.x <- dim.x[-dim.len]
        dim.len <- dim.len - 1
    }
    qtypes <- x.attributes[["questiontypes"]]
    grid.types <- c("PickAnyGrid", "PickOneMulti", "NumberGrid")
    grid.in.cols <- length(qtypes) > 1 && qtypes[2] %in% grid.types

    ## For any single-stat. qTable, x, with z-Statistics in cells:
    ##   as.vector(aperm(x, perm)) == q.test.info[,"zstatistic"]
    if (grid.in.cols)
        perm <- switch(dim.len, NaN, 2:1, c(3, 1, 2), c(4, 2, 1, 3))
    else
        perm <- dim.len:1

    vector.output <- length(dim(y)) <= 1 && length(evaluated.args) == 1
    if (vector.output)
    {
        keep.rows <- getQTestInfoIndexForVectorOutput(evaluated.args, dimnames.x, perm)
    }else
    {
        idx.array <- array(FALSE, dim = dim.x, dimnames = dimnames.x)
        idx.array <- do.call(`[<-`, c(list(idx.array), evaluated.args, value = TRUE))
        if (dim.len > 1L)
            idx.array <- aperm(idx.array, perm)  # match(seq_len(dim.len), perm)
        keep.rows <- which(idx.array)
    }

    q.test.info <- addArrayIndicesIfMissing(q.test.info, y, dimnames.x, perm)  # [perm]
    q.test.info <- q.test.info[keep.rows, ]

    ## Drop columns from array indices corresponding to dropped dimensions of table
    dim.names.names <- names(dimnames(y))
    if (!is.null(dim.names.names))
        dropped.dim <- !names(dimnames.x) %in% dim.names.names
    else if (vector.output)
        dropped.dim <- seq_along(dim.x)
    else
        dropped.dim <- vapply(q.test.info[, names(dimnames.x)],
                              function(col) length(unique(col)) == 1L, logical(1L))

    if (!is.null(dimnames(y)) && length(dim(y)) < length(dim.x))
        y <- nameDimensionAttributes(y)

    updated.qtypes <- attr(y, "questiontypes")
    new.dim.len <- length(dim(y))
    new.dim.names.names <- names(dimnames(y))
    updated.is.multistat <- !is.null(new.dim.names.names) &&
        new.dim.names.names[new.dim.len] == "Statistic"
    if (updated.is.multistat)
    {
        new.dim.len <- new.dim.len - 1
        dropped.dim <- dropped.dim[-length(dropped.dim)]
        new.dim.names.names <- new.dim.names.names[-length(new.dim.names.names)]
    }
    if (any(dropped.dim))
    {
        q.test.info <- q.test.info[, !colnames(q.test.info) %in% names(dimnames.x)[dropped.dim]]
        if (!all(dropped.dim))
            colnames(q.test.info)[seq_len(new.dim.len)] <- new.dim.names.names
    }

    ## Reorder q.test.info to be row-major by forming (correctly-ordered) indices
    ##  for output table and finding matches in original array indices
    grid.in.cols <- length(updated.qtypes) > 1 && updated.qtypes[2] %in% grid.types
    if (grid.in.cols)
        perm <- switch(new.dim.len, NaN, 2:1, c(3, 1, 2), c(4, 2, 1, 3))
    else
        perm <- new.dim.len:1
    if (new.dim.len > 1)
    {
        new.df.ord <- expand.grid(dimnames(y)[perm])[, new.dim.names.names]
        new.idx.str <- apply(new.df.ord, 1, paste0, collapse = "")
        curr.idx.str <- apply(q.test.info[, seq_len(ncol(new.df.ord))], 1, paste0,
                              collapse = "")
        new.ord <- match(new.idx.str, curr.idx.str)
        q.test.info <- q.test.info[new.ord, ]
    }
    # if no labels on original table, create new numeric indices based on new dimensions
    if (missing.names)
        q.test.info[, new.dim.names.names] <- expand.grid(lapply(dim(y), seq_len)[perm])

    attr(y, "QStatisticsTestingInfo") <- q.test.info
    y
}

getQTestInfoIndexForVectorOutput <- function(evaluated.args, dimnames.x, perm)
{
    dim.x <- vapply(dimnames.x, length, 1L)
    dim.len <- length(dim.x)
    # 1. Form array of column-major indices and subset it using the evaluated.args
    idx.array.cmajor <- array(1:prod(dim.x), dim = dim.x)
    dimnames(idx.array.cmajor) <- dimnames.x
    kept.idx <- do.call(`[`, c(list(idx.array.cmajor), evaluated.args, drop = FALSE))
    ## 2. undo previous aperm call so attribute retains row-major order
    if (!is.null(dim(kept.idx)))
        kept.idx <- as.vector(aperm(kept.idx, match(seq_len(dim.len), perm)))

    q.test.info.rmajor.idx <- as.vector(aperm(idx.array.cmajor, perm))

    ## 3. Subset data.frame attr, keeping rows from rmajor.idx that are still in
    ##   cmajor.idx after subsetting
    df.idx <- match(kept.idx, q.test.info.rmajor.idx)
    return(df.idx)
}

addArrayIndicesIfMissing <- function(q.test.info, y, dim.names, perm)
{
    QTABLE.DIM.NAMES.ALLOWED <- c("Row", "Column", "Inner Row", "Outer Column",
                                  "Outer Row", "Inner Column")
    col.idx <- colnames(q.test.info) %in% QTABLE.DIM.NAMES.ALLOWED
    indices.already.present <- any(col.idx)
    if (indices.already.present)
        return(q.test.info)
    arr.idx <- expand.grid(dim.names[perm])
    if (NCOL(arr.idx) > 1)
        arr.idx <- arr.idx[, names(dim.names)]
    # col.ord <- match(colnames(arr.idx), QTABLE.DIM.NAMES.ALLOWED)
    # if (all(!is.na(col.ord)))
    #     arr.idx <- arr.idx[, QTABLE.DIM.NAMES.ALLOWED[sort(col.ord)],
    #                        drop = FALSE]
    return(cbind(arr.idx, q.test.info))
}

qTableDimensionNames <- function(dim.len, q.types = NULL, is.multi.stat = FALSE)
{
    if (dim.len < 0 || dim.len > 5)
        return(dim.len)
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
    if (isEmptyArg(idx)) return(span.attr)
    if (is.character(idx))
        idx <- which(span.attr[[NCOL(span.attr)]] %in% idx)
    out <- span.attr[idx, , drop = FALSE]
    if (all(is.na(out[[1L]]))) invisible() else out
}

updateSpanIfNecessary <- function(y, x.attributes, evaluated.args) {
    span.attribute <- x.attributes[["span"]]
    if (is.null(span.attribute)) return(y)
    x.dim <- x.attributes[["dim"]]
    dim.length <- length(x.dim)
    # Span will be dropped if single indexing argument (vector or matrix etc) used on an array
    # with more than 1 dimension. The dimension isn't retained on base R here and the spans lose utility
    if (dim.length > 1L && length(evaluated.args) == 1L) return(y)
    if (dim.length > 2L && length(evaluated.args) > 2L)
        evaluated.args <- evaluated.args[1:2]
    span.df <- mapply(subscriptSpanDF, span.attribute, evaluated.args, SIMPLIFY = FALSE)
    span.df <- Filter(Negate(is.null), span.df)
    if (length(span.df))
        attr(y, "span") <- span.df
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
    # has.questiontypes <- !is.null(q.types)
    dim.names <- qTableDimensionNames(dim.len, q.types, is.multi.stat)

    if (is.list(x))
    {
        names(x) <- dim.names
        return(x)
    }

    dimnames.x <- dimnames(x)
    if (!is.null(dimnames.x))
    {
        names(dim(x)) <- dim.names
        names(dimnames.x) <- dim.names
        dimnames(x) <- dimnames.x
    }
    # attr(x, "dim.types") <- type.per.dim
    return(x)
}

# Return the number of dimensions for a given question type.
# This can either be 1 or 2.
#' @param question.types A vector of characters corresponding to question types
#' @noRd
questionDimension <- function(question.types) {
    q.dims <- rep(1, length(question.types))
    q.dims[question.types %in% c("PickOneMulti", "PickAnyGrid", "NumberGrid")] <- 2
    q.dims
}

# Determine new questiontypes by comparing the new QTable to the original
#' @param new The newly-subscripted QTable
#' @param original The original QTable pre-subscripting
#' @noRd
getUpdatedQuestionTypes <- function(new, original) {
    orig.q.types <- attr(original, "questiontypes")
    if (is.null(orig.q.types))
        return(NULL)

    if (length(dim(new)) == length(dim(original))) # Nothing to do
        return(orig.q.types)

    # When one dimension remains, its dimname has been dropped.
    # So need to infer questiontype.
    if (length(dim(new)) == 1) {
        # Try to match the names to the dimnames of the original array
        matching.dims <- vapply(dimnames(original),
                                FUN = function(x) all(names(new) %in% x),
                                FUN.VALUE = logical(1))
        q.type.per.dim <- getQuestionTypeForEachDimension(orig.q.types)
        q.type.of.matched.dim <- unique(q.type.per.dim[matching.dims])
        if (length(q.type.of.matched.dim) == 1) {
            # Uniquely matched names to an original dimension so can
            # use the corresponding question type.
            q.dim <- questionDimension(q.type.of.matched.dim)
            return(dropQuestionTypeDimensions(q.type.of.matched.dim, q.dim - 1))
        }
        # Can't match so just do the best you can based on the statistic
        new.q.type <- "PickAny"
        stat <- attr(new, "statistic")
        if (!is.null(stat)) {
            if (!grepl("%", stat)) { # Not obviously categorical
                new.q.type <- "NumberMulti"
            }
        }
        return(new.q.type)
    }

    # Identify question types of remaining dimensions and drop appropriately
    orig.dims <- dim(original)
    is.multi.stat <- is.null(attr(original, "statistic"))
    if (is.multi.stat)
        orig.dims <- orig.dims[-length(orig.dims)]

    # For each dimension, does it correspond to question 1 (rows) or
    # question 2 (columns)
    q.numbers.per.dim <- rep(seq_along(orig.q.types), questionDimension(orig.q.types))

    new.dims <- dim(new)
    dropped.dims <- !names(orig.dims) %in% names(new.dims)
    dropped.qs <- q.numbers.per.dim[dropped.dims]
    if (all(is.na(dropped.qs)))
        return(orig.q.types)
    new.first.q <- dropQuestionTypeDimensions(orig.q.types[1], Sum(dropped.qs == 1))
    new.second.q <- ""
    if (length(orig.q.types) == 2)
        new.second.q <- dropQuestionTypeDimensions(orig.q.types[2], Sum(dropped.qs == 2))
    new.q.types <- c(new.first.q, new.second.q)

    # If any question dropped entirely return only one q type
    new.q.types <- new.q.types[nzchar(new.q.types)]
    return(new.q.types)
}

# Form a new question type by dropping dimensions from an existing one.
# Used to update the questiontypes for a subscripted QTable.
#' @param question.type A character referring to a questiontype from a QTable, e.g. PickOneMulti
#' @param Dimensions The number of dimnensions to drop from the question. Must be less than
#' or equal to the number of dimensions for the questiontype. For example a PickOne question
#' has a single dimension, and a PickOneMulti has two. If all dimensions are dropped,
#' a blank string is returned, indicating that this questiontype should be dropped from the
#' questiontypes attribute.
#' @noRd
dropQuestionTypeDimensions <- function(question.type, dimensions) {
    q.dim <- questionDimension(question.type)
    # Question eliminated entirely?
    if (q.dim == dimensions)
        return("")
    # If we get to here then Chris has made a mistake
    if (dimensions > q.dim)
        stop(question.type, " cannot drop ", dimensions, " dimensions!")
    # Probably never get to here
    if (dimensions == 0)
        return(question.type)

    return(switch(question.type,
        "PickOneMulti" = "PickAny",
        "PickAnyGrid" = "PickAny",
        "NumberGrid" = "NumberMulti"))
}

# Given a vector of question type strings (from the questiontypes attribute)
# return a vector whose length is equal to the number of dimensions of the
# corresponding QTable (modulo the statistic dimension) which describes
# which question type each dimension belongs to. For example, if the
# question types are "PickOne" and "PickOneMulti", then the vector will
# tell us the first dimesion corresponds to the PickOne question and
# that dimensions 2 and 3 correspond to the PickOneMulti.
#' @param question.types A character vector indicating the questiontypes
#' from a QTable.
#' @noRd
getQuestionTypeForEachDimension <- function(question.types) {
    # For each dim, what question type does it belong to?
    q.dims <- questionDimension(question.types)
    question.types[rep(seq_along(question.types), q.dims)]
}
