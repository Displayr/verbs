#' Convert Q Tables to Tabular (2D) Format
#'
#' Flattens/melts a Q Table of more than two dimensions, dropping
#' multiple statistics if they are present and returning only the first in
#' a flattened matrix.
#' @param table A Q Table (an array of 1-5 dimensions with special
#'     attributes)
#' @param drop.stats.from.2d.table Logical; should multiple statistics
#'     be dropped from 2D Q Tables? Defaults to \code{FALSE} for the
#'     Table - Select feature in Displayr.
#' @return A matrix (2D array) containing a flattened version of
#'     \code{x} with only its first statistic (unless \code{table} is
#'     2D and \code{drop.stats.from.2d.table} is \code{FALSE}. It will
#'     contain multiple variables varying in possibly both dimensions,
#'     similar to how the table is displayed in Q and Displayr.
#' @export
FlattenTableAndDropStatisticsIfNecessary <- function(
                                                     table,
                                                     drop.stats.from.2d.table = FALSE)
{
    table.out <- table
    n.dims <- length(dim(table.out))

    has.multiple.stats <- is.null(attr(table.out, "statistic")) &&
        !is.null(attr(table.out, "questiontypes")) &&
        n.dims > 1L
    if (has.multiple.stats)
    {
        table.out <- DropMultipleStatisticsFromTable(table.out,
                                                     drop.stats.from.2d.table = drop.stats.from.2d.table,
                                                     warn = TRUE)
        attr(table, "statistic") <- attr(table.out, "statistic")
        n.dims <- n.dims - 1
    }

    if (hasRowSpan(table.out))
    {
        table.out <- updateTableNamesWithRowSpanLabels(table.out, table.out)
        if (n.dims == 3){
            qtypes <- attr(table.out, "questiontypes")
            if (qtypes[1] %in% c("PickOneMulti", "PickAnyGrid", "NumberGrid"))
                table.out <- FlattenQTableToMatrix(table.out, 1:2, 3)
            else  # Multi is in columns
                table.out <- FlattenQTableToMatrix(table.out, 2:1, 3)
        }else if (n.dims == 4)
            table.out <- FlattenQTableToMatrix(table.out, c(1, 3), c(2, 4))
        n.dims <- length(dim(table.out))
    }
    if (hasColSpan(table))
    {
        table.out <- updateTableNamesWithColSpanLabels(table.out, table.out)
        if (n.dims == 3){
            qtypes <- attr(table.out, "questiontypes")
            ## Multi is in rows, combine 2nd and 3rd dimensions of table
            if (qtypes[1] %in% c("PickOneMulti", "PickAnyGrid", "NumberGrid"))
                table.out <- FlattenQTableToMatrix(table.out, 1, 2:3)
            else
                table.out <- FlattenQTableToMatrix(table.out, 2:1, 3)
        }else if (n.dims == 4)  # e.g. Nominal - Multi by Binary - Gird
            table.out <- FlattenQTableToMatrix(table.out, c(1, 3), c(2, 4))
        n.dims <- length(dim(table.out))
    }
    table.out <- CopyAttributes(table.out, table)
    return(table.out)
}

#' Convert High-Dimensional Q Tables to Matrix
#'
#' Flattens/melts a Q Table with more than two dimensions into a
#' matrix using \code{ftable}, ensuring the output table has
#' informative  names.
#' @param x A 4D or 3D Q Table containing a single statistic to be
#'     converted to matrix.
#' @param row.dims length one or two integer vector specifying the
#'     dimensions of \code{x} to use for the rows of the output
#'     matrix.
#' @param col.dims length one or two integer vector specifying the
#'     dimensions of \code{x} to use for the rows of the output
#'     matrix.
#' @return A matrix (2D array) containing a flattened version of
#'     \code{x} with multiple variables varying in possibly both
#'     dimensions, similar to how the table is displayed in Q and
#'     Displayr.
#' @seealso \code{ftable}
#' @importFrom flipU CopyAttributes
#' @importFrom stats ftable
#' @export
FlattenQTableToMatrix <- function(x, row.dims, col.dims)
{
    out <- ftable(x, row.vars = row.dims, col.vars = col.dims)
    dnames <- dimnames(x)
    span <- attr(x, "span")

    .combineNames <- function (name.list, flip = FALSE)
    {
        if (flip)
            name.list <- rev(name.list)
        name.grid <- expand.grid(unname(name.list))
        paste(name.grid$Var2, name.grid$Var1, sep = " - ")
    }

    if (length(row.dims) == 2)
    {
        rownames(out) <- .combineNames(dnames[sort(row.dims)], flip = FALSE)
        if (!is.null(span) && !is.null(span$rows))
            span$rows <- data.frame(rownames(out), fix.empty.names = FALSE)
    }
    else if (length(row.dims) == 1)
        rownames(out) <- dnames[[row.dims]]
    else
        stop(sQuote("row.dims"), " is not the right length.")
    if (length(col.dims) == 2)
    {
        colnames(out) <- .combineNames(dnames[col.dims], flip = TRUE)
        if (!is.null(span) && !is.null(span$columns))
            span$columns <- data.frame(colnames(out), fix.empty.names = FALSE)
    }
    else if (length(col.dims) == 1)
        colnames(out) <- dnames[[col.dims]]
    else
        stop(sQuote("col.dims"), " is not the right length.")

    out <- CopyAttributes(out, x)
    return(out)
}

#' Select the first statistic from a Q Table
#'
#' Modifies a Q Table input containing multiple statistics, to an
#' identical Q Table containing only the first statistic.
#' @param table A Q Table (an \code{array} of 1 to 5 dimensions)
#'     containing multiple statistics (which vary along the final
#'     dimension)
#' @param warn Logical; should a warning be thrown when dropping
#'     statistics?
#' @param drop.stats.from.2d.table Logical; should multiple statistics
#'     be dropped from 2D Q Tables? Defaults to \code{FALSE} for the
#'     Table - Select feature in Displayr.
#' @return \code{table} with only the first element (statistic) from
#'     the last dimension
#' @export
DropMultipleStatisticsFromTable <- function(table,
                                            warn = TRUE,
                                            drop.stats.from.2d.table = FALSE)
{
    n.dims <- getDimensionLength(table)
    if (n.dims <= 2 && !drop.stats.from.2d.table)
        return(drop(table))

    args <- c(list(table), rep(alist(, )[1L], n.dims))
    origin.dim <- dim(table)
    original.dimnames <- dimnames(table)
    mapped.dims <- attr(table, "mapped.dimensions")
    stat.dim <- if (inherits(table, "qTable") && !is.null(mapped.dims)) {
        match("Statistic", names(mapped.dims), nomatch = n.dims)
    } else {
        n.dims
    }
    stat.name <- dimnames(table)[[stat.dim]][1L]#
    args[[stat.dim + 1L]] <- 1L
    out <- do.call(`[`, args)
    # Ensure dimension not dropped if any single length dimensions exist
    # e.g. 2x1x2 will become 2x1 (last dim is statistics)
    dim(out) <- origin.dim[-stat.dim]
    dimnames(out) <- original.dimnames[-stat.dim]
    out <- copyAttributesIfNotQTable(out, table)
    attr(out, "statistic") <- stat.name

    if (warn)
        warning("Multiple statistics detected in table, only the first, ",
                sQuote(stat.name), ", will be shown.")

    out
}

hasRowSpan <- function(table)  # NCOL(NULL) == 1L
    !is.null(attr(table, "span")) && NCOL(attr(table, "span")$rows) > 1L
    ## length(ls(pattern = "^formIncludeT[0-9]+RowSpan", envir = .GlobalEnv)) > 0

hasColSpan <- function(table)
    !is.null(attr(table, "span")) && !is.null(attr(table, "span")$columns) ## && NCOL(attr(table, "span")$column) > 1L
    ## length(ls(pattern = "^formIncludeT[0-9]+ColSpan", envir = .GlobalEnv)) > 0

removeNAsAndPasteRows <- function(char.mat)
    unname(apply(char.mat, 1, function(r)
        paste0(r[!is.na(r)], collapse = " - ")))

updateTableNamesWithRowSpanLabels <- function(table, table.orig)
{
    span <- attr(table.orig, "span")$rows
    if (NCOL(span) > 1L)
    {
        names.out <- removeNAsAndPasteRows(span)
        rownames(table) <- names.out
    }
    table
}

updateTableNamesWithColSpanLabels <- function(table, table.orig)
{
    span <- attr(table.orig, "span")$columns
    if (NCOL(span) > 1L)
    {
        names.out <- removeNAsAndPasteRows(span)
        colnames(table) <- names.out
    }
    table
}

updateTableRowSpanAttribute <- function(table, table.orig, keep.idx)
{
    if (is.character(keep.idx))
        keep.idx <- which(rownames(table.orig) %in% keep.idx)
    span <- attr(table.orig, "span")$rows
    span.out <- span[keep.idx, , drop = FALSE]
    attr(table, "span")$rows <- span.out
    table
}

updateTableColSpanAttribute <- function(table, table.orig, keep.idx)
{
    if (is.character(keep.idx))
        keep.idx <- which(colnames(table.orig) %in% keep.idx)
    span <- attr(table.orig, "span")$columns
    span.out <- span[keep.idx, , drop = FALSE]
    attr(table, "span")$columns <- span.out
    table
}

flattening3DRowDims <- function(x) {
    switch(x,
        "PickOneMulti" = ,
        "PickAnyGrid" = ,
        "NumberGrid" = 1L,
        2:1
    )
}

flattening3DColDims <- function(x) {
    switch(x,
        "PickOneMulti" = ,
        "PickAnyGrid" = ,
        "NumberGrid" = 2:3,
        3
    )
}

determineFlatteningRowAndColVars <- function(question.types = NULL, n.dim = 1L) {
    if (is.null(question.types))
        stop("Need question types to resolve row and column variables for flattening")
    stopifnot("n.dim should be a single integer" = is.numeric(n.dim) && length(n.dim) == 1L)
    if (n.dim < 3L || n.dim > 4L)
        stop("Flattening only supported for 3D and 4D tables")
    if (n.dim == 3L) {
        row.vars <- flattening3DRowDims(question.types[1])
        col.vars <- flattening3DColDims(question.types[1])
    } else {
        row.vars <- c(3L, 1L)
        col.vars <- c(2L, 4L)
    }
    list(row.vars = row.vars, col.vars = col.vars)
}

#' Flatten a QTable
#' @param x A QTable to be flattened
#' @param drop If a multi statistic table, should only the first statistic be retained.
#' @export
FlattenQTable <- function(x, drop = FALSE) {
    is.multi.stat <- isMultiStatTable(x)
    dim.x <- dim(x)
    dim.length <- length(dim.x)

    no.flattening.required <-  (is.multi.stat && dim.length <= 3L && !drop) ||
                              (!is.multi.stat && dim.length <= 2L)
    if (no.flattening.required)
        return(x)

    if (!is.multi.stat)
        return(flattenTable(x))

    original.class <- class(x)
    all.indices <- rep(alist(, )[1L], dim.length)
    x <- unclass(x)
    subscript.args <- c(list(x), all.indices)
    dimnames.x <- dimnames(x)
    statistics <- dimnames.x[[dim.length]]
    x.attr <- attributes(x)
    if (dim.length <= 3L && drop) {
        subscript.args[[dim.length + 1L]] <- 1L
        output <- do.call(`[`, subscript.args)
        new.class <- if (dim.length == 2L) { # Will now be a vector, coerce output to 1d array
            output <- as.array(output)
            setdiff(original.class, "matrix")
        } else
            original.class
        output <- addQTableAttributesToFlattenedTable(output, x.attr)
        attr(output, "statistic") <- statistics[1L]
        class(output) <- new.class
        return(output)
    }
    qtypes <- attr(x, "questiontype")
    if (drop) {
        statistics <- statistics[1L]
        subscript.args[[dim.length + 1L]] <- 1L
        subscript.args <- c(subscript.args, list(drop = TRUE))
        output <- do.call(`[`, subscript.args)
        x.attr <- attributes(x)
        x.attr[["statistic"]] <- statistics
        x.attr[["dim"]] <- dim(output)
        x.attr[["dimnames"]] <- dimnames(output)
        mostattributes(output) <- x.attr
        output <- flattenTable(output, add.attributes = TRUE)
        return(output)
    }
    output <- lapply(statistics, function(statistic, args) {
        args[[dim.length + 1L]] <- statistic
        single.stat.table <- do.call(`[`, args)
        attr(single.stat.table, "statistic") <- statistic
        attr(single.stat.table, "questiontype") <- qtypes
        flattenTable(single.stat.table, add.attributes = FALSE)
    }, args = subscript.args) |> setNames(statistics)
    output <- simplify2array(output, except = 0L)
    output <- addQTableAttributesToFlattenedTable(output, x.attr)
    class(output) <- original.class
    output
}

# See tests in test-flatten.R to see how this works
createFlattenedNames <- function(x) {
    if (length(x) == 1L) {
        return(x[[1L]])
    }
    x.lengths <- lengths(x)
    outer <- rep(x[[1L]], each = x.lengths[[2L]])
    inner <- rep(x[[2L]], x.lengths[[1L]])
    paste0(outer, " - ", inner)
}

flattenNames <- function(x) {
    var.names <- attributes(x)[c("row.vars", "col.vars")] |> unname()
    if (identical(var.names[[1L]], 2:1))
        var.names[[1L]] <- rev(var.names[[1L]])
    lapply(var.names, createFlattenedNames)
}

#' @param x The flattened matrix of class \code{ftable} without dimnames
#' @noRd
assignNamesToFlattenedTable <- function(x) {
    new.names <- flattenNames(x)
    dimnames(x) <- new.names
    x
}

flattenTable <- function(x, add.attributes = TRUE) {
    if (isMultiStatTable(x))
        stop("Multi statistic tables not supported for flattenTable")
    n.dim <- getDimensionLength(x)
    question.types <- attr(x, "questiontype")
    settings <- determineFlatteningRowAndColVars(question.types, n.dim)
    # Spans not supported since not all spans appear in table attributes
    output <- ftable(x, row.vars = settings[["row.vars"]], col.vars = settings[["col.vars"]])
    output <- assignNamesToFlattenedTable(output)
    # Remove the ftable class and its attributes
    attributes(output)[c("row.vars", "col.vars")] <- NULL
    output <- unclass(output)
    if  (add.attributes)
        output <- addQTableAttributesToFlattenedTable(output, attributes(x))
    output
}

#' @param flattened.table The new flattened table which needs attributes updated.
#' @param x.attr The original table attributes
#' @noRd
addQTableAttributesToFlattenedTable <- function(flattened.table, x.attr) {
    if (is.array(flattened.table)) {
        x.attr[["dim"]] <- dim(flattened.table)
        x.attr[["dimnames"]] <- dimnames(flattened.table)
    } else
        x.attr[["names"]] <- names(flattened.table)
    q.stat.info <- x.attr[["QStatisticsTestingInfo"]]
    x.attr[["QStatisticsTestingInfo"]] <- addFlattenedDimensionsToQStatInfo(q.stat.info, x.attr[["dimnames"]])
    x.attr[["questiontype"]] <- updateFlattenedQuestionTypes(x.attr[["questiontype"]])
    x.attr[["name"]] <- updateFlattenedName(x.attr[["name"]])
    mostattributes(flattened.table) <- x.attr
    flattened.table
}

updateFlattenedName <- function(x) {
    if (is.null(x)) return(x)
    paste0("FlattenTable(", x, ")")
}

addFlattenedDimensionsToQStatInfo <- function(q.stat.info, new.dimnames) {
    if (is.null(q.stat.info)) return(NULL)
    if (NROW(q.stat.info) == 1L || is.null(new.dimnames)) return(q.stat.info)
    dimnames.lengths <- lengths(new.dimnames)
    if (length(dimnames.lengths) == 1L)
        return(q.stat.info)
    cols <- rep(new.dimnames[[2L]], dimnames.lengths[[1L]])
    rows <- rep(new.dimnames[[1L]], each = dimnames.lengths[[2L]])
    cbind(Row = rows, Column = cols, q.stat.info)
}

remapQuestionType <- function(x) {
    switch(x,
        "NumberGrid" = "NumberMulti",
        "PickAnyGrid" = "PickAny",
        "PickOneMulti" = "PickAny",
        x # return same name as default
    )
}

updateFlattenedQuestionTypes <- function(question.types) {
    if (is.null(question.types)) return(NULL)
    vapply(question.types, remapQuestionType, character(1L), USE.NAMES = FALSE)
}
