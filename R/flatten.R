FlattenTableAndDropStatisticsIfNecessary <- function(table)
{
    table.out<- table
    n.dims <- length(dim(table.out)

    has.multiple.stats <- is.null(attr(table.out "statistic")) &&
        !is.null(attr(table.out "questiontypes")) &&
        n.dims > 1L
    if (has.multiple.stats)
    {
        table.out<- DropMultipleStatisticsFromTable(table.out warn = TRUE)
        attr(table, "statistic") <- attr(table.out "statistic")
        n.dims <- n.dims - 1
    }

    if (hasRowSpan(table.out)
    {
        if (n.dims == 3){
            qtypes <- attr(table.out "questiontypes")
            if (qtypes[1] %in% c("PickOneMulti", "PickAnyGrid", "NumberGrid"))
                table.out<- flattenQTableToMatrix(table.out 1:2, 3)
            else  # Multi is in rows, combine 2nd and 3rd dimensions of table
                table.out<- flattenQTableToMatrix(table.out 1, 2:3)
        }else if (n.dims == 4)
            table.out<- flattenQTableToMatrix(table.out c(1, 3), c(2, 4))
        else
            table.out<- updateTableNamesWithRowSpanLabels(table.out table)
        n.dims <- length(dim(table.out)
    }
    if (hasColSpan(table))
    {
        if (n.dims == 3){
            qtypes <- attr(table.out "questiontypes")
            if (qtypes[1] %in% c("PickOneMulti", "PickAnyGrid", "NumberGrid"))
                table.out<- flattenQTableToMatrix(table.out 1, 2:3)
            else  # Multi is in rows, combine 2nd and 3rd dimensions of table
                table.out<- flattenQTableToMatrix(table.out 1:2, 3)
        }else if (n.dims == 4)  # e.g. Nominal - Multi by Binary - Gird
            table.out<- flattenQTableToMatrix(table.out c(1, 3), c(2, 4))
        else
            table.out<- updateTableNamesWithColSpanLabels(table.out table.out
        n.dims <- length(dim(table.out)
    }
    table.out<- flipU::CopyAttributes(table.out table)
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
#' @export
FlattenQTableToMatrix <- function(x, row.dims, col.dims){
    out <- ftable(x, row.vars = row.dims, col.vars = col.dims)
    dnames <- dimnames(x)
    if (length(row.dims) == 2)
        rownames(out) <- flipTables:::combineNames(dnames[row.dims], flip = FALSE)
    else if (length(row.dims) == 1)
        rownames(out) <- dnames[[row.dims]]
    else
        stop(sQuote("row.dims"), " is not the right length.")
    if (length(col.dims) == 2)
        colnames(out) <- flipTables:::combineNames(dnames[col.dims], flip = TRUE)
    else if (length(col.dims) == 1)
        colnames(out) <- dnames[[col.dims]]
    else
        stop(sQuote("col.dims"), " is not the right length.")

    out <- flipU::CopyAttributes(out, table)
    return(out)
}

#' Select the first statistic from a Q Table
#'
#' Modifies a Q Table input containing multiple statistics, to an
#' identical Q Table containing only the first statistic
#' @param table A Q Table (an \code{array} of 1 to 5 dimensions)
#'     containing multiple statistics (which vary along the final
#'     dimension)
#' @param warn Logical; should a warning be thrown when dropping
#'     statistics?
#' @return \code{table} with only the first element (statistic) from
#'     the last dimension
#' @export
DropMultipleStatisticsFromTable <- function(table, warn = TRUE)
{
    n.dims <- length(dim(table))
    if (n.dims <= 2)
        return(drop(table))

    stat.name <- dimnames(table)[[n.dims]][1]
    if (warn)
        warning("Multiple statistics detected in table, only the first, ",
                sQuote(stat.name), ", will be shown.")

    ## select first statistic from last dimension of table
    ## array() is used to ensure that any dimensions of the
    ## table that are length-1 are not dropped
    if (n.dims == 3)
        out <- array(table[, , 1], dim = dim(table)[-n.dims])
    else if (n.dims == 4)
        out <- array(table[, , , 1], dim = dim(table)[-n.dims])
    else if (n.dims == 5)
        out <- array(table[, , , , 1], dim = dim(table)[-n.dims])
    dimnames(out) <- dimnames(table)[-n.dims]
    out <- flipU::CopyAttributes(out, table)
    attr(out, "statistic") <- stat.name
    return(out)
}

hasRowSpan <- function(table)  # NCOL(NULL) == 1L
    !is.null(attr(table, "span")) && NCOL(attr(table, "span")$rows) > 1L
    ## length(ls(pattern = "^formIncludeT[0-9]+RowSpan", envir = .GlobalEnv)) > 0

hasColSpan <- function(table)
    !is.null(attr(table, "span")) && !is.null(attr(table, "span")$columns) ## && NCOL(attr(table, "span")$column) > 1L
    ## length(ls(pattern = "^formIncludeT[0-9]+ColSpan", envir = .GlobalEnv)) > 0


removeNAsAndPasteRows <- function(char.mat)
    apply(char.mat, 1, function(r) paste0(r[!is.na(r)], collapse = " - "))

updateTableNamesWithRowSpanLabels <- function(table, table.orig)
{
    span <- attr(table.orig, "span")$row
    names.out <- removeNAsAndPasteRows(span)
    rownames(table) <- names.out
    table
}

updateTableNamesWithColSpanLabels <- function(table, table.orig)
{
    span <- attr(table.orig, "span")$columns
    names.out <- removeNAsAndPasteRows(span)
    colnames(table) <- names.out
    table
}

updateTableRowSpanAttribute <- function(table, table.orig, keep.idx)
{
    span <- attr(table.orig, "span")$rows
    span.out <- span[keep.idx, , drop = FALSE]
    attr(table.orig, "span")$rows <- span.out
    table
}

updateTableColSpanAttribute <- function(table, table.orig, keep.idx)
{
    span <- attr(table.orig, "span")$columns
    span.out <- span[keep.idx, , drop = FALSE]
    attr(table, "span")$columns <- span.out
    table
}

