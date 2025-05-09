#' Select from row and columns of tables
#'
#' Subsets tables supporting a number of different modes for
#' specifying the rows and columns to retain. In addition to the usual
#' integer and character vector subsetting similar to \code{[}, first
#' and last rows and columns can be retained similar to
#' \code{\link{head}} and \code{\link{tail}}, and ranges of indices or
#' dates can be specified as a character string. Special support for
#' selecting from tables with dates in the rows and columns is also
#' provided.
#' @param table A table (\code{matrix} or \code{array}) to select
#'     from.
#' @param row.selection.mode String specifying how to select rows to
#'     include in the output table. One of
#' \enumerate{
#' \item \code{"vector"} - The default mode, use to specify integer, logical, or
#'     character vector indices to perform the selection similar to
#'     \code{[}.
#' \item \code{"first rows"} - Similar to \code{\link{head}}, use this mode to
#' select the first \code{n} rows of the table, where \code{n} is supplied
#' in \code{row.selections}.
#' \item \code{"last rows"} - Similar to \code{\link{tail}}, use this mode to
#' select the last rows of the table.
#' \item \code{"First date-time periods"} - Use this mode if the row names of
#' the table contain dates to select the first rows of the table within a
#' certain time period specified by \code{unit} (e.g. within the first three days
#' from the first date in the rows names of \code{table}).
#' \item \code{"Last date-time periods"} - For tables containing dates, this
#' mode allows for selecting the last rows of the table falling in the specified
#' time period (e.g. within the last three quarters of the final date in the
#' rows names).
#' \item \code{"range"} - use this mode to supply a range of indices to select
#' \item \code{"date range"} - use this mode to supply a range of dates to select
#' when the rows of \code{table} contains dates.
#' }
#' @param row.selections For \code{row.selection.mode = "vector"} an integer
#' vector providing the row indices of \code{table} to include in the output, a logical
#' vector with the same length as rows in the table, or a
#' character vector containing the row names to include. When
#' \code{row.selection.mode} is \code{"First rows"}, \code{"Last rows"},
#' \code{"First date-time periods"}, or \code{"Last date-time periods"}; a
#' scalar integer specifying the number of first/last rows to include in
#' the output. When \code{row.selection.mode = "range"}, a string containing a
#' (possibly comma-separated) list of indices and ranges to include; e.g.
#' \code{"5-8"} or \code{"1,3-5,9"}. When \code{row.selection.mode = "date range"},
#' a string containing two dates separated by \code{"--"}; e.g.,
#' \code{"2002-01-15--2004-06-30"}. Only rows with names containing dates
#' that lie between the two dates in the range are included in the output.
#' @param column.selection.mode as \code{row.selection.mode} except
#'     specifies the mode to use for selecting columns. Possible
#'     values are \code{"vector"}, \code{"first columns"},
#'     \code{"last columns"}, \code{"range"}, \code{"date range"},
#'     \code{"first date-time periods"},
#'     \code{"last date-time periods"}.
#' @param column.selections same as \code{row.selections} except for
#'     rows.
#' @param unit See \code{\link{First}}, the unit of time to consider
#'     when the selection mode uses dates. Ignored if the selection
#'     mode does not involve dates. Either a scalar or length-2 vector
#'     to use different units in rows and columns.
#' @param calendar Only used when selection mode involves dates. A
#'     logical scalar that indicates whether to consider calendar
#'     periods when the selection mode involves dates. Can also be
#'     length-2 vector to use different options in rows and
#'     columns. See Details in \code{\link{First}}.
#' @param ... Additional arguments passed to \code{\link{First}}.
#' @return A matrix containing the selected rows and columns of the
#'     table specified by \code{table.to.show}.
#' @details For Q Tables containing multiple statistics, only the
#'     first statistic will be shown in the output table (with a
#'     warning); unless the table contains a single-variable Variable
#'     Set, which permits selecting statistics using
#'     \code{column.selections}.
#'
#' Q Tables that stored as array of more than two dimensions are first
#' flatten using \code{\link{FlattenTableAndDropStatisticsIfNecessary}}.
#' @seealso \code{\link{First}}, \code{\link{Last}},
#'     \code{\link{FlattenTableAndDropStatisticsIfNecessary}}
#' @importFrom flipTime AsDate AsDateTime
#' @examples
#' x <- matrix(1:6, 3, 2)
#' rownames(x) <- LETTERS[1:3]
#' SelectFromTable(x, row.selection.mode = "vector", row.selections = c("B", "C"))
#' SelectFromTable(x, row.selection.mode = "Last rows", row.selections = 2)
#'
#' SelectFromTable(t(1:6), column.selection.mode = "range",
#'                 column.selections = "2-4,6")
#' @export
SelectFromTable <- function(table,
                            row.selection.mode = c("vector",
                                                   "first rows",
                                                   "last rows",
                                                   "range",
                                                   "first date-time periods",
                                                   "last date-time periods",
                                                   "date range"),
                            row.selections = NULL,
                            column.selection.mode = c("vector",
                                                      "first columns",
                                                      "last columns",
                                                      "range",
                                                      "first date-time periods",
                                                      "last date-time periods",
                                                      "date range"),
                            column.selections = NULL,
                            unit = NULL,
                            calendar = TRUE,
                            ...)
{
    row.selection.mode <- tolower(row.selection.mode)
    column.selection.mode <- tolower(column.selection.mode)
    row.selection.mode <- match.arg(row.selection.mode)
    column.selection.mode <- match.arg(column.selection.mode)
    if (isMultiStatTable(table) && getDimensionLength(table) > 2L) {
        table.dimnames <- dimnames(table)
        stat.name <- table.dimnames[[length(table.dimnames)]][1L]
        warning("Multiple statistics detected in table, only the first, ",
                sQuote(stat.name), ", will be shown.")
    }
    table.out <- FlattenQTable(table, drop = getDimensionLength(table) > 2L)
    if (!is.null(row.selections))
    {
        row.selection.mode <- tolower(row.selection.mode)
        table.out <- selectFromRows(table.out, row.selection.mode,
                                    row.selections, unit = unit[1],
                                    calendar = calendar[1], ...)
    }


    if (NCOL(table.out) > 1L && !is.null(column.selections))
    {
        if (length(calendar) == 2)
            calendar <- calendar[2]
        if (length(unit) == 2)
            unit <- unit[2]
        table.out <- selectFromColumns(table.out, table.out,
                                       selection.mode = column.selection.mode,
                                       selections = column.selections,
                                       unit = unit, calendar = calendar, ...)

    }


    if (!is.null(attr(table.out, "name")))
    {
        attr(table.out, "name.original") <- attr(table.out, "name")
        attr(table.out, "name") <- NULL
    }
    if (IsQTable(table) && !is.null(attr(table.out, "is.subscripted")))
        attr(table.out, "table.select.subscripted") <- TRUE

    table.out
}

#' @importFrom flipU StopForUserError
selectFromRows <- function(table, selection.mode = "vector",
                           selections = NULL, unit, calendar, ...)
{
    table.out <- table
    if (selection.mode == "range")
    {
        selections <- parseRangeString(selections, NROW(table))
        selections <- checkSelections(selections, table, 1)
    }else if (selection.mode == "date range")
        selections <- findDatesInTable(table, selections, 1)
    else if (selection.mode == "first rows")
        unit <- "Row"

    n.dims <- length(dim(table))

    if (startsWith(selection.mode, "first"))
    {
        table.out <- First(table, keep = selections,
                           unit = unit, calendar = calendar, ...)
        selections <- seq_len(NROW(table.out))
    }else if (startsWith(selection.mode, "last")) {
        table.out <- Last(table, keep = selections,
                          unit = unit, calendar = calendar, ...)
        selections <- (NROW(table) - NROW(table.out) + 1):NROW(table)
    }else
    {
        selections <- checkSelections(selections, table, 1)
        args <- c(list(table), rep(alist(, )[1], max(1L, n.dims)), drop = FALSE)
        args[[2L]] <- selections
        table.out <- do.call(`[`, args)
    }
    if (NROW(table.out) == 0L)
        StopForUserError("No rows selected, output contains no rows.")

    table.out <- copyAttributesIfNotQTable(table.out, table)
    if (!is.null(attr(table, "span")) && !is.null(attr(table, "span")$rows))
        table.out <- updateTableRowSpanAttribute(table.out, table, selections)
    if (is.null(attr(table.out, "statistic")) && !is.null(attr(table, "statistic")))
        attr(table.out, "statistic") <- attr(table, "statistic")
    table.out
}

#' @importFrom flipU StopForUserError
selectFromColumns <- function(table, table.orig, selection.mode = "vector",
                              selections = NULL, unit, calendar, ...)
{
    n.dims <- length(dim(table))
    if (selection.mode == "range") {
        selections <- parseRangeString(selections, NCOL(table))
        selections <- checkSelections(selections, table, 2)
    }else if (selection.mode == "date range")
        selections <- findDatesInTable(table, selections, 2)

    if (selection.mode == "first columns") {
        table.out <- First(table, keep = selections, unit = "Column", ...)
        selections <- seq_len(ncol(table.out))
    }else if (selection.mode == "last columns") {
        table.out <- Last(table, keep = selections, unit = "Column", ...)
        selections <- (ncol(table) - ncol(table.out) + 1):ncol(table)
    }else if (selection.mode == "first date-time periods") {
        table.out <- t(First(t(table),  # t() in case date labels in rows and columns
                             keep = selections, unit = unit,
                             calendar = calendar, ...))
        selections <- seq_len(ncol(table.out))
    }else if (selection.mode == "last date-time periods") {
        table.out <- t(Last(t(table),  # t() in case date labels in rows and columns
                            keep = selections, unit = unit,
                            calendar = calendar, ...))
        selections <- (ncol(table) - ncol(table.out) + 1):ncol(table)
    }else
    {
        selections <- checkSelections(selections, table, 2)
        if (length(dim(table)) >= 2) {
            args <- c(list(table), rep(alist(, )[1L], n.dims), drop = FALSE)
            args[[3L]] <- selections
            table.out <- do.call(`[`, args)
        }
    }
    if (NCOL(table.out) == 0L)
        StopForUserError("No columns selected, output contains no columns.")

    table.out <- copyAttributesIfNotQTable(table.out, table)
    if (hasColSpan(table))
        table.out <- updateTableColSpanAttribute(table.out, table, selections)
    if (is.null(attr(table.out, "statistic")) && !is.null(attr(table, "statistic")))
        attr(table.out, "statistic") <- attr(table, "statistic")
    return(table.out)
}

#' Check indices are valid for selecting from a table
#'
#' Validates a vector of indices to be used to select from a dimension
#' of a table; e.g., removing duplicates and indices out of range for
#' the supplied table with a warning.
#' @param indices A vector to be used to select from a table
#' @param ... Currently, ignored
#' @return A possibly modified copy of \code{indices} with invalid
#'     values removed.
#' @export
checkSelections <- function(indices, ...)
    UseMethod("checkSelections")

#' @importFrom flipU StopForUserError
#' @export
checkSelections.default <- function(indices, ...)
{
    if (!inherits(indices, c("numeric", "factor", "character"))) {
        StopForUserError(
            "Supplied format for selections is not valid. ",
            "Selections should be a integer, character, or logical vector."
        )
    }
    NextMethod("checkSelections")
}

#' @export
#' @rdname checkSelections
#' @param table A table (matrix, data.frame, etc.) to check \code{indices} against.
#' @param dim Integer specficying the dimension of \code{table} to consider.
#' @importFrom flipU StopForUserError
checkSelections.character <- function(indices, table, dim, ...)
{
    INVALID.IDX.MAX.PRINT <- 10
    indices.out <- unique(indices)
    if (length(indices.out) == 0L) throwErrorInvalidSelection(dim)
    dim.str <- ifelse(dim == 1, "row", "column")
    if (length(indices.out) != length(indices))
        warning("Duplicate entries detected in ", dim.str, " selections have ",
                "been ignored.")

    if (is.data.frame(table))
    {
        if (dim == 1)
            names <- rownames(table)
        else
            names <- names(table)
    }else if (!is.null(dimnames(table)))
        names <- dimnames(table)[[dim]]
    else
        names <- names(table)

    bad.idx <- which(!indices.out %in% names)
    if (length(bad.idx))
    {
        if (length(bad.idx) == length(indices.out))
            StopForUserError("No valid selections were found in the ", dim.str,
                             " labels. If supplying selections using a page control, check ",
                             " the item list for the control and edit if necessary.")

        if (length(bad.idx) > INVALID.IDX.MAX.PRINT)
            bad.names <- paste(c(indices.out[bad.idx[seq_len(INVALID.IDX.MAX.PRINT)]],
                             "..."), collapse = ", ")
        else
            bad.names <- paste(indices.out[bad.idx], collapse = ", ")
         warning("The following labels/selections were not found in the ",
                 dim.str, " labels of the table and will be ignored: ", bad.names,
                 ". Please check the item list of the control being used ",
                 "for making selections and edit if necessary.", call. = FALSE)
        indices.out <- indices.out[-bad.idx]
    }
    return(indices.out)
}

#' @importFrom flipU StopForUserError
#' @export
checkSelections.numeric <- function(indices, table, dim, ...)
{
    INVALID.IDX.MAX.PRINT <- 10
    indices.out <- unique(as.integer(indices))
    if (length(indices.out) == 0L) throwErrorInvalidSelection(dim)
    dim.str <- ifelse(dim == 1, "row", "column")
    if (length(indices.out) != length(indices))
        warning("Duplicate entries detected in ", dim.str, " selections have ",
                "been ignored.")

    n <- if (dim == 1) NROW(table) else NCOL(table)
    bad.idx <- which(indices.out < 1 | indices.out > n)
    if (length(bad.idx))
    {
        if (length(bad.idx) == length(indices.out))
            StopForUserError("The supplied numeric selections are not valid for selecting ",
                             "from the ", dim.str, "s of the table. Selections must be ",
                             "positive integers less than or equal to the number of ",
                             dim.str, "s in the table (", n, ").")

        if (length(bad.idx) > INVALID.IDX.MAX.PRINT)
            idx.str <- paste(c(indices.out[bad.idx[seq_len(INVALID.IDX.MAX.PRINT)]],
                             "..."), collapse = ", ")
        else
            idx.str <- paste(indices.out[bad.idx], collapse = ", ")
        warning("Numeric selections need to be positive integers less than",
                " or equal to the number of ", dim.str, "s in the table (",
                n, "). The following selections will be ignored: ", idx.str,
                ".", call. = FALSE)
        indices.out <- indices.out[-bad.idx]
    }
    return(indices.out)
}

#' @importFrom flipU StopForUserError
#' @export
checkSelections.logical <- function(indices, table, dim, ...)
{

    if (dim == 1)
        n <- NROW(table)
    else
        n <- NCOL(table)
    if (length(indices) != n)
    {
        dim.str <- ifelse(dim == 1, "rows", "columns")
        StopForUserError("The supplied logical selections are not valid for selecting ",
                         "from the ", dim.str, " of the table. Selections must be ",
                         "TRUE/FALSE values equal to the number of ",
                         dim.str, " in the table (", n, ").")
    }
    return(indices)
}

#' @importFrom flipU StopForUserError
throwErrorInvalidSelection <- function(dim) {
    dim.name <- if (dim == 1L) "rows" else "columns"
    StopForUserError("No ", dim.name, " selected, output contains no ", dim.name, ".")
}


#' @param range string specifying numeric ranges,
#'     e.g. \code{"1-3, 5-7,12"}
#' @param dim.length integer range specifying the end value to use for
#'     range inputs of the form \code{1-}.
#' #' @return An integer vector of values included in \code{range}.
#' @noRd
#' @importFrom flipU ConvertCommaSeparatedStringToVector StopForUserError
parseRangeString <- function(range, dim.length)
{
    indices <- range |> ConvertCommaSeparatedStringToVector() |>
        sub(pattern = "^ ?- ?([0-9]+)$", replacement = "1-\\1") |>
        sub(pattern = "^([0-9]+) ?- ?$",
            replacement = paste0("\\1-", dim.length)) |>
        strsplit(split = " ?- ?") |>
        lapply(FUN = as.numeric) |>
        lapply(FUN = function(x) if (length(x) == 2 && !anyNA(x))
                                     return(seq(x[1], x[2]))
                                 else return(x)) |>
        unlist()
    if (anyNA(indices))
        StopForUserError("The supplied range of indices could not be parsed. It must ",
                         "contain a comma-separated list of numbers; e.g., '1,3-5,8,10-15'.")
    return(indices)
}

#' @importFrom flipTime AsDate
findDatesInTable <- function(table, date.range, dim = 1)
{
    dates <- strsplit(date.range, "--")[[1L]]
    start.date <- AsDate(dates[1])
    end.date <- AsDate(dates[2])
    if (dim == 1)
        tnames <- rownames(table)
    else
        tnames <- colnames(table)

    names.dates <- AsDateTime(tnames)
    return(which(names.dates >= start.date & names.dates <= end.date))
}
