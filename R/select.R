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
#' \item \code{"vector"} - The default mode, use to specify integer or
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
#' vector providing the rows of \code{table} to include in the output or a
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
SelectFromTable <- function(
                             table,
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

    table.out <- FlattenTableAndDropStatisticsIfNecessary(table)
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
    return(table.out)
}

selectFromRows <- function(table, selection.mode = "vector",
                           selections = NULL, unit, calendar, ...)
{
    table.out <- table
    if(selection.mode == "range")
        selections <- parseRangeString(selections)
    else if (selection.mode == "date range")
        selections <- findDatesInTable(table, selections, 1)
    else if (selection.mode == "first rows")
        unit <- "Row"

    n.dims <- length(dim(table))

    if (grepl("^first", selection.mode))
    {
        table.out <- First(table, keep = selections,
                           unit = unit, calendar = calendar, ...)
        selections <- seq_len(nrow(table.out))
    }else if (grepl("^last", selection.mode)){
        table.out <- Last(table, keep = selections,
                          unit = unit, calendar = calendar, ...)
        selections <- (nrow(table) - nrow(table.out) + 1):nrow(table)
    }else
    {
        if (is.character(selections))
            selections <- checkSuppliedIndices(table, selections, 1)

        if (n.dims == 1){
            table.out <- table[selections, drop = FALSE]
        }else if (n.dims == 2){
            table.out <- table[selections, , drop = FALSE]
        }else if (n.dims == 3){
            table.out <- table[selections, , , drop = FALSE]
        }

    }
    if (NROW(table.out) == 0L)
        stop("No rows selected, output contains no rows.")

    table.out <- flipU::CopyAttributes(table.out, table)
    if (!is.null(attr(table, "span")) && !is.null(attr(table, "span")$rows))
        table.out <- updateTableRowSpanAttribute(table.out, table, selections)
    return(table.out)
}

selectFromColumns <- function(table, table.orig, selection.mode = "vector",
                           selections = NULL, unit, calendar, ...)
{
    n.dims <- length(dim(table))
    if(selection.mode == "range")
        selections <- parseRangeString(selections)
    else if (selection.mode == "date range")
        selections <- findDatesInTable(table, selections, 2)

    if (selection.mode == "first columns"){
        table.out <- First(table, keep = selections, unit = "Column", ...)
        selections <- seq_len(ncol(table.out))
    }else if (selection.mode == "last columns"){
        table.out <- Last(table, keep = selections, unit = "Column", ...)
        selections <- (ncol(table) - ncol(table.out) + 1):ncol(table)
    }else if (selection.mode == "first date-time periods"){
        table.out <- t(First(t(table),  # t() in case date labels in rows and columns
                             keep = selections, unit = unit,
                             calendar = calendar, ...))
        selections <- seq_len(ncol(table.out))
    }else if (selection.mode == "last date-time periods"){
        table.out <- t(Last(t(table),  # t() in case date labels in rows and columns
                             keep = selections, unit = unit,
                            calendar = calendar, ...))
        selections <- (ncol(table) - ncol(table.out) + 1):ncol(table)
    }else
    {
        if (is.character(selections))
            selections <- checkSuppliedIndices(table, selections, 2)
        if (length(dim(table)) == 2){
            table.out <- table[, selections, drop = FALSE]
        }else if (length(dim(table)) == 3)
            table.out <- table[, selections, , drop = FALSE]
    }
    if (NCOL(table.out) == 0L)
        stop("No columns selected, output contains no columns.")

    table.out <- flipU::CopyAttributes(table.out, table)
    if (hasColSpan(table))
         table.out <- updateTableColSpanAttribute(table.out, table, selections)
    return(table.out)
}

checkSuppliedIndices <- function(table, indices, dim)
{
    if (is.data.frame(table))
    {
        if (dim == 1)
            names <- names(table)
        else
            names <- rownames(table)
    }else
        names <- dimnames(table)[[dim]]

    bad.idx <- which(!indices %in% names)
    if (length(bad.idx))
    {
        dim.str <- ifelse(dim == 1, "row", "column")
        if (length(bad.idx) == length(names))
            stop("No selections found in the ", dim.str,
                 " labels. If using a page control, check ",
                 " the item list for the control and edit if necessary.")

         bad.names <- paste(indices[bad.idx], collapse = ", ")
         warning("The following labels/selections were not found in the ",
                 dim.str, " labels of the table and will be ignored: ", bad.names,
                 ". Please check the item list of the control being used ",
                 "for making selections and edit if necessary.")
        return(indices[-bad.idx])
    }
    return(indices)
}

#' @param range string specifying numeric ranges, e.g. \code{"1-3, 5-7,12"}
#' @return An integer vector of values included in \code{range}.
#' @noRd
#' @importFrom flipU ConvertCommaSeparatedStringToVector
#' @importFrom magrittr %>%
parseRangeString <- function(range)
{
    indices <- range %>% ConvertCommaSeparatedStringToVector %>%
        strsplit(split = " ?- ?") %>%
        lapply(FUN = as.numeric) %>%
        lapply(FUN = function(x) if (length(x) == 2 && !anyNA(x))
                                     return(seq(x[1], x[2]))
                                 else return(x)) %>%
        unlist
    if (anyNA(indices))
        stop("The supplied range of indices could not be parsed. It must ",
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
