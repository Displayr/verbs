#' General-Purpose Element-wise Summation
#'
#' A replacement for \code{\link{sum}} supporting element-wise
#' summation of many objects in addition to vectors.
#' @param ... Objects to be summed; e.g. Q Tables, arrays
#' @param remove.missing Should missing values be removed in calculations? Defaults to \code{TRUE},
#' @param remove.rows The categories of named vectors, or, rows of tables and similar
#' structures to be removed.
#' @param warn Warnings are shown when non-obvious operations are performed (e.g.,
#' removing rows).
#' @param remove.columns The labels of categories to be removed from the calculation.
#' Where the table is two-dimensional,
#
#' differing from \code{na.rm} in \code{\link{sum}}.
#' @return The sum.
#' @export
#' @examples
#' Sum(1:3, 2:4)
Sum <- function(...,
                remove.missing = TRUE,
                remove.rows = c("NET", "SUM", "Total"),
                remove.columns = c("NET", "SUM", "Total"),
                warn = FALSE)
{
    # I've just hacked this together to check some basic tests. Don't assume there's any
    # genius in the structure. For example, I've just pulled out x and y in a hacky way
    # but expect it to be rewritten with recursion and/or or a loop.

    checkTypes(..., function.name = "Sum")
    lst <- list(...)
    n <- length(lst)
    x <- lst[[1]]
    y <- if (length(lst) == 1) NULL else lst[[2]]
    if (n == 1)
        return(sumElement(x, remove.missing, remove.rows, remove.columns, warn))
    sum(sumElement(x, remove.missing, remove.rows, remove.columns, warn),
        sumElement(y, remove.missing, remove.rows, remove.columns, warn),
        na.rm = remove.missing)
}


sumElement <- function(x, remove.missing, remove.rows, remove.columns, warn)
{
    # This needs to be a recursion itself. E.g., to deal with json-like lists. I
    #        I'm not suggesting that we need to deal with json-like lists, but rather that
    #        this thinking of them is a good way to create a nice efficient general recursion
    x <- removeRows(x, remove.rows, warn)
    checkDifferentStatistics(x, function.name = "Sum", warn = warn)
    checkForOppositeInfinites(x, warn)
    # Need to generalize to remove columns as well
    sum(x,
        na.rm = remove.missing)

}
