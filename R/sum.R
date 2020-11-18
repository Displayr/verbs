#' General-Purpose Element-wise Summation
#'
#' A replacement for \code{\link{sum}} supporting element-wise
#' summation of many objects in addition to vectors.
#' @param ... Objects to be summed; e.g. Q Tables, arrays
#' @param remove.missing Should missing values be removed in calculations? Defaults to \code{TRUE},
#' @param remove.rows The categories of named vectors, or, rows of tables and similar
#' structures to be removed.
#' @param remove.columns The labels of categories to be removed from the calculation
#' where the table is two-dimensional,
#' @param warn Warnings are shown when non-obvious operations are performed (e.g.,
#' removing rows).
#' @param subset Logical vector of units to use in the calculation. Only applied to variables and
#' not to QTables that contain statistics since the original variable data is unavailable.
#' @param weights Numeric vector of weights to use in the calculation. Only applies to variables
#' and not to QTables that contain statistics since the original variable data is unavailable.
#' @return The sum of the provided inputs
#' @export
#' @examples
#' Sum(1:3, 2:4)
Sum <- function(...,
                remove.missing = TRUE,
                remove.rows = c("NET", "SUM", "Total"),
                remove.columns = c("NET", "SUM", "Total"),
                subset = NULL,
                weights = NULL,
                warn = FALSE)
{
    function.name <- sQuote(match.call()[[1]], q = FALSE)
    x <- list(...)
    x <- processArguments(...,
                          remove.missing = remove.missing,
                          function.name = function.name,
                          remove.rows = remove.rows,
                          remove.columns = remove.columns,
                          subset = subset,
                          weights = weights,
                          warn = warn)
    checkRowOrColDimensionsEqual(x, function.name = function.name)

    sum.function <- if (remove.missing) sumWithNAsRemoved else sum
    sum.output <- sumElements(x, sum.function)
    if (warn && is.nan(sum.output))
        checkForOppositeInfinites(x, function.name = function.name)
    sum.output
}

# extra arguments cannot be specified in Reduce
sumWithNAsRemoved <- function(...)
    sum(..., na.rm = TRUE)

sumElements <- function(x, sum.function)
{
    if (length(x) >= 2)
        Reduce(sum.function, x)
    else
        sum.function(x[[1]])
}



