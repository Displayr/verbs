#' @rdname SumOperations
#' @title General-Purpose Summation functions
#'
#' A replacement for \code{\link{sum}} supporting element-wise
#' summation of many objects in addition to vectors.
#' @param ... Objects to be summed; e.g. Q Tables, arrays, vectors, Variables or Variable Sets.
#'   Depending on if \code{Sum}, \code{SumRows} or \code{SumColumns} is used, different possible
#'   multiple input types are allowed. See the
#' @param remove.missing Logical element controlling whether missing values should be
#'   removed in calculations? Defaults to \code{TRUE}. If set to \code{FALSE} and one of
#'   input elements contains missing values, then the resulting computed sum will also be
#'   \code{NA}.
#' @param remove.rows The labels of named vectors, or, rows of tables and similar
#'   structures to be removed from the row dimension of the input. Any row elements
#'   with the labels specified here will not be included in the resulting sum calculation.
#' @param remove.columns The labels of columns of tables and similar
#'   structures to be removed from the column dimension of the input. Any column elements
#'   with the labels specified here will not be included in the resulting sum calculation.
#' @param match.elements String element specifying if inputs with named row elements
#'   (or named elements for a single vector) should be matched when used in \code{SumRows}.
#' @param warn Logical element to control whther warnings are shown when non-obvious
#'   operations are performed (e.g., removing rows, removing missing values when they are present).
#' @param subset Logical vector of units to use in the calculation. Only applied to variables and
#'   not to QTables that contain statistics since the original variable data is unavailable.
#' @param weights Numeric vector of weights to use in the calculation. Only applies to variables
#'   and not to QTables that contain statistics since the original variable data is unavailable.
#' @details TBA
#' @return The sum or sum of the rows or sum of the columns of the provided inputs respectively
#'   depending on the function used.
#' @export
#' @examples
#' # Examples using Sum
#' Sum(1:3, 2:4)
#' all.equal(Sum(1:3, 2:4), sum(1:3, 2:4))
#' x <- 1:4
#' names(x) <- c(LETTERS[1:3], "SUM")
#' Sum(x)
#' x <- 1:9
#' desired.subset = rep(c(TRUE, FALSE), c(3, 6))
#' Sum(x, subset = desired.subset)
#' desired.weights <- runif(9)
#' y <- 10:18
#' Sum(x, y, weights = desired.weights)
#' sum(x * desired.weights, y * desired.weights)
#' basic.table <- matrix(1:12, nrow = 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' table.with.marginals <- rbind(basic.table, SumColumns(basic.table))
#' table.with.marginals <- cbind(table.with.marginals, c(SumRows(basic.table), Sum(basic.table)))
#' dimnames(table.with.marginals) <- list(c(letters[1:4], "SUM"), c(LETTERS[1:3], "SUM"))
#' all.equal(Sum(table.with.marginals), sum(basic.table))
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
    {
        opposite.infinities <- vapply(x, checkForOppositeInfinites, logical(1))
        if (any(opposite.infinities))
            warning(function.name,
                    " cannot be computed as the data contains both Inf and -Inf.")
    }
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
