#' @rdname variabilityOperations
#' @description Also, \code{VarianceEachRow} is an extension of \code{\link{Variance}} for single
#'  inputs where the variance off each row is computed instead of all elements. Similar to the
#'  other functions in verbs, the input can be processed to remove rows or column before calculation
#'  is done.
#' @details For \code{VarianceEachRow} the variance is computed within the row dimension of the input.
#'  E.g. a n x p matrix supplied to \code{VarianceEachRow} will produce a vector of of length \code{n}.
#'  If names are provided in the row dimension of the input then the output will have the same
#'  row names.
#'
#' @return The \code{VarianceEachRow} function returns the variance of all the elements in each row
#'   index provided in the input, possibly after some rows have been removed via the \code{remove.rows}
#'   argument.
#' @examples
#' # Examples using VarianceEachRow
#' input.matrix <- matrix(runif(12), nrow = 6, dimnames = list(letters[1:6], c("Q1", "Q2")))
#' var.row <- VarianceEachRow(input.matrix)
#' sd.row <- StandardDeviationEachRow(input.matrix)
#' all.equal(sqrt(var.row), sd.row)
#' input.matrix.with.total <- cbind(input.matrix, "Total" = rowSums(input.matrix))
#' VarianceEachRow(input.matrix.with.total, remove.columns = "Total")
#' @export
VarianceEachRow <- function(x,
                            remove.missing = TRUE,
                            remove.rows = NULL,
                            remove.columns = c("NET", "SUM", "Total"),
                            warn = FALSE)
{
    varianceRows(x,
                 standard.deviation = FALSE,
                 remove.missing = remove.missing,
                 remove.rows = remove.rows,
                 remove.columns = remove.columns,
                 return.column.counts = FALSE,
                 warn = warn,
                 function.name = sQuote(deparse(sys.call()[[1]])))
}

#' @rdname SumOperations
#' @export
StandardDeviationEachRow <- function(x,
                                     remove.missing = TRUE,
                                     remove.rows = NULL,
                                     remove.columns = c("NET", "SUM", "Total"),
                                     warn = FALSE)
{
    varianceRows(x,
                 standard.deviation = TRUE,
                 remove.missing = remove.missing,
                 remove.rows = remove.rows,
                 remove.columns = remove.columns,
                 return.column.counts = FALSE,
                 warn = warn,
                 function.name = sQuote(deparse(sys.call()[[1]])))
}

#' @rdname SumOperations
#' @export
VarianceRows <- VarianceEachRow

#' @rdname SumOperations
#' @export
StandardDeviationRows <- StandardDeviationEachRow

varianceRows <- function(x,
                         standard.deviation = FALSE,
                         remove.missing = TRUE,
                         remove.rows = NULL,
                         remove.columns = c("NET", "SUM", "Total"),
                         return.column.counts = FALSE,
                         warn = FALSE,
                         function.name)
{
    higher.dim.array <- isQTable(x) && getDimensionLength(x) > 2L
    x <- processArguments(list(x),
                          remove.missing = FALSE, # This is only used to trigger a warning
                          remove.rows = remove.rows, remove.columns = remove.columns,
                          subset = NULL, weights = NULL,
                          check.statistics = !higher.dim.array,
                          return.total.element.weights = "No",
                          warn = warn,
                          function.name = function.name)
    input <- x[[1L]]
    output <- computeVarianceRows(input, remove.missing = remove.missing)
    if (warn)
    {
        if (NCOL(input) == 1L)
            throwWarningAboutVarianceCalculationWithSingleElement(input, dimension = 2L, function.name)
        checkOppositeInifinitiesByRow(output, input, function.name)
        warnIfDataHasMissingValues(x, remove.missing = remove.missing)
    }
    if (standard.deviation)
        sqrt(output)
    output
}

#' @importFrom stats setNames
computeVarianceRows <- function(x, remove.missing)
{
    x.names <- rowNames(x)
    # Higher dimensional arrays that can occur in some Q Tables
    # are handled as a special case here.
    if (isQTable(x) && getDimensionLength(x) > 2)
    {
        y <- apply(x, c(1L, 3L), var, na.rm = remove.missing)
        if (NCOL(y) == 1L)
            y <- setNames(as.vector(y), x.names)
        y
    } else if (NCOL(x) == 1)
        setNames(rep(NA, nrow(x)), nm = x.names)
    else
    {
        setNames(as.vector(apply(x, 1L, var, na.rm = remove.missing)),
                 nm = x.names)
    }
}

throwWarningAboutVarianceCalculationWithSingleElement <- function(input, dimension, function.name)
{
    single.dim.input <- switch(dimension, "row", "column")
    operation.dims <- switch(dimension, "columns", "rows")
    operation <- if (grepl("Variance", function.name)) "variance" else "standard deviation"
    input.type <- if (isVariable(input)) "a single variable" else paste0("an input with a single ", single.dim.input)
    warning("Only ", input.type, " was provided to ", function.name, " but an input with at least two ",
            operation.dims, " with non-missing values are required to calculate ", function.name,
            ". Since only an input with a single ", single.dim.input,
            " has been provided, the calculated output has been set to a ", single.dim.input,
            " of missing values.")
}
