#' @rdname variabilityOperations
#' @description Also, \code{VarianceEachRow} is an extension of \code{\link{Variance}} for single
#'  inputs where the variance of each row is computed instead of all elements. Similar to the
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
                            sample = TRUE,
                            remove.missing = TRUE,
                            remove.rows = NULL,
                            remove.columns = c("NET", "SUM", "Total"),
                            warn = FALSE)
{
    fun.name <- deparse(sys.call()[[1]])
    fun.call <- match.call()
    fun.call[[1L]] <- varianceRows
    fun.call[["function.name"]] <- sQuote(fun.name)
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    eval.fun(fun.call, parent.frame())
}

#' @rdname variabilityOperations
#' @export
StandardDeviationEachRow <- function(x,
                                     sample = TRUE,
                                     remove.missing = TRUE,
                                     remove.rows = NULL,
                                     remove.columns = c("NET", "SUM", "Total"),
                                     warn = FALSE)
{
    fun.name <- deparse(sys.call()[[1]])
    fun.call <- match.call()
    fun.call[[1L]] <- varianceRows
    fun.call[["function.name"]] <- sQuote(fun.name)
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    sqrt(eval.fun(fun.call, parent.frame()))
}

#' @rdname variabilityOperations
#' @export
VarianceRows <- VarianceEachRow

#' @rdname variabilityOperations
#' @export
StandardDeviationRows <- StandardDeviationEachRow

varianceRows <- function(x,
                         sample = TRUE,
                         remove.missing = TRUE,
                         remove.rows = NULL,
                         remove.columns = c("NET", "SUM", "Total"),
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
    output <- computeVarianceRows(input, remove.missing = remove.missing, sample = sample)
    if (warn)
    {
        min.n.required <- 1L + sample
        if (NCOL(input) == 1L && sample)
            warnSampleVarCalcWithSingleVal(input, dimension = 1L, function.name)
        else if (remove.missing && any(countNonMissingValues(input, dimension = 1L) < min.n.required))
            throwWarningAboutTooManyMissingInDimIfNecessary(input, dimension = 1L, sample, function.name)
        checkOppositeInifinitiesByRow(output, input, function.name)
    }
    output
}

#' @importFrom stats setNames
computeVarianceRows <- function(x, remove.missing, sample)
{
    fun <- if (sample) var else pvar
    x.names <- rowNames(x)
    # Higher dimensional arrays that can occur in some Q Tables
    # are handled as a special case here.
    if (isQTable(x) && getDimensionLength(x) > 2L)
    {
        y <- apply(x, c(1L, 3L), fun, na.rm = remove.missing)
        if (NCOL(y) == 1L)
            y <- setNames(as.vector(y), x.names)
        y
    } else if (NCOL(x) == 1) {
        output <- if (sample) rep(NA_real_, NROW(x)) else c(0, NA_real_)[is.na(x) + 1L]
        setNames(output, nm = x.names)
    } else
        setNames(as.vector(apply(x, 1L, fun, na.rm = remove.missing)),
                 nm = x.names)
}

throwWarningAboutTooManyMissingInDimIfNecessary <- function(input, dimension, sample, function.name)
{
    min.n.required <- 1L + sample
    input.dim.length <- getDimensionLength(input)
    dims.to.apply <- if (input.dim.length == 3L) c(dimension, 3L) else dimension
    if (input.dim.length == 1L)
        throw.warning <- sum(!is.na(input)) < min.n.required
    else
        throw.warning <- any(apply(!is.na(input), dims.to.apply, sum) < min.n.required)
    if (throw.warning)
        throwWarningAboutDimWithTooManyMissing(dimension, sample, function.name = function.name)
}

warnSampleVarCalcWithSingleVal <- function(input, dimension, function.name)
{
    dims <- c("row", "column")
    single.dim.input <- dims[dimension]
    operation.dim <- dims[-dimension]
    operation <- if (grepl("Variance", function.name)) "variance" else "standard deviation"
    input.type <- if (isVariable(input)) "a single variable" else paste0("an input with a single ", single.dim.input)
    operation <- if (grepl("Variance", function.name)) "variance" else "standard deviation"
    warning("Only ", input.type, " was provided to ", function.name, " but an input with at least two ",
            operation.dim, "s with non-missing values are required to calculate the sample ", operation,
            ". Since only an input with a single ", operation.dim,
            " has been provided, the calculated output has been set to missing values.")
}

throwWarningAboutDimWithTooManyMissing <- function(dimension, sample, function.name)
{
    missing.msg<- if (sample) "less than two non-missing values" else "all missing values"
    operation.dim <- c("row", "column")[dimension]
    non.missing.msg <- if (sample) "two non-missing values are" else "one non-missing value is"
    operator <- if (grepl('Variance', function.name)) "variance" else "standard deviation"
    calculation <- paste(if (sample) "sample" else "population", operator)
    warning("Some of the ", operation.dim, "s in the input to ", function.name, " have ", missing.msg,
            ". However at least ", non.missing.msg, " required ",
            "to calculate the ", calculation, " along each ", operation.dim, ". ",
            "In those situations the calculated output has been set to missing values.")
}
