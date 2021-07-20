#' @rdname variabilityOperations
#' @param x A single input to be used in \code{VarianceEachRow} or \code{VarianceEachColumn}.
#' @description In a similar way, \code{VarianceEachColumn} is an extension of \code{\link{Variance}} for
#'  single inputs and also supporting the application of filters and weights before calculation
#'  but not supporting multiple inputs.
#'  The functions \code{VarianceColumns} and \code{VarianceRows} are aliases for
#'  \code{VarianceEachColumn} and \code{VarianceEachRow}.
#' @details The \code{VarianceEachRow} and \code{VarianceEachColumn} only support a single input \code{x}.
#'  The permissible input is a \code{numeric} vector, \code{array} with at most 2 dimensions, a \code{data.frame},
#'  Q Table, \code{matrix} or other possible structure that has well defined rows or columns.
#'
#'  In the case of a vector of length n, it is interpreted to be an input with n rows and
#'  a single column. An array is only permissible if it has 2 dimensions, the exception being
#'  higher order arrays are only allowed in the form of a Q Table.
#'
#'  For \code{VarianceEachColumn}, the column names of the inputs if provided are used to define the names
#'  in the output vector.
#' @return The \code{VarianceEachColumn} function returns the variance of all the elements in each column
#'   index provided in the input, possibly after the elements have been filtered, weights applied or rows
#'   removed via the provided calling arguments.
#' @examples
#' # Examples using VarianceEachColumn
#' input.matrix <- matrix(1:8, nrow = 4)
#' wgts <- runif(4)
#' VarianceEachColumn(input.matrix)
#' named.matrix <- input.matrix
#' dimnames(named.matrix) <- list(LETTERS[1:4], letters[1:2])
#' VarianceEachColumn(named.matrix)
#' VarianceEachColumn(named.matrix, weights = wgts)
#' VarianceEachColumn(named.matrix, remove.rows = c("A", "C"))
#' VarianceEachColumn(named.matrix, subset = c(TRUE, FALSE, TRUE, FALSE))
#' VarianceEachColumn(named.matrix, subset = c(TRUE, FALSE, TRUE, FALSE), weights = wgts)
#' VarianceEachColumn(named.matrix, remove.rows = c("B", "D"))
#' @export
VarianceEachColumn <- function(x,
                               remove.missing = TRUE,
                               remove.rows = c("NET", "SUM", "Total"),
                               remove.columns = NULL,
                               subset = NULL, weights = NULL,
                               warn = FALSE)
{
    varianceColumns(x,
                    remove.missing = remove.missing,
                    remove.rows = remove.rows,
                    remove.columns = remove.columns,
                    subset = subset, weights = weights,
                    return.total.element.weights = if (weightsRequired(weights)) "ByColumn" else "No",
                    warn = warn,
                    function.name = sQuote(deparse(sys.call()[[1]])))
}

#' @rdname variabilityOperations
#' @export
StandardDeviationEachColumn <- function(x,
                                        remove.missing = TRUE,
                                        remove.rows = c("NET", "SUM", "Total"),
                                        remove.columns = NULL,
                                        subset = NULL, weights = NULL,
                                        warn = FALSE)
{
    sqrt(varianceColumns(x,
                         remove.missing = remove.missing,
                         remove.rows = remove.rows, remove.columns = remove.columns,
                         subset = subset, weights = weights,
                         return.total.element.weights = if (weightsRequired(weights)) "ByColumn" else "No",
                         warn = warn,
                         function.name = sQuote(deparse(sys.call()[[1]]))))
}

#' @rdname variabilityOperations
#' @export
VarianceColumns <- VarianceEachColumn

#' @rdname variabilityOperations
#' @export
StandardDeviationColumns <- StandardDeviationEachColumn


varianceColumns <- function(x,
                            remove.missing = TRUE,
                            remove.rows = c("NET", "SUM", "Total"),
                            remove.columns = NULL,
                            subset = NULL, weights = NULL,
                            return.total.element.weights = "No",
                            warn = FALSE,
                            function.name)
{
    x <- processArguments(list(x),
                          remove.missing = remove.missing,
                          remove.rows = remove.rows, remove.columns = remove.columns,
                          subset = NULL, weights = NULL,
                          check.statistics = FALSE,
                          warn = warn,
                          function.name = function.name)
    input <- subsetAndWeightInputsIfNecessary(x,
                                              subset = subset, weights = weights,
                                              return.total.element.weights = return.total.element.weights,
                                              warn = warn,
                                              function.name = function.name)
    input <- coerceToVectorTo1dArrayIfNecessary(input)[[1L]]
    output <- varianceCols(input, weights = if (isQTable(input)) NULL else weights,
                           remove.missing = remove.missing)
    if (warn)
    {
        if (NROW(input) == 1L)
            throwWarningAboutVarianceCalculationWithSingleElement(input, dimension = 2L, function.name)
        else if (remove.missing)
            throwWarningAboutTooManyMissingInDimIfNecessary(input, dimension = 2L, function.name)
        checkOppositeInifinitiesByColumn(output, input, function.name)
    }
    output
}

#' @importFrom stats setNames
#' @importFrom flipU DIM
#' @noRd
varianceCols <- function(x, weights, remove.missing = TRUE)
{
    stopifnot(is.array(x) || is.data.frame(x))
    sum.w <- attr(x, "sum.weights")
    not.weighted <- is.null(sum.w)
    if (is.data.frame(x))
    {
        if (not.weighted)
            y <- vapply(x, computeVariance, numeric(1L),
                        weights = NULL, remove.missing = remove.missing)
        else
            y <- mapply(computeVariance, x, sum.w,
                        MoreArgs = list(weights = weights, remove.missing = remove.missing))
    }
    else # is an array
    {
        x.dims <- DIM(x)
        d.length <- length(x.dims)
        x.dimnames <- dimnames(x)
        if (d.length == 1L) # If a 1d array, the input is considered to have n rows and 1 column.
            y <- computeVariance(x, sum.w, weights = weights, remove.missing = remove.missing)
        else if (d.length == 2L)
        { # If a 2d array, the computation is applied to each column but weights need to be accounted for.
            names.exist <- !is.null(x.dimnames[[2L]])
            factor.to.split <- factor(rep(1:x.dims[2L], each = x.dims[1L]),
                                      labels = if (names.exist) x.dimnames[[2L]] else 1:x.dims[2L])
            split.x <- split(x, factor.to.split)
            if (not.weighted || length(sum.w) == 1L) # No weights provided or weighted for a single column
                y <- vapply(split.x,
                            computeVariance, numeric(1L),
                            sum.weights = sum.w, weights = weights, remove.missing = remove.missing,
                            USE.NAMES = names.exist)
            else # Weights provided and they are to be applied on each column.
                y <- mapply(computeVariance, split.x, sum.w,
                            MoreArgs = list(weights = weights, remove.missing = remove.missing),
                            USE.NAMES = names.exist)
        }
        else
        { # The input must be a 3d array (multi-stat Q Table), requiring a different split regime.
            X <- split(x, rep(1:prod(x.dims[-1L]), each = x.dims[1L]))
            y <- array(vapply(X, computeVariance, numeric(1L),
                              sum.weights = sum.w, weights = weights, remove.missing = remove.missing),
                       dim = x.dims[-1L],
                       dimnames = dimnames(x)[-1L])
        }
    }
    if (NCOL(x) == 1L && (isVariable(x) || isQTable(x)))
            y <- setNames(y, getInputNames(x))
    else if (NCOL(x) > 1L)
    {
        if (is.data.frame(x) && any(variables.inside <- vapply(x, isVariable, logical(1L))))
            names(y)[variables.inside] <- vapply(x[variables.inside],
                                                 getInputNames,
                                                 character(1L))
    }
    y
}
