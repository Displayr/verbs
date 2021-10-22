#' @rdname AverageOperations
#' @inheritParams SumColumns
#' @return In a similar way, \code{AverageEachColumn} is a generalization of \code{\link{colMeans}} supporting
#'  row removal and the application of filters and weights before calculation but not supporting row or column matching for multiple inputs.
#' @examples
#' # Examples using SumColumns
#' input.matrix <- matrix(1:8, nrow = 4)
#' AverageEachColumn(input.matrix) #= c(10, 26)
#' named.matrix <- input.matrix
#' dimnames(named.matrix) <- list(LETTERS[1:4], letters[1:2])
#' AverageEachColumn(named.matrix)
#' AverageEachColumn(named.matrix, remove.rows = c("A", "C"))
#' AverageEachColumn(named.matrix, subset = c(TRUE, FALSE, TRUE, FALSE))
#' AverageEachColumn(named.matrix, remove.rows = c("B", "D"))
#' @export
AverageEachColumn <- function(...,
                           remove.missing = TRUE,
                           remove.rows = c("NET", "SUM", "Total"),
                           remove.columns = NULL,
                           subset = NULL,
                           weights = NULL,
                           warn = FALSE)
{
    if (identical(Filter(Negate(is.null), list(...)), list()))
        return(NaN)
    fun.call <- match.call()
    fun.call[[1L]] <- sumColumns
    return.total.element.weights <- if (weightsRequired(weights)) "ByColumn" else "Yes"
    fun.call[["return.total.element.weights"]] <- return.total.element.weights
    fun.call[["function.name"]] <- sQuote("AverageEachColumn")
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    computed.sum <- eval.fun(fun.call, parent.frame())
    n.sum <- attr(computed.sum, "n.sum")
    attr(computed.sum, "n.sum") <- NULL
    computed.sum / n.sum
}

#' @rdname AverageOperations
#' @export
AverageColumns <- AverageEachColumn
