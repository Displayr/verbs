#' @rdname AverageOperations
#' @inheritParams SumColumns
#' @return In a similar way, \code{AverageColumns} is a generalization of \code{\link{colMeans}} supporting
#'  row removal and the application of filters and weights before calculation but not supporting row or column matching for multiple inputs.
#' @examples
#' # Examples using SumColumns
#' input.matrix <- matrix(1:8, nrow = 4)
#' AverageColumns(input.matrix) #= c(10, 26)
#' named.matrix <- input.matrix
#' dimnames(named.matrix) <- list(LETTERS[1:4], letters[1:2])
#' AverageColumns(named.matrix)
#' AverageColumns(named.matrix, remove.rows = c("A", "C"))
#' AverageColumns(named.matrix, subset = c(TRUE, FALSE, TRUE, FALSE))
#' AverageColumns(named.matrix, remove.rows = c("B", "D"))
#' @export
AverageColumns <- function(...,
                           remove.missing = TRUE,
                           remove.rows = c("NET", "SUM", "Total"),
                           subset = NULL,
                           weights = NULL,
                           warn = FALSE)
{
    called.args <- match.call(expand.dots = FALSE)
    function.args <- formals()
    called.args[[1L]] <- as.name('list')
    called.args[["..."]] <- function.args[["..."]] <- NULL
    called.args <- eval.parent(called.args)
    matched.args <- match(names(called.args), names(function.args), nomatch = 0L)
    if (length(matched.args))
        function.args[matched.args] <- called.args[]
    inputs <- list(...)
    inputs <- Filter(Negate(is.null), inputs)
    if (identical(inputs, list()))
        return(NaN)
    return.total.element.weights <- if (weightsRequired(weights)) "ByColumn" else "Yes"
    new.arguments <- c(inputs, function.args,
                       return.total.element.weights = return.total.element.weights,
                       function.name = sQuote("AverageColumns"))
    computed.sum <- do.call(sumColumns, new.arguments)
    n.sum <- attr(computed.sum, "n.sum")
    attr(computed.sum, "n.sum") <- NULL
    computed.sum / n.sum
}
