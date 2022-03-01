#' @rdname AverageOperations
#' @inheritParams SumEachRow
#' @description Also, \code{AverageEachRow} is a generalization of \code{\link{rowMeans}} supporting
#'  column removal before calculation but not supporting filters, weights and row or column matching
#'  for multiple inputs.
#' @details For \code{AverageEachRow} the average is computed within the row dimension of the input.
#'  E.g. a n x p matrix supplied to \code{Average} will produce a vector of of length \code{n}.
#'  If names are provided in the row dimension of the input then the output will have the same
#'  row names.
#'
#' @return The \code{AverageEachRow} function returns the arithmetic mean of all the elements in each row
#'   index provided in the input, possibly after the elements have been pre-processed similar
#'   to \code{Average}.
#' @examples
#' # Examples using AverageEachRow
#' input.matrix <- matrix(runif(6), nrow = 3, dimnames = list(letters[1:3], c("Q1", "Q2")))
#' AverageEachRow(input.matrix)
#' input.matrix.with.total <- cbind(input.matrix, "Total" = rowSums(input.matrix))
#' AverageEachRow(input.matrix.with.total) # The total column is removed by default
#' colnames(input.matrix.with.total) <- c("Q1", "Q2", "tot")
#' # This will include the total in the calculation since the non-standard Total label is used.
#' AverageEachRow(input.matrix.with.total)
#' AverageEachRow(input.matrix.with.total, remove.columns = "tot")
#' @export
AverageEachRow <- function(...,
                           remove.missing = TRUE,
                           remove.rows = NULL,
                           remove.columns = c("NET", "SUM", "Total"),
                           warn = FALSE)
{
    if (identical(Filter(Negate(is.null), list(...)), list()))
        return(NaN)
    fun.call <- match.call()
    fun.call[[1L]] <- sumRowsInputs
    fun.call[["return.column.counts"]] <- TRUE
    fun.call[["function.name"]] <- sQuote("AverageEachRow")
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    computed.sum <- eval.fun(fun.call, parent.frame())
    n.sum <- attr(computed.sum, "n.sum")
    attr(computed.sum, "n.sum") <- NULL
    computed.sum / n.sum
}

#' @rdname AverageOperations
#' @export
AverageRows <- AverageEachRow
