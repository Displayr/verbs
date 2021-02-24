#' @rdname AverageOperations
#' @inheritParams SumRows
#' @description In a similar way, \code{AverageRows} is a generalization of \code{\link{rowMeans}} but
#'  not supporting row or column matching for multiple inputs that is supported in \code{Average}.
#' @details If a single input is provided to \code{AverageRows} and \code{AverageColumns}, it is
#'  permissible to be a \code{numeric} vector, \code{data.frame}, \code{Q Table}, \code{matrix} or
#'  other possible structure that has well defined rows or columns. An array is only
#'  permissible if it has 2 dimensions. Higher order arrays are only allowed in the form of
#'  a \code{Q Table}. Multiple inputs are allowed but only if each input is a single \code{numeric}
#'  vector with the same number of rows (a vector with n elements is interpreted as a matrix with
#'  n rows and 1 column) or the input elements can be reduced to that situation. For example,
#'  an n x p \code{matrix} or \code{data.frame} can be converted to p separate vectors with n rows.
#'
#'  For \code{AverageRows} the sum is computed not element-wise but across the whole row dimension
#'  E.g. a n x p matrix supplied to \code{AverageRows} will produce a vector or column vector of
#'  of length \code{n}. If names are provided in the row dimension of the input then the output will have the same
#'  row names.
#'
#' @return The \code{AverageRows} function returns the arithmetric mean of all the elements in each row
#'   index provided in the input, possibly after the elements have been pre-processed similar
#'   to \code{Average}.
#' @examples
#' # Examples using AverageRows
#' input.matrix <- matrix(runif(6), nrow = 3, dimnames = list(letters[1:3], c("Q1", "Q2")))
#' AverageRows(input.matrix)
#' input.matrix.with.total <- cbind(input.matrix, "Total" = rowSums(input.matrix))
#' AverageRows(input.matrix.with.total) # The total column is removed by default
#' colnames(input.matrix.with.total) <- c("Q1", "Q2", "tot")
#' # This will include the total in the calculation since the non-standard Total label is used.
#' AverageRows(input.matrix.with.total)
#' AverageRows(input.matrix.with.total, remove.columns = "tot")
#' v3 <- matrix(runif(3), nrow = 3, dimnames = list(letters[1:3], "Q3"))
#' AverageRows(input.matrix, v3)
#' input.df <- data.frame(V1 = runif(3), V2 = runif(3))
#' AverageRows(input.matrix, input.df)
#' @export
AverageRows <- function(...,
                        remove.missing = TRUE,
                        remove.columns = c("NET", "SUM", "Total"),
                        warn = FALSE)
{
    called.args <- match.call(expand.dots = FALSE)
    function.args <- formals()
    called.args[[1L]] <- as.name('list')
    called.args[["..."]] <- function.args[["..."]] <- NULL
    called.args <- eval(called.args, parent.frame())
    matched.args <- match(names(called.args), names(function.args), nomatch = 0L)
    if (length(matched.args))
        function.args[matched.args] <- called.args
    inputs <- list(...)
    inputs <- Filter(Negate(is.null), inputs)
    if (identical(inputs, list()))
        return(NaN)
    attr(inputs[[1L]], "called.from.average") <- "AverageRows"
    new.arguments <- c(inputs, function.args)
    computed.sum <- do.call("SumRows", new.arguments)
    n.sum <- attr(computed.sum, "n.sum")
    attr(computed.sum, "n.sum") <- NULL
    computed.sum / n.sum
}
