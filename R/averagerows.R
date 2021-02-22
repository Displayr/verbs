#' @rdname AverageOperations
#' @inheritParams SumRows
#' @description In a similar way, \code{AverageRows} is a generalization of \code{\link{rowSums}} but
#'  not supporting row or column matching for multiple inputs.
#' @details If a single input is provided to \code{AverageRows} and \code{AverageColumns}, it is
#'  permissible to be a \code{numeric} vector, \code{data.frame}, \code{Q Table}, \code{matrix} or
#'  other possible structure that has well defined rows or columns. An array is only
#'  permissible if it has 2 dimensions. Higher order arrays are only allowed in the form of
#'  a \code{Q Table}. Multiple inputs are allowed but only if each input is a single \code{numeric}
#'  vector with the same number of rows (a vector with n elements is interpreted as a matrix with
#'  n rows and 1 column) or the input elements can be reduced to that situation. For example,
#'  an n x p \code{matrix} or \code{data.frame} can be converted to p separate vectors with n rows.
#'
#'  For \code{SumRows} the sum is computed not element-wise but across the whole row dimension
#'  E.g. a n x p matrix supplied to \code{SumRows} will produce a vector or column vector of
#'  of length \code{n}. If names are provided in the row dimension of the input then the output will have the same
#'  row names.
#'
#' @return The \code{SumRows} function returns the summation of all the elements in each row
#'   index provided in the input, possibly after the elements have been pre-processed similar
#'   to \code{Sum}. However, \code{SumRows} also allows elements to be matched by name.
#' @examples
#' # Examples using AverageRows
#' @export
AverageRows <- function(...,
                        remove.missing = TRUE,
                        remove.columns = c("NET", "SUM", "Total"),
                        warn = FALSE)
{
    called.args <- match.call(expand.dots = FALSE)
    function.args <- formals()
    called.args[["..."]] <- function.args[["..."]] <- NULL
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
