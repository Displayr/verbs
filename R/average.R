#' @rdname AverageOperations
#' @title General-Purpose Averaging functions
#' @inheritParams Sum
#' @param ... Objects to be averaged; e.g. vectors, matrices, Variables, VariableSets or Q Tables.
#' @description \code{Average} is a generalization of the arithmetic \code{\link{mean}} function
#'  but supporting additional pre-processing and matching of data before calculation
#'  by utilizing the \code{\link{Sum}} function to compute the numerator.
#' @details For \code{Average}, if a single input element is provided, then the output is computed in the same
#'   way as \code{\link{mean}}, i.e. all elements added together and divided by the number of inputs
#'   to give a single scalar value.
#'   If multiple input elements are provided, then the numerator is computed in the same way as \code{\link{Sum}}.
#'   That is, then element-wise addition is performed using the + function in \code{\link{Arithmetic}}.
#'   In the case of multiple inputs, the dimensions need to match before element-wise
#'   addition can occur. i.e. if the first element is a 3 x 2 matrix, then the second element needs to be
#'   a matrix of the same dimension. Partial dimension matching is also supported, so if an n x p matrix is
#'   used as the first input, then the second input could be an n x 1 column vector that is recycled to an
#'   n x p matrix before calculation.
#'
#'   If weights are provided, they are applied across the row dimension to the input elements. If there is a single
#'   input with multiple elements, then the weights in the denominator are computed accounting for the missingness of
#'   the elements. That is, the weight element will be omitted from the calculation if the input element is missing.
#'   Furthermore, if multiple inputs are provided, then any weights provided will be ignored since all elements in the
#'   element-wise mean computation will have the same weight and the weights are redundant.
#' @return The \code{Average} function returns the arithmetic mean of all the elements provided in the input,
#'   possibly after the elements have had entries removed, filtered out or weighted using the provided
#'   options.
#' @export
#' @examples
#' # Examples using average
#' Average(1:6)
#' mean(1:6)
#' input.matrix <- matrix(runif(6), nrow = 3, dimnames = list(letters[1:3], c("Q1", "Q2")))
#' mean(input.matrix)
#' Average(input.matrix)
#' Average(1:6, 1:6)
#' ((1:6) + (1:6))/2
#' x <- 1:6
#' y <- sample(1:6)
#' y
#' Average(x, y)
#' (x + y)/2
#' is.na(y) <- 3
#' is.na(x) <- 3:4
#' Average(x, y)
#' x <- matrix(1:12, nrow = 4, ncol = 3, dimnames = list(letters[1:4], LETTERS[1:3]))
#' y <- matrix(1:20, nrow = 5, ncol = 4, dimnames = list(letters[1:5], LETTERS[1:4]))
#' Average(x, y, remove.rows = "e", remove.columns = "D")
#' x <- matrix(1:12, nrow = 4, ncol = 3, dimnames = list(letters[1:4], letters[1:3]))
#' y <- matrix(1:20, nrow = 5, ncol = 4, dimnames = list(LETTERS[1:5], LETTERS[1:4]))
#' Average(x, y, match.elements = "Yes")
#' Average(x, y, match.elements = c(rows = "Fuzzy - show unmatched",
#'                                  columns = "Fuzzy - show unmatched"))
Average <- function(...,
                    remove.missing = TRUE,
                    remove.rows = NULL, remove.columns = NULL,
                    match.elements = "Yes",
                    subset = NULL, weights = NULL,
                    warn = FALSE)
{
    inputs <- list(...)
    if (identical(Filter(Negate(is.null), inputs), list()))
        return(NaN)
    fun.call <- match.call()
    fun.call[[1L]] <- sumInputs
    if (length(inputs) > 1L && !is.null(weights))
        fun.call[["weights"]] <- weights <- NULL
    return.total.element.weights <- if (weightsRequired(weights)) "TotalWeight" else "Yes"
    fun.call[["return.total.element.weights"]] <- return.total.element.weights
    fun.call[["function.name"]] <- sQuote("Average")
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    computed.sum <- eval.fun(fun.call, parent.frame())
    n.sum <- attr(computed.sum, "n.sum")
    attr(computed.sum, "n.sum") <- attr(computed.sum, "n.sum.removed") <-
        attr(computed.sum, "missing.in.all.inputs") <- NULL
    computed.sum / n.sum
}
