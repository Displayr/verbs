#' @rdname AverageOperations
#' @title General-Purpose Averaging functions
#' @inheritParams Sum
#'
#' @description \code{Average} is a generalization of \code{\link{mean}} function
#'  but supporting additional pre-processing and matching of data before calculation
#'  by utilizing the \code{\link{Sum}} function to compute the numerator.
#' @details For \code{Sum}, if a single input element is provided, then the element is added in the same
#'   way as \code{\link{sum}}, i.e. all elements added together to give a single scalar value.
#'   If multiple input elements are provided, then elementwise addition is performed in a similar way
#'   to the + function in \code{\link{Arithmetic}}. In the case of multiple inputs, the dimensions need to match before elementwise
#'   addition can occur. i.e. if the first element is a 3 x 2 matrix, then the second element needs to be
#'   a matrix of the same dimension. Partial dimension matching is also supported, so if an n x p matrix is
#'   used as the first input, then the second input could be an n x 1 column vector that is reshaped to an
#'   n x p matrix before calculation.
#' @return The \code{Average} function returns the summation of all the elements provided in the input,
#'   possibly after the elements have had entries removed, filtered out or weighted using the provided
#'   options.
#' @export
#' @examples
#' # TBA
Average <- function(...,
                    remove.missing = TRUE,
                    remove.rows = NULL, remove.columns = NULL,
                    match.rows = "Yes", match.columns = "Yes",
                    subset = NULL, weights = NULL,
                    warn = FALSE)
{
    called.args <- match.call(expand.dots = FALSE)
    function.args <- formals()
    called.args[[1L]] <- as.name('list')
    called.args[["..."]] <- function.args[["..."]] <- NULL
    called.args <- eval(called.args, parent.frame())
    matched.args <- match(names(called.args), names(function.args), nomatch = 0L)
    if (length(matched.args))
        function.args[matched.args] <- called.args[]
    inputs <- list(...)
    inputs <- Filter(Negate(is.null), inputs)
    if (identical(inputs, list()))
        return(NaN)
    attr(inputs[[1L]], "called.from.average") <- "Average"
    new.arguments <- c(inputs, function.args)
    computed.sum <- do.call(Sum, new.arguments)
    n.sum <- attr(computed.sum, "n.sum")
    if (length(inputs) > 1L && any(missing.in.all <- attr(computed.sum, "missing.in.all.inputs")))
    {
        if (!remove.missing)
            n.sum <- array(n.sum, dim = dim(missing.in.all), dimnames = dimnames(missing.in.all))
        n.sum[missing.in.all] <- 0
    }
    attr(computed.sum, "n.sum") <- attr(computed.sum, "n.sum.removed") <-
        attr(computed.sum, "missing.in.all.inputs") <- NULL
    computed.sum / n.sum
}
