#' @rdname ExtremeOperations
#' @title General-Purpose Calculation of Maxima and Minima
#'
#' @description \code{Max} (\code{Min}) is a generalization of
#'     \code{\link{max}} (\code{\link{min}}) supporting additional
#'     pre-processing and matching of data before calculation.
#' @param ... Input objects to compute the minima or maxima;
#'     e.g. vectors, matrices, Variables, Variable Sets or Q Tables
#' @inheritParams Sum
#' @details If a single input is provided, then the maximum or minimum
#'     of all elements in the input is return (replicating
#'     \code{\link{min}} and \code{\link{max}}).  If multiple inputs
#'     are provided, then element-wise addition is performed using
#'     \code{\link{pmin}} or \code{\link{pmax}}. In the case of
#'     multiple inputs, the dimensions need to match before
#'     element-wise calculation can occur. For example, if the first
#'     element is a 3 x 2 matrix, then the second element needs to be
#'     a matrix of the same dimension.
#'
#' If the inputs have named elements, then these names can be used to
#'     match up each of the elements between inputs via the
#'     \code{match.rows} and \code{match.columns} arguments. If either
#'     of \code{match.rows} or \code{match.columns} is set to
#'     \code{"No"} then names are ignored and the length on that
#'     dimension needs to agree between inputs. Partial dimension
#'     agreement is also supported. For example if an n x p matrix is
#'     used as the first input, then the second input could be an n x
#'     1 column vector that is recycled to an n x p matrix before
#'     calculation.
#' @return If a single input is provided, a single number is returned,
#'     the maximum/minimum of all the elements. If multiple
#'     inputs, the output will have the same dimension as the input
#'     and contain the parallel (element-wise) maximum/minimums are
#'     returned.
#' @export
#' @examples
#' # Examples using Sum
#' x <- c(NA, 1:3)
#' Max(x)
#' Max(x, remove.missing = FALSE)
#'
#' x <- matrix(1:12, nrow = 4, ncol = 3, dimnames = list(letters[1:4], LETTERS[1:3]))
#' y <- matrix(20:1, nrow = 5, ncol = 4, dimnames = list(letters[1:5], LETTERS[1:4]))
#' Max(x, y, remove.rows = "e", remove.columns = "D")
Max <- function(...,
                remove.missing = TRUE,
                remove.rows = NULL, remove.columns = NULL,
                match.elements = "Yes",
                subset = NULL,
                warn = FALSE)
{
    fun.call <- match.call()
    fun.call[[1L]] <- calculateExtremum
    fun.call[["type"]] <- "Max"
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    eval.fun(fun.call, parent.frame())
}

#' @rdname ExtremeOperations
#' @export
#' @examples
#' x <- matrix(1:12, nrow = 4, ncol = 3, dimnames = list(letters[1:4], letters[1:3]))
#' y <- matrix(20:1, nrow = 5, ncol = 4, dimnames = list(LETTERS[1:5], LETTERS[1:4]))
#' Min(x, y, match.elements = "Yes")
#' Min(x, y, match.elements = c(rows = "Fuzzy - show unmatched",
#'                              columns = "Fuzzy - show unmatched"))
Min <- function(...,
                remove.missing = TRUE,
                remove.rows = NULL, remove.columns = NULL,
                match.elements = "Yes",
                subset = NULL,
                warn = FALSE)
{
    fun.call <- match.call()
    fun.call[[1L]] <- calculateExtremum
    fun.call[["type"]] <- "Min"
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    eval.fun(fun.call, parent.frame())
}

calculateExtremum <- function(...,
                              type = "Max",
                              remove.missing = TRUE,
                              remove.rows = NULL, remove.columns = NULL,
                              match.elements = "Yes",
                              subset = NULL,
                              warn = FALSE)
{
    x <- list(...)
    n.inputs <- length(x)
    fname <- sQuote(type)
    x <- processArguments(x,
                          remove.missing = remove.missing,
                          remove.rows = remove.rows, remove.columns = remove.columns,
                          subset = subset, weights = NULL,
                          return.total.element.weights = FALSE,
                          check.statistics = TRUE,
                          warn = warn,
                          function.name = fname)
    if (n.inputs == 1)
    {
        x <- x[[1L]]
        function.to.use <- if (identical(type, "Max")) max else min
        if (isQTable(x) && statisticsPresentInLastDim(x))
            output <- apply(x, getDimensionLength(x), baseExtreme,
                            fun = function.to.use,
                            remove.missing = remove.missing)
        else
            output <- baseExtreme(x, fun = function.to.use, remove.missing = remove.missing)
    }else
    {
        match.elements[tolower(match.elements) == "yes"] <- "Yes - hide unmatched"
        match.elements <- checkMatchingArguments(match.elements,
                                                 function.name = fname)
        extreme.fun <- if (identical(type, "Max")) pmax else pmin
        .Fun <- function(x, y)
            calculateBinaryOperation(x, y,
                                     operation = extreme.fun,
                                     match.elements = match.elements,
                                     remove.missing = remove.missing,
                                     function.name = fname,
                                     warn = warn)
        output <- Reduce(.Fun, x)
        if (warn)
        {
            throwWarningIfTransposedInput(output, fname)
            unmatched.elements <- attr(output, "unmatched")
            if (!is.null(unmatched.elements))
                throwWarningAboutUnmatched(unmatched.elements, fname)
        }

        output <- sanitizeAttributes(output)
    }
    if (getDimensionLength(output) == 1L)
        output <- setNames(as.vector(output), nm = names(output))
    output
}

baseExtreme <- function(x, fun, remove.missing = TRUE)
{
    if (allNA(x))
        return(NA)
    fun(x, na.rm = remove.missing)
}
