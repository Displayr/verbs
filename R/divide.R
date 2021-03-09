#' @rdname ArithmeticOperators
#' @title General-Purpose Arithmetic functions
#'
#' @description \code{Divide} is a generalization of the / function in \code{\link{Arithmetic}}
#'   but supporting additional pre-processing and matching of data before calculation.
#' @param numerator Numerator in the division e.g. a scalar, vector, matrix, Variable, Variable Set or Q Table
#' @param denominator Denominator in the division, assuming to be the same data structure
#'   as the numerator or can be coerced into an equivalent data structure.
#' @inheritParams Sum
#' @export
#' @examples
#' # TBA
Divide <- function(numerator = NULL,
                   denominator = NULL,
                   remove.missing = TRUE,
                   remove.rows = NULL, remove.columns = NULL,
                   match.rows = "Yes", match.columns = "Yes",
                   subset = NULL, weights = NULL,
                   warn = FALSE)
{
    calling.arguments <- match.call(expand.dots = FALSE)
    function.name <- sQuote(calling.arguments[[1L]])
    input <- list(numerator, denominator)
    checkBothNumeratorAndDenominatorExist(input, function.name)
    input <- processArguments(input, remove.missing = remove.missing,
                              remove.rows = remove.rows, remove.columns = remove.columns,
                              subset = subset, weights = weights,
                              warn = warn, check.statistics = FALSE,
                              function.name = function.name)
    input <- matchAndReshapeInputs(input, match.rows = match.rows, match.columns = match.columns,
                                   add.labels = FALSE, warn = warn, function.name = function.name)
    input <- if (remove.missing) lapply(input, removeMissing) else input
    # Simplify the array to a named scalar or named vector if appropriate
    input <- simplifyToScalarOrVectorIfNecessary(input)
    input[[1L]]/input[[2L]]
}


checkBothNumeratorAndDenominatorExist <- function(inputs, function.name)
{
    inputs.dont.exist <- vapply(inputs,
                                function(x) is.null(x) || length(x) == 0L,
                                logical(1L))
    if (any(inputs.dont.exist))
    {
        msg <- ngettext(sum(inputs.dont.exist),
                        paste("The", if(inputs.dont.exist[1L]) "numerator" else "denominator", "argument needs"),
                        "Both the numerator and denominator arguments need")
        msg <- paste(msg, "to be specified before", function.name, "can be calculated")
        stop(msg)
    }
}
