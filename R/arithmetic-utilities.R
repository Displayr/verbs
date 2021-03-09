#' @param input List with two elements to have their internal dimensions matched or
#'   reshaped if required.
#' @inheritParams Sum
#' @param add.labels Logical to specify if the labels of the inputs should be pasted together
#' @noRd
matchAndReshapeInputs <- function(input, match.rows, match.columns, add.labels, warn, function.name)
{
    # Coerce any vectors to 1d array
    input <- coerceToVectorTo1dArrayIfNecessary(input)
    matching <- list(match.rows, match.columns)
    matching.required <- vapply(matching, function(x) x != "No", logical(1L))
    if (any(matching.required))
        input <- matchDimensionElements(input, match.rows, match.columns,
                                        warn, function.name)
    input <- reshapeIfNecessary(input, warn = warn, function.name = function.name)
    checkDimensionsEqual(input, function.name = function.name)
    if (add.labels && any(!matching.required))
        input <- assignLabelsIfPossible(input, dimension = which(!matching.required))
    input
}

#' Checks the two elements, if the array is 1d, then it will be
#' coerced to a scalar and get the names from the first input if
#' required.
#' @param input List with two array elements
#' @noRd
simplifyToScalarOrVectorIfNecessary <- function(input)
{
    if (all(unlist(lapply(input, getDim)) == 1L))
    {
        input.names <- names(input[[1L]])
        input <- lapply(input, as.vector)
        if (!is.null(input.names))
            names(input[[1L]]) <- input.names
    }
    input
}
