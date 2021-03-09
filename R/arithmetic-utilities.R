#' @param input List with two elements to have their internal dimensions matched or
#'   reshaped if required.
#' @inheritParams Sum
#' @noRd
matchAndReshapeInputs <- function(input, match.rows, match.columns, warn, function.name)
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
    if (any(!matching.required))
        input <- assignLabelsIfPossible(input,
                                        dimension = which(!matching.required))
    input
}
