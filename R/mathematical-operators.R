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
#'
Divide <- function(numerator = NULL,
                   denominator = NULL,
                   remove.missing = TRUE,
                   remove.rows = NULL, remove.columns = NULL,
                   match.rows = "Yes", match.columns = "Yes",
                   subset = NULL,
                   warn = FALSE)
{
    mathOperator(first = numerator, second = denominator,
                 remove.missing = remove.missing,
                 remove.rows = remove.rows, remove.columns = remove.columns,
                 match.rows = match.rows, match.columns = match.columns,
                 subset = subset,
                 function.operator = `/`,
                 warn = warn,
                 function.name = sQuote("Divide"))
}

Multiply <- function(multiplicand = NULL,
                     multiplier = NULL,
                     remove.missing = TRUE,
                     remove.rows = NULL, remove.columns = NULL,
                     match.rows = "Yes", match.columns = "Yes",
                     subset = NULL, weights = NULL,
                     warn = FALSE)
{
    mathOperator(first = multiplicand, second = multiplier,
                 remove.missing = remove.missing,
                 remove.rows = remove.rows, remove.columns = remove.columns,
                 match.rows = match.rows, match.columns = match.columns,
                 subset = subset,
                 function.operator = `*`,
                 warn = warn,
                 function.name = sQuote("Multiply"))
}

Subtract <- function(minuend = NULL,
                     subtrahend = NULL,
                     remove.missing = TRUE,
                     remove.rows = NULL, remove.columns = NULL,
                     match.rows = "Yes", match.columns = "Yes",
                     subset = NULL, weights = NULL,
                     warn = FALSE)
{
    inputs <- list(minuend, subtrahend)
    checkBothInputsExist(inputs, sQuote("Subtract"), c("minuend", "subtrahend"))
    inputs <- removeCharacterStatisticsFromQTables(x)
    checkInputTypes(inputs, function.name = function.name)
    inputs <- lapply(inputs, extractChartDataIfNecessary)
    inputs <- convertToNumeric(inputs)
    inputs[[2L]] <- inputs[[2L]] * -1L
    sumInputs(inputs[[1L]], inputs[[2L]],
              remove.missing = remove.missing,
              remove.rows = remove.rows, remove.columns = remove.columns,
              match.rows = match.rows, match.columns = match.columns,
              subset = subset,
              weights = weights,
              return.total.element.weights = "No",
              warn = warn,
              function.name = sQuote("Subtract"))
}

mathOperator <- function(first = NULL,
                         second = NULL,
                         remove.missing = TRUE,
                         remove.rows = NULL, remove.columns = NULL,
                         match.rows = "Yes", match.columns = "Yes",
                         subset = NULL,
                         function.operator = `/`,
                         warn = FALSE,
                         function.name)
{
    calling.arguments <- match.call(expand.dots = FALSE)
    operand.names <- names(formals(sys.function(sys.parent(1L)))[1:2])
    input <- list(first, second)
    checkBothInputsExist(input, function.name, operand.names)
    input <- processArguments(input, remove.missing = remove.missing,
                              remove.rows = remove.rows, remove.columns = remove.columns,
                              subset = subset, weights = NULL,
                              warn = warn, check.statistics = FALSE,
                              function.name = function.name)
    input <- matchAndRecycleInputs(input, match.rows = match.rows, match.columns = match.columns,
                                   add.labels = FALSE, warn = warn, function.name = function.name)
    input <- if (remove.missing) lapply(input, removeMissing) else input
    # Simplify the array to a named scalar or named vector if appropriate
    input <- simplifyToScalarOrVectorIfNecessary(input)
    output <- function.operator(input[[1L]], input[[2L]])
    output <- sanitizeAttributes(output)
    output
}


checkBothInputsExist <- function(inputs, function.name, operand.names)
{
    inputs.dont.exist <- vapply(inputs,
                                function(x) is.null(x) || length(x) == 0L,
                                logical(1L))
    if (any(inputs.dont.exist))
    {
        msg <- ngettext(sum(inputs.dont.exist),
                        paste("The", if(inputs.dont.exist[1L]) operand.names[1L] else operand.names[2L], "argument needs"),
                        paste("Both the", paste0(operand.names, collapse = " and "), "arguments need"))
        msg <- paste(msg, "to be specified before", function.name, "can be calculated")
        stop(msg)
    }
}


