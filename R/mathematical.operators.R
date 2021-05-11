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
                   remove.rows = NULL, remove.columns = NULL,
                   match.elements = "Yes",
                   subset = NULL,
                   warn = FALSE)
{
    mathOperator(first = numerator, second = denominator,
                 remove.rows = remove.rows, remove.columns = remove.columns,
                 match.elements = match.elements,
                 subset = subset,
                 function.operator = `/`,
                 warn = warn,
                 function.name = sQuote("Divide"))
}

#' @rdname ArithmeticOperators
#' @export
Multiply <- function(multiplicand = NULL,
                     multiplier = NULL,
                     remove.rows = NULL, remove.columns = NULL,
                     match.elements = "Yes",
                     subset = NULL, weights = NULL,
                     warn = FALSE)
{
    mathOperator(first = multiplicand, second = multiplier,
                 remove.rows = remove.rows, remove.columns = remove.columns,
                 match.elements = match.elements,
                 subset = subset,
                 function.operator = `*`,
                 warn = warn,
                 function.name = sQuote("Multiply"))
}

#' @rdname ArithmeticOperators
#' @export
Subtract <- function(minuend = NULL,
                     subtrahend = NULL,
                     remove.rows = NULL, remove.columns = NULL,
                     match.elements = "Yes",
                     subset = NULL, weights = NULL,
                     warn = FALSE)
{
    mathOperator(first = minuend, second = subtrahend,
                 remove.rows = remove.rows, remove.columns = remove.columns,
                 match.elements = match.elements,
                 subset = subset,
                 function.operator = `-`,
                 warn = warn,
                 function.name = sQuote("Subtract"))
}

mathOperator <- function(first = NULL,
                         second = NULL,
                         remove.rows = NULL, remove.columns = NULL,
                         match.elements = "Yes",
                         subset = NULL,
                         function.operator = `/`,
                         warn = FALSE,
                         function.name)
{
    calling.arguments <- match.call(expand.dots = FALSE)
    operand.names <- names(formals(sys.function(sys.parent(1L)))[1:2])
    input <- list(first, second)
    checkBothInputsExist(input, function.name, operand.names)
    input <- processArguments(input,
                              remove.missing = FALSE,
                              remove.rows = remove.rows, remove.columns = remove.columns,
                              subset = subset, weights = NULL,
                              return.total.element.weights = "No",
                              check.statistics = FALSE,
                              warn = warn,
                              function.name = function.name)
    match.elements[tolower(match.elements) == "yes"] <- "Yes - hide unmatched"
    match.elements <- checkMatchingArguments(match.elements,
                                             function.name = function.name)
    result <- calculateBinaryOperation(input[[1L]], input[[2L]],
                                       operation = function.operator,
                                       match.elements = match.elements,
                                       remove.missing = FALSE,
                                       with.count.attribute = FALSE,
                                       warn = warn,
                                       function.name = function.name)
    if (warn)
    {
        throwWarningIfTransposedInput(result, function.name)
        unmatched.elements <- attr(result, "unmatched")
        if (!is.null(unmatched.elements))
            throwWarningAboutUnmatched(unmatched.elements, function.name)
    }
    result <- sanitizeAttributes(result)
    if (getDimensionLength(result) == 1L)
        result <- setNames(as.vector(result), nm = names(result))
    result
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

