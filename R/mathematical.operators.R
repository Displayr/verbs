#' @rdname ArithmeticOperators
#' @title General-Purpose Arithmetic functions
#'
#' @description The binary operations of \code{/}, \code{*} and \code{-} in \code{\link{Arithmetic}}
#' are generalized in a similar manner \code{Sum} generalizes \code{+}.
#' @details The binary operations of division, multiplication and subtraction are
#' generalized to allow the pre-processing of inputs before the operation occurs.
#' The pre-processing operations include the removal of named rows or columns of
#' the inputs and matching by name across the rows and columns. This is the same
#' behavior as in \code{Sum} except only two inputs are allowed while an arbitrary
#' number of inputs are allowed in \code{Sum}.
#' @param numerator Numerator in the division e.g. a scalar, vector, matrix, Variable, Variable Set or Q Table
#' @param denominator Denominator in the division, assuming to be the same data structure
#'   as the numerator or can be coerced into an equivalent data structure.
#' @inheritParams Sum
#' @export
#' @examples
#' scalar.x <- runif(4)
#' scalar.y <- runif(4)
#' all.equal(Divide(scalar.x, scalar.y), scalar.x / scalar.y)
#' x <- matrix(1:12, nrow = 4, ncol = 3, dimnames = list(letters[1:4], LETTERS[1:3]))
#' y <- matrix(1:20, nrow = 5, ncol = 4, dimnames = list(letters[1:5], LETTERS[1:4]))
#' Divide(x, y)
#' Divide(x, 1:4)
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
#' @inheritParams Sum
#' @param multiplicand The multiplicand in the multiplication operation. That is, the
#' \code{x} in the operation \code{x * y}.
#' @param multiplier The multiplier in the multiplication operation. That is, the
#' \code{y} in the operation \code{x * y}.
#' @export
#' @examples
#' all.equal(Multiply(scalar.x, scalar.y), scalar.x * scalar.y)
#' Multiply(x, y)
#' Multiply(x, 1:4)
Multiply <- function(multiplicand = NULL,
                     multiplier = NULL,
                     remove.rows = NULL, remove.columns = NULL,
                     match.elements = "Yes",
                     subset = NULL,
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
#' @inheritParams Sum
#' @param minuend The minuend in the subtraction operation. That is, the
#' \code{x} in the operation \code{x - y}.
#' @param subtrahend The subtrahend in the subtraction operation. That is, the
#' \code{y} in the operation \code{x - y}.
#' @export
#' @examples
#' all.equal(Subtract(scalar.x, scalar.y), scalar.x - scalar.y)
#' Subtract(x, y)
#' Subtract(x, 1:4)
Subtract <- function(minuend = NULL,
                     subtrahend = NULL,
                     remove.rows = NULL, remove.columns = NULL,
                     match.elements = "Yes",
                     subset = NULL,
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
                              check.statistics = TRUE,
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

throwWarningAboutDivisionByZeroIfNecessary <- function(input, output, function.name)
{
    inf.output <- if (is.data.frame(output)) is.infinite(as.matrix(output)) else is.infinite(output)
    if (any(inf.output))
    {
        zero.denominator <- input[[2L]] == 0
        division.by.zero <- inf.output & zero.denominator
        n.output <- length(output)
        all.division.by.zero <- all(division.by.zero)
        if (all.division.by.zero && n.output == 1L)
        {
            warning("The denominator is zero and resulted in Infinity in the output for ", function.name, ".")

        } else if (all.division.by.zero && n.output > 1L)
            warning("All elements in the denominator were zero and resulted in ",
                    "values of Infinity in the output for ", function.name, ".")
        else if (any(division.by.zero))
            warning("Some elements in the denominator were zero and resulted in ",
                    "values of Infinity in the output for ", function.name, ".")
    }
}

#' @title Inspect two variable sets and both share the same variable names
#' @param first First Variable Set to perform the matching
#' @param second Second Variable Set to perform the matching
#' @param remove.columns character vector containing variable names (column names) to
#'   ignore
#' @param function.name String containing the function name, used in the thrown error messages.
#' @details Takes two inputs and checks if both variable set inputs contain the same variable names
#'  on their columns (after ignoring data reductions).
#'  These can be of different order but all elements need to exist in both inputs.
#'  If that isn't the case then the function will thrown an error with a message containing the
#'  variable names that are not present in both inputs.
#' @export
CheckInputVariableNamesMatch <- function(first, second,
                                         remove.columns = c("NET", "SUM", "Total"),
                                         function.name)
{
    function.name <- sQuote(function.name)
    input <- list(first, second)
    variable.set.inputs <- vapply(input, isVariableSet, logical(1L))
    if (!all(variable.set.inputs))
        stop("Both the inputs need to be Variable Sets")
    input <- processArguments(input,
                              remove.missing = FALSE,
                              remove.rows = NULL, remove.columns = remove.columns,
                              subset = NULL, weights = NULL,
                              return.total.element.weights = "No",
                              function.name = function.name)
    input <- coerceToVectorTo1dArrayIfNecessary(input)
    basic.names <- lapply(input, colnames)
    common.names <- intersect(basic.names[[1L]], basic.names[[2L]])
    if (length(common.names) == 0L)
        throwErrorAboutUnmatchedVariables(unlist(basic.names), function.name)
    match.elements = c("rows" = "No",
                       "columns" = "Yes - hide unmatched")
    matched.input <- suppressWarnings(matchInputsUsingCustomArgs(input, match.elements = match.elements,
                                                                 operation = `+`, warn = TRUE, function.name))
    if (!is.null(unmatched.variables <- attr(matched.input, "unmatched")))
        throwErrorAboutUnmatchedVariables(unmatched.variables, function.name)
}

throwErrorAboutUnmatchedVariables <- function(unmatched.variable.names, function.name)
{
    unmatched.variable.names <- dQuote(unmatched.variable.names)
    n <- length(unmatched.variable.names)
    if (n > 1L)
    {
        unmatched.variable.names <- paste0(paste0(unmatched.variable.names[1:(n - 1L)], collapse = ", "),
                                           " and ", unmatched.variable.names[n])
    }
    error.msg <- paste0("Two variable sets with more than one variable have been used as input and ",
                        "they need to contain the same variables so they can be matched before ",
                        function.name, " can be computed. ",
                        "However, the following variables were not present in both variable sets: ",
                        "(", unmatched.variable.names, "). ",
                        "Either relabel the variables or modify the variable sets so that they all can ",
                        "be matched and ", function.name, " can be computed.")
    stop(error.msg)
}

throwWarningAboutBothElementsZeroInDivisionIfNecessary <- function(input, output, function.name)
{
    nan.output <- if (is.data.frame(output)) is.nan(as.matrix(output)) else is.nan(output)
    if (any(nan.output))
    {
        zeros <- lapply(input, function(x) x == 0L)
        zeros <- zeros[[1L]] & zeros[[2L]]
        all.nan <- all(nan.output & zeros)
        some.nan <- any(nan.output & zeros)
        if (all.nan)
        {
            warning("The calculated output values are NaN (Not a Number) in ", function.name,
                    " since both the numerator and denominator are zero.")

        } else if (some.nan)
            warning("Some of the calculated output values are NaN (Not a Number) in ", function.name,
                    " since both the numerator and denominator are zero.")
    }
}

