context("Divide")

test_that("Both inputs required", {
    checkInputErrors <- function(input, expected.message)
    {
        fun <- checkBothNumeratorAndDenominatorExist
        generated.error <- capture_error(fun(input, sQuote("Divide")))
        expect_equal(generated.error[["message"]],
                     expected.message)
        expect_error(Divide(numerator = input[[1L]],
                            denominator = input[[2L]]),
                     expected.message)
    }
    checkInputErrors(replicate(2, NULL),
                     paste("Both the numerator and denominator arguments need",
                            "to be specified before", sQuote("Divide"),
                            "can be calculated"))
    checkInputErrors(list(NULL, 1),
                     paste("The numerator argument needs to be specified",
                           "before", sQuote("Divide"), "can be calculated"))
    checkInputErrors(list(1, NULL),
                     paste("The denominator argument needs to be specified",
                           "before", sQuote("Divide"), "can be calculated"))
})

checkDivideOutput <- function(input, expected.output, ...)
{
    args <- list(numerator = input[[1L]],
                 denominator = input[[2L]])
    if (!missing(...))
        args <- c(args, list(...))
    expect_equal(do.call("Divide", args), expected.output)
}

test_that("Divide: Scalar inputs", {
    # Unnamed scalars
    inputs <- replicate(5, runif(2, min = -1e3, max = 1e3), simplify = FALSE)
    expected.outputs <- lapply(inputs, function(x) x[1]/x[2])
    # Basic unnamed usage tests
    invisible(mapply(checkDivideOutput, inputs, expected.outputs))
    # Two named scalars
    input <- list(c(a = 1), c(b = 2))
    # When matching is requested
    expected.output <- c(a = 1/0, b = 0/2)
    checkDivideOutput(input, expected.output)
    params <- list(remove.missing = TRUE)
    expected.output <- c(a = 1/0, b = 0/2)
    checkDivideOutput(input, expected.output, remove.missing = TRUE)
    # No matching
    expected.output <- c(a = 1/2)
    checkDivideOutput(input, expected.output, match.rows = "No")
})

test_that("Divide: vector inputs", {
    m <- 7
    n <- 5
    inputs <- replicate(n,
                        replicate(2, runif(m, min = -1e3, max = 1e3), simplify = FALSE),
                        simplify = FALSE)
    expected.outputs <- lapply(inputs, function(x) x[[1L]]/x[[2L]])
    # Basic unnamed usage tests
    invisible(mapply(checkDivideOutput, inputs, expected.outputs))
    # Basic named vector matching
    numerator   <- c(a = 10, b = 9, c = 8, d = 7, e = 6)
    denominator <- c(c = 1,  e = 2, b = 3, a = 4, d = 5)
    expected.output <- numerator/denominator[names(numerator)]
    checkDivideOutput(list(numerator, denominator), expected.output)
    # Named vector, different size
    numerator   <- c(a = 10, b = 9, c = 8, d = 7, e = 6, f = 5)
    denominator <- c(c = 1,  e = 2, b = 3, a = 4, d = 5)
    matched.denominator <- c(denominator[names(numerator)[1:5]], f = 0)
    expected.output <- numerator/matched.denominator
    checkDivideOutput(list(numerator, denominator), expected.output)
    # Same named vector, different size. Sign switched and keep missing
    numerator   <- - c(a = 10, b = 9, c = 8, d = 7, e = 6, f = 5)
    denominator <-   c(c = 1,  e = 2, b = 3, a = 4, d = 5)
    matched.denominator <- c(denominator[names(numerator)[1:5]], f = NA)
    expected.output <- numerator/matched.denominator
    checkDivideOutput(list(numerator, denominator), expected.output, remove.missing = FALSE)
    # Named vector, but jumbled order in numerator, denominator longer
    numerator   <- c(c = 1,  a = 2, b = 3, e = 4, d = 5)
    denominator <- c(a = 10, b = 9, c = 8, d = 7, e = 6, f = 5)
    matched.denominator <- c(denominator[names(numerator)], f = 5)
    matched.numerator   <- c(numerator, f = 0)
    expected.output <- matched.numerator/matched.denominator
    checkDivideOutput(list(numerator, denominator), expected.output)
    # Numerator vector, denominator scalar
    numerator <- c(a = 10, b = 9, c = 8, d = 7, e = 6)
    denominator <- 5
    reshaped.denominator <- rep(5, length(numerator))
    expected.output <- numerator/reshaped.denominator
    checkDivideOutput(list(numerator, denominator), expected.output, match.rows = "No")
    expect_warning(checkDivideOutput(list(numerator, denominator), expected.output,
                                     match.rows = "No", warn = TRUE),
                   "A scalar element was reshaped to a vector with 5 rows")
})

test_that("Divide: Variables", {
    load("variable.Income.cat.rda")
    load("variable.Numeric.rda")
    # Categorical variable mapped to numeric using value attributes
    numerator <- variable.Income.cat
    denominator <- variable.Numeric
    input <- list(numerator, denominator)
    mapped.numerator <- AsNumeric(numerator, binary = FALSE)
    expected.output <- as.vector(mapped.numerator/denominator)
    checkDivideOutput(input, expected.output, remove.missing = FALSE)
    # Correct mapping when missing values removed
    mapped.inputs <- list(mapped.numerator, denominator)
    mapped.inputs.wo.missing <- lapply(list(mapped.numerator, denominator), removeMissing)
    expected.output <- as.vector(mapped.inputs.wo.missing[[1L]]/
                                     mapped.inputs.wo.missing[[2L]])
    checkDivideOutput(input, expected.output)
    # Check filters and weights
    ## Check filter, using gender
    load("variable.Gender.cat.rda")
    males <- variable.Gender.cat == "Male"
    filtered.input <- lapply(mapped.inputs, function(x) x[males])
    expected.output <- filtered.input[[1L]]/filtered.input[[2L]]
    checkDivideOutput(input, expected.output,
                      subset = males, remove.missing = FALSE)
    filtered.input.wo.missing <- lapply(filtered.input, removeMissing)
    expected.output <- filtered.input.wo.missing[[1L]]/filtered.input.wo.missing[[2L]]
    checkDivideOutput(list(numerator, denominator), expected.output,
                      subset = males)
    ## Check weights

})
