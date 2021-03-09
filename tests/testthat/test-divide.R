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
        args <- c(args, ...)
    expect_equal(do.call("Divide", args), expected.output)
}

test_that("Scalar inputs compute", {

    # Unnamed scalars
    inputs <- replicate(5, runif(2, min = -1e3, max = 1e3), simplify = FALSE)
    expected.outputs <- lapply(inputs, function(x) x[1]/x[2])
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
    expected.output <- 1/2
    checkDivideOutput(input, expected.output, match.rows = "No")
})
