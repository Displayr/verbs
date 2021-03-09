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

test_that("Scalar inputs compute", {
    inputs <- replicate(5, runif(2, min = -1e3, max = 1e3), simplify = FALSE)
    checkScalarOutput <- function(input, params = NULL, expected.output)
    {
        expected.answer <- array(input[1]/input[2], dim = getDim(input[1]))
        expected.answer
        expect_equal(Divide(numerator = input[[1L]],
                            denominator = input[[2L]]),
                     expected.answer)
    }
    invisible(lapply(inputs, checkScalarOutput))
})
