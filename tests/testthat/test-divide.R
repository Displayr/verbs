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
