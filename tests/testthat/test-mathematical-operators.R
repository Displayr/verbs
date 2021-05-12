context("Mathematical Operators: Divide, Multiply, Subtract")

test_that("Both inputs required", {
    checkInputErrors <- function(input, operator.fun,
                                 function.name,
                                 operand.names,
                                 expected.message)
    {
        fun <- checkBothInputsExist
        generated.error <- capture_error(fun(input, sQuote(function.name),
                                             operand.names))
        expect_equal(generated.error[["message"]],
                     expected.message)
        expect_error(operator.fun(input[[1L]], input[[2L]]),
                     expected.message)
    }
    checkInputErrors(replicate(2, NULL), Divide, "Divide",
                     c("numerator", "denominator"),
                     paste("Both the numerator and denominator arguments need",
                           "to be specified before", sQuote("Divide"),
                           "can be calculated"))
    checkInputErrors(list(NULL, 1), Divide, "Divide",
                     c("numerator", "denominator"),
                     paste("The numerator argument needs to be specified",
                           "before", sQuote("Divide"), "can be calculated"))
    checkInputErrors(list(1, NULL), Divide, "Divide",
                     c("numerator", "denominator"),
                     paste("The denominator argument needs to be specified",
                           "before", sQuote("Divide"), "can be calculated"))
    checkInputErrors(replicate(2, NULL), Multiply, "Multiply",
                     c("multiplicand", "multiplier"),
                     paste("Both the multiplicand and multiplier arguments need",
                           "to be specified before", sQuote("Multiply"),
                           "can be calculated"))
    checkInputErrors(list(NULL, 1), Multiply, "Multiply",
                     c("multiplicand", "multiplier"),
                     paste("The multiplicand argument needs to be specified",
                           "before", sQuote("Multiply"), "can be calculated"))
    checkInputErrors(list(1, NULL), Multiply,  "Multiply",
                     c("multiplicand", "multiplier"),
                     paste("The multiplier argument needs to be specified",
                           "before", sQuote("Multiply"), "can be calculated"))

})


checkOperatorOutput <- function(input, expected.output, test.function, ...)
{
    args <- list(input[[1L]], input[[2L]])
    if (!missing(...))
        args <- c(args, list(...))
    expect_equal(do.call(test.function, args), expected.output)
}

expectedOutput <- function(x, fun.operator)
{
    fun.operator(x[[1]], x[[2]])
}

verb.function.names <- c("Divide", "Multiply", "Subtract")
verbs.operator.functions <- c(Divide, Multiply, Subtract)
base.operator.functions <- c(`/`, `*`, `-`)
operator.labels <- c("/", "*", "-")

for (fun.ind in seq_along(verbs.operator.functions))
{
    base.operator  <- base.operator.functions[[fun.ind]]
    verbs.operator <- verbs.operator.functions[[fun.ind]]
    fun.name <- verb.function.names[fun.ind]
    verb.label <- operator.labels[fun.ind]
    test_that(paste0(fun.name, ": Scalar inputs"), {
        # Unnamed scalars
        inputs <- replicate(5, runif(2, min = -1e3, max = 1e3), simplify = FALSE)
        expected.outputs <- lapply(inputs, expectedOutput, fun.operator = base.operator)
        # Basic unnamed usage tests
        invisible(mapply(checkOperatorOutput, inputs, expected.outputs,
                         MoreArgs = list(test.function = verbs.operator)))
        # Two named scalars
        rand.in <- runif(2)
        input <- list(c(foo = rand.in[1]), c(bar = rand.in[2]))
        # When no matching is requested
        expected.output <- setNames(base.operator(rand.in[1], rand.in[2]),
                                    nm = paste0("foo ", verb.label, " bar"))
        checkOperatorOutput(input, expected.output, verbs.operator, match.elements = "No")

        # Error when matching requested
        expect_error(verbs.operator(input[[1L]], input[[2L]]),
                     capture_error(throwErrorNoMatchingElementsFound(sQuote(fun.name)))[["message"]],
                     fixed = TRUE)
        input <- lapply(input, setNames, nm = "foo")
        expected.output <- c(foo = base.operator(rand.in[1], rand.in[2]))
        checkOperatorOutput(input, expected.output, verbs.operator)
    })

    test_that(paste0(fun.name, ": vector inputs"), {
        m <- 7
        n <- 5
        inputs <- replicate(n,
                            replicate(2, runif(m, min = -1e3, max = 1e3), simplify = FALSE),
                            simplify = FALSE)
        expected.outputs <- lapply(inputs, expectedOutput, fun.operator = base.operator)
        # Basic unnamed usage tests
        invisible(mapply(checkOperatorOutput, inputs, expected.outputs,
                         MoreArgs = list(test.function = verbs.operator)))
        # Basic named vector matching
        first.operand  <- c(foo = 10, bar = 9, baz = 8, dog = 7, cat = 6)
        second.operand <- c(baz = 1,  cat = 2, bar = 3, foo = 4, dog = 5)
        expected.output <- expectedOutput(list(first.operand, second.operand[names(first.operand)]),
                                          fun.operator = base.operator)
        checkOperatorOutput(list(first.operand, second.operand), expected.output, verbs.operator)
        # Named vector, different size
        first.operand  <- - c(foo = 10, bar = 9, baz = 8, dog = 7, cat = 6, rat = 5)
        second.operand <-   c(baz = 1,  cat = 2, bar = 3, foo = 4, dog = 5)
        matched.second.operand <- c(second.operand[names(first.operand)[1:5]], rat = NA)
        expected.output <- base.operator(first.operand, matched.second.operand)
        checkOperatorOutput(list(first.operand, second.operand), expected.output, verbs.operator,
                            match.elements = "Yes - show unmatched")
        # Named vector, but jumbled order in first.operand, second.operand longer
        first.operand  <- c(baz = 1,  foo = 2, bar = 3, cat = 4, dog = 5)
        second.operand <- c(foo = 10, bar = 9, baz = 8, dog = 7, cat = 6, rat = 5)
        matched.second.operand <- c(second.operand[names(first.operand)], rat = 5)
        matched.first.operand   <- c(first.operand, rat = NA)
        expected.output <- base.operator(matched.first.operand, matched.second.operand)
        checkOperatorOutput(list(first.operand, second.operand), expected.output, verbs.operator,
                            match.elements = "Yes - show unmatched")
        # first.operand vector, second.operand scalar
        first.operand <- c(foo = 10, bar = 9, baz = 8, dog = 7, cat = 6)
        second.operand <- 5
        recycled.second.operand <- rep(5, length(first.operand))
        expected.output <- base.operator(first.operand, recycled.second.operand)
        checkOperatorOutput(list(first.operand, second.operand), expected.output, verbs.operator,
                            match.elements = c("No", "No"))
    })

    test_that(paste0(fun.name, ": matrices, tables"), {
        # Basic matrix
        first.operand <- matrix(1:12, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:4]))
        second.operand <- matrix(12:1, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:4]))
        input <- list(first.operand, second.operand)
        expected.output <- base.operator(first.operand, second.operand)
        checkOperatorOutput(input, expected.output, verbs.operator)
        # basic matrix with matching
        shuffled.second.operand <- second.operand[c(2, 3, 1), c(2, 4, 3, 1)]
        input <- list(first.operand, shuffled.second.operand)
        checkOperatorOutput(input, expected.output, verbs.operator)
        # Basic matrix with extra dims
        diff.second.operand <- array(20:1, dim = c(4, 5), dimnames = list(letters[1:4], LETTERS[1:5]))
        diff.second.operand <- diff.second.operand[sample(1:4), sample(1:5)]
        larger.first.operand <- rbind(cbind(first.operand, E = NA), d = NA)
        expected.output <- base.operator(first.operand, diff.second.operand[letters[1:3], LETTERS[1:4]])
        input <- list(first.operand, diff.second.operand)
        checkOperatorOutput(input, expected.output, verbs.operator)
        # Basic matrix with no matching
        input <- list(first.operand, shuffled.second.operand)
        expected.output <- base.operator(first.operand, shuffled.second.operand)
        expected.names  <- mapply(function(x.names, y.names) paste0(x.names, " ", verb.label, " ", y.names),
                                  dimnames(input[[1L]]),
                                  dimnames(input[[2L]]),
                                  SIMPLIFY = FALSE)
        dimnames(expected.output) <- expected.names
        checkOperatorOutput(input, expected.output, verbs.operator, match.elements = c("No", "No"))
        # Basic matrix with recycled vector (n x 1) -> (n x m)
        second.operand <- 1:3
        recycled.second.operand <- array(second.operand, dim = 3:4)
        expected.output <- base.operator(first.operand, recycled.second.operand)
        input <- list(first.operand, second.operand)
        checkOperatorOutput(input, expected.output, verbs.operator, match.elements = c("No", "No"))
        ## Check reverse order
        expected.output <- base.operator(recycled.second.operand, first.operand)
        input <- rev(input)
        checkOperatorOutput(input, expected.output, verbs.operator, match.elements = c("No", "No"))
        # Basic matrix with recycled vector (m x 1) -> (n x m)
        second.operand <- 1:4
        recycled.second.operand <- array(rep(second.operand, each = 3), dim = 3:4)
        expected.output <- base.operator(first.operand, recycled.second.operand)
        input <- list(first.operand, second.operand)
        checkOperatorOutput(input, expected.output, verbs.operator, match.elements = c("No", "No"))
        ## Check reverse order
        expected.output <- base.operator(recycled.second.operand, first.operand)
        input <- rev(input)
        checkOperatorOutput(input, expected.output, verbs.operator, match.elements = c("No", "No"))
        # Basic matrix with recycled column vector (n x 1) -> (n x m)
        second.operand <- matrix(1:3, nrow = 3)
        recycled.second.operand <- array(second.operand, dim = 3:4)
        expected.output <- base.operator(first.operand, recycled.second.operand)
        input <- list(first.operand, second.operand)
        checkOperatorOutput(input, expected.output, verbs.operator, match.elements = c("No", "No"))
        ## Check reverse order
        expected.output <- base.operator(recycled.second.operand, first.operand)
        input <- rev(input)
        checkOperatorOutput(input, expected.output, verbs.operator, match.elements = c("No", "No"))
        # Basic matrix with recycled row vector (1 x m) -> (n x m)
        second.operand <- matrix(1:4, nrow = 1)
        recycled.second.operand <- array(rep(second.operand, each = 3), dim = 3:4)
        expected.output <- base.operator(first.operand, recycled.second.operand)
        input <- list(first.operand, second.operand)
        checkOperatorOutput(input, expected.output, verbs.operator, match.elements = c("No", "No"))
        ## Check reverse order
        expected.output <- base.operator(recycled.second.operand, first.operand)
        input <- rev(input)
        checkOperatorOutput(input, expected.output, verbs.operator, match.elements = c("No", "No"))
        # Basic matrix with scalar second.operand
        input <- list(first.operand, 3)
        expected.output <- base.operator(first.operand, 3)
        checkOperatorOutput(input, expected.output, verbs.operator, match.elements = c("No", "No"))
        ## Check reverse order
        expected.output <- base.operator(3, first.operand)
        input <- rev(input)
        checkOperatorOutput(input, expected.output, verbs.operator, match.elements = c("No", "No"))
    })

    test_that(paste0(fun.name, ": QTables handled (flattened)"), {
        load("numeric.grid.with.multiple.stats.qtable.rda")
        first.operand <- numeric.grid.with.multiple.stats.qtable
        second.operand <- first.operand + array(runif(length(first.operand)),
                                                dim = dim(first.operand),
                                                dimnames = dimnames(first.operand))
        input <- list(first.operand, second.operand)
        expected.output <- sanitizeAttributes(base.operator(first.operand, second.operand))
        checkOperatorOutput(input, expected.output, verbs.operator)
        ## Row and column removal is working
        table.dimnames <- dimnames(first.operand)
        first.operand.subset <- first.operand[!table.dimnames[[1L]] == "SUM",
                                              !table.dimnames[[2L]] == "SUM",]
        second.operand.subset <- second.operand[!table.dimnames[[1L]] == "SUM",
                                                !table.dimnames[[2L]] == "SUM",]
        expected.output <- base.operator(first.operand.subset, second.operand.subset)
        checkOperatorOutput(input, expected.output, verbs.operator,
                            remove.rows = "SUM", remove.columns = "SUM")
        ## Extra unmatched is ok
        # Hack in an extra row, it is inaccurate (dont have the original data)
        # but is sufficient for testing purposes
        first.operand.extra <- first.operand
        extra.rows <- colSums(first.operand[c("Breakfast", "Lunch", "Dinner"), , ])
        new.dimnames <- dimnames(first.operand)
        new.dimnames[[1L]] <- c(new.dimnames[[1L]], "Breakfast + Lunch + Dinner")
        first.operand.extra <- array(c(rbind(first.operand[,, 1], extra.rows[, 1]),
                                       rbind(first.operand[,, 2], extra.rows[, 2])),
                                     dim = dim(first.operand) + c(1, 0, 0),
                                     dimnames = new.dimnames)
        first.operand.extra <- first.operand.extra[c(1:7, 9:8), , ]
        first.operand.extra <- CopyAttributes(first.operand.extra, first.operand)
        second.operand.extra <- array(c(rbind(second.operand[,, 1], NA),
                                        rbind(second.operand[,, 2], NA)),
                                      dim = dim(first.operand) + c(1, 0, 0),
                                      dimnames = new.dimnames)
        second.operand.extra <- second.operand.extra[c(1:7, 9:8), , ]
        expected.output <- sanitizeAttributes(base.operator(first.operand.extra, second.operand.extra))
        input <- list(first.operand.extra, second.operand)
        checkOperatorOutput(input, expected.output, verbs.operator, match.elements = "Yes - show unmatched")
        ## Check recycling of lower dim inputs
        denom.matrix <- second.operand[, , 1]
        recycled.second.operand <- array(denom.matrix, dim = c(dim(denom.matrix), 2),
                                         dimnames = c(dimnames(denom.matrix),
                                                      list(dimnames(first.operand)[[3]])))
        input <- list(first.operand, denom.matrix)
        expected.output <- sanitizeAttributes(base.operator(first.operand, recycled.second.operand))
        checkOperatorOutput(input, expected.output, verbs.operator)
    })
}

test_that("Appropriate error thrown if inputs not compatible", {
    expect_error(mathOperator(first = matrix(1:6, nrow = 2),
                              second = matrix(1:6, nrow = 3),
                              function.operator = `/`,
                              function.name = sQuote("Test")),
                 paste(sQuote("Test"), "requires the inputs to have the same",
                       "dimension or partially agreeing dimensions. In this case,",
                       "the inputs are two matrices with 2 rows and 3 columns and",
                       "3 rows and 2 columns respectively. Please ensure the inputs",
                       "have the same or partially agreeing dimensions before attempting",
                       "to recompute", sQuote("Test")))
})

test_that("Warning thrown appropriately", {
    X <- array(1:12, dim = 3:4,
               dimnames = list(c("foo", "bar", "baz"),
                               c("15 - 18", "19 - 24", "25 - 29", "30 - 39")))
    Y <- setNames(1:5, nm = c(colnames(X), "40 - 49"))
    captured.warnings <- capture_warnings(Divide(X, Y, warn = TRUE))
    expect_setequal(captured.warnings,
                    c(capture_warnings(throwWarningAboutRecycling(c(1L, 4L), 3:4)),
                      capture_warnings(throwWarningAboutUnmatched("40 - 49", sQuote("Divide"))),
                      capture_warnings(throwWarningIfTransposedInput(structure(1, "transposed.input" = TRUE),
                                                                     sQuote("Divide")))))
    x <- 1:3
    y <- 0:2
    captured.warnings <- capture_warnings(Divide(x, y, warn = TRUE))
    expect_equal(captured.warnings,
                 capture_warnings(throwWarningAboutDivisionByZeroIfNecessary(list(x, y), c(Inf, 1, 1), sQuote("Divide"))))
    x <- 1
    y <- 0
    captured.warnings <- capture_warnings(Divide(x, y, warn = TRUE))
    expect_equal(captured.warnings,
                 capture_warnings(throwWarningAboutDivisionByZeroIfNecessary(list(x, y), c(Inf), sQuote("Divide"))))
    x <- 1:2
    y <- c(0L, 0L)
    captured.warnings <- capture_warnings(Divide(x, y, warn = TRUE))
    expect_equal(captured.warnings,
                 capture_warnings(throwWarningAboutDivisionByZeroIfNecessary(list(x, y), c(Inf, Inf), sQuote("Divide"))))
    x <- 0
    y <- 0
    captured.warnings <- capture_warnings(Divide(x, y, warn = TRUE))
    expect_equal(captured.warnings,
                 capture_warnings(throwWarningAboutBothElementsZeroInDivisionIfNecessary(list(x, y), NaN, sQuote("Divide"))))
    x <- y <- rep(0, 2L)
    x[3] <- y[3] <- 1
    x <- 0
    y <- 0
    captured.warnings <- capture_warnings(Divide(x, y, warn = TRUE))
    expect_equal(captured.warnings,
                 capture_warnings(throwWarningAboutBothElementsZeroInDivisionIfNecessary(list(x, y), NaN, sQuote("Divide"))))
    x <- y <- rep(0, 2L)
    x[3] <- y[3] <- 1
})
