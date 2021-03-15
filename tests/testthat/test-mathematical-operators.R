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

for (fun.ind in seq_along(verbs.operator.functions))
{
    base.operator  <- base.operator.functions[[fun.ind]]
    verbs.operator <- verbs.operator.functions[[fun.ind]]
    fun.name <- verb.function.names[fun.ind]
    test_that(paste0(fun.name, ": Scalar inputs"), {
        # Unnamed scalars
        inputs <- replicate(5, runif(2, min = -1e3, max = 1e3), simplify = FALSE)
        expected.outputs <- lapply(inputs, expectedOutput, fun.operator = base.operator)
        # Basic unnamed usage tests
        invisible(mapply(checkOperatorOutput, inputs, expected.outputs,
                         MoreArgs = list(test.function = verbs.operator)))
        # Two named scalars
        rand.in <- runif(2)
        input <- list(c(a = rand.in[1]), c(b = rand.in[2]))
        # When matching is requested
        expected.output <- c(a = base.operator(rand.in[1], 0),
                             b = base.operator(0,          rand.in[2]))
        checkOperatorOutput(input, expected.output, verbs.operator)
        params <- list(remove.missing = TRUE)
        expected.output <- c(a = base.operator(rand.in[1], 0),
                             b = base.operator(0,          rand.in[2]))
        checkOperatorOutput(input, expected.output, verbs.operator, remove.missing = TRUE)
        # No matching
        expected.output <- c(a = base.operator(rand.in[1], rand.in[2]))
        checkOperatorOutput(input, expected.output, verbs.operator, match.rows = "No")
    })
}

for (fun.ind in seq_along(verbs.operator.functions))
{
    base.operator  <- base.operator.functions[[fun.ind]]
    verbs.operator <- verbs.operator.functions[[fun.ind]]
    fun.name <- verb.function.names[fun.ind]
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
        first.operand  <- c(a = 10, b = 9, c = 8, d = 7, e = 6)
        second.operand <- c(c = 1,  e = 2, b = 3, a = 4, d = 5)
        expected.output <- expectedOutput(list(first.operand, second.operand[names(first.operand)]),
                                          fun.operator = base.operator)
        checkOperatorOutput(list(first.operand, second.operand), expected.output, verbs.operator)
        # Named vector, different size
        first.operand  <- c(a = 10, b = 9, c = 8, d = 7, e = 6, f = 5)
        second.operand <- c(c = 1,  e = 2, b = 3, a = 4, d = 5)
        matched.second.operand <- c(second.operand[names(first.operand)[1:5]], f = 0)
        expected.output <- base.operator(first.operand, matched.second.operand)
        checkOperatorOutput(list(first.operand, second.operand), expected.output, verbs.operator)
        # Same named vector, different size. Sign switched and keep missing
        first.operand  <- - c(a = 10, b = 9, c = 8, d = 7, e = 6, f = 5)
        second.operand <-   c(c = 1,  e = 2, b = 3, a = 4, d = 5)
        matched.second.operand <- c(second.operand[names(first.operand)[1:5]], f = NA)
        expected.output <- base.operator(first.operand, matched.second.operand)
        checkOperatorOutput(list(first.operand, second.operand), expected.output, verbs.operator,
                            remove.missing = FALSE)
        # Named vector, but jumbled order in first.operand, second.operand longer
        first.operand  <- c(c = 1,  a = 2, b = 3, e = 4, d = 5)
        second.operand <- c(a = 10, b = 9, c = 8, d = 7, e = 6, f = 5)
        matched.second.operand <- c(second.operand[names(first.operand)], f = 5)
        matched.first.operand   <- c(first.operand, f = 0)
        expected.output <- base.operator(matched.first.operand, matched.second.operand)
        checkOperatorOutput(list(first.operand, second.operand), expected.output, verbs.operator)
        # first.operand vector, second.operand scalar
        first.operand <- c(a = 10, b = 9, c = 8, d = 7, e = 6)
        second.operand <- 5
        recycled.second.operand <- rep(5, length(first.operand))
        expected.output <- base.operator(first.operand, recycled.second.operand)
        checkOperatorOutput(list(first.operand, second.operand), expected.output, verbs.operator,
                            match.rows = "No")
        expect_warning(checkOperatorOutput(list(first.operand, second.operand), expected.output, verbs.operator,
                                         match.rows = "No", warn = TRUE),
                       "A scalar element was recycled to a vector with 5 rows")
    })
}

for (fun.ind in seq_along(verbs.operator.functions))
{
    base.operator  <- base.operator.functions[[fun.ind]]
    verbs.operator <- verbs.operator.functions[[fun.ind]]
    fun.name <- verb.function.names[fun.ind]
    test_that(paste0(fun.name, ": Variables"), {
        load("variable.Income.cat.rda")
        load("variable.Numeric.rda")
        # Categorical variable mapped to numeric using value attributes
        first.operand <- variable.Income.cat
        second.operand <- variable.Numeric
        input <- list(first.operand, second.operand)
        mapped.first.operand <- AsNumeric(first.operand, binary = FALSE)
        expected.output <- as.vector(base.operator(mapped.first.operand, second.operand))
        checkOperatorOutput(input, expected.output, verbs.operator, remove.missing = FALSE)
        # Correct mapping when missing values removed
        mapped.inputs <- list(mapped.first.operand, second.operand)
        mapped.inputs.wo.missing <- lapply(list(mapped.first.operand, second.operand), removeMissing)
        expected.output <- as.vector(base.operator(mapped.inputs.wo.missing[[1L]], mapped.inputs.wo.missing[[2L]]))
        checkOperatorOutput(input, expected.output, verbs.operator)
        # Check filters and weights
        ## Check filter, using gender
        load("variable.Gender.cat.rda")
        males <- variable.Gender.cat == "Male"
        filtered.input <- lapply(mapped.inputs, function(x) x[males])
        expected.output <- base.operator(filtered.input[[1L]], filtered.input[[2L]])
        checkOperatorOutput(input, expected.output, verbs.operator,
                            subset = males, remove.missing = FALSE)
        filtered.input.wo.missing <- lapply(filtered.input, removeMissing)
        expected.output <- base.operator(filtered.input.wo.missing[[1L]], filtered.input.wo.missing[[2L]])
        checkOperatorOutput(list(first.operand, second.operand), expected.output, verbs.operator,
                            subset = males)
    })
}

for (fun.ind in seq_along(verbs.operator.functions))
{
    base.operator  <- base.operator.functions[[fun.ind]]
    verbs.operator <- verbs.operator.functions[[fun.ind]]
    fun.name <- verb.function.names[fun.ind]
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
        expected.output <- base.operator(larger.first.operand, diff.second.operand[letters[1:4], LETTERS[1:5]])
        input <- list(first.operand, diff.second.operand)
        checkOperatorOutput(input, expected.output, verbs.operator, remove.missing = FALSE)
        # Basic matrix with no matching
        expected.output <- base.operator(first.operand, shuffled.second.operand)
        input <- list(first.operand, shuffled.second.operand)
        checkOperatorOutput(input, expected.output, verbs.operator, match.rows = "No", match.columns = "No")
        # Basic matrix with recycled vector (n x 1) -> (n x m)
        second.operand <- 1:3
        recycled.second.operand <- array(second.operand, dim = 3:4)
        expected.output <- base.operator(first.operand, recycled.second.operand)
        input <- list(first.operand, second.operand)
        checkOperatorOutput(input, expected.output, verbs.operator, match.rows = "No", match.columns = "No")
        ## Check reverse order
        expected.output <- base.operator(recycled.second.operand, first.operand)
        input <- rev(input)
        checkOperatorOutput(input, expected.output, verbs.operator, match.rows = "No", match.columns = "No")
        # Basic matrix with recycled vector (m x 1) -> (n x m)
        second.operand <- 1:4
        recycled.second.operand <- array(rep(second.operand, each = 3), dim = 3:4)
        expected.output <- base.operator(first.operand, recycled.second.operand)
        input <- list(first.operand, second.operand)
        checkOperatorOutput(input, expected.output, verbs.operator, match.rows = "No", match.columns = "No")
        ## Check reverse order
        expected.output <- base.operator(recycled.second.operand, first.operand)
        input <- rev(input)
        checkOperatorOutput(input, expected.output, verbs.operator, match.rows = "No", match.columns = "No")
        # Basic matrix with recycled column vector (n x 1) -> (n x m)
        second.operand <- matrix(1:3, nrow = 3)
        recycled.second.operand <- array(second.operand, dim = 3:4)
        expected.output <- base.operator(first.operand, recycled.second.operand)
        input <- list(first.operand, second.operand)
        checkOperatorOutput(input, expected.output, verbs.operator, match.rows = "No", match.columns = "No")
        ## Check reverse order
        expected.output <- base.operator(recycled.second.operand, first.operand)
        input <- rev(input)
        checkOperatorOutput(input, expected.output, verbs.operator, match.rows = "No", match.columns = "No")
        # Basic matrix with recycled row vector (1 x m) -> (n x m)
        second.operand <- matrix(1:4, nrow = 1)
        recycled.second.operand <- array(rep(second.operand, each = 3), dim = 3:4)
        expected.output <- base.operator(first.operand, recycled.second.operand)
        input <- list(first.operand, second.operand)
        checkOperatorOutput(input, expected.output, verbs.operator, match.rows = "No", match.columns = "No")
        ## Check reverse order
        expected.output <- base.operator(recycled.second.operand, first.operand)
        input <- rev(input)
        checkOperatorOutput(input, expected.output, verbs.operator, match.rows = "No", match.columns = "No")
        # Basic matrix with scalar second.operand
        input <- list(first.operand, 3)
        expected.output <- base.operator(first.operand, 3)
        checkOperatorOutput(input, expected.output, verbs.operator, match.rows = "No", match.columns = "No")
        ## Check reverse order
        expected.output <- base.operator(3, first.operand)
        input <- rev(input)
        checkOperatorOutput(input, expected.output, verbs.operator, match.rows = "No", match.columns = "No")
    })
}

for (fun.ind in seq_along(verbs.operator.functions))
{
    base.operator  <- base.operator.functions[[fun.ind]]
    verbs.operator <- verbs.operator.functions[[fun.ind]]
    fun.name <- verb.function.names[fun.ind]
    test_that(paste0(fun.name, ": QTables handled (flattened)"), {
        load("numeric.grid.with.multiple.stats.qtable.rda")
        first.operand <- numeric.grid.with.multiple.stats.qtable
        second.operand <- first.operand + array(runif(length(first.operand)),
                                         dim = dim(first.operand),
                                         dimnames = dimnames(first.operand))
        input <- list(first.operand, second.operand)
        expected.output <- sanitizeAttributes(base.operator(first.operand, second.operand))
        if (fun.name %in% c("Multiply", "Subtract"))
            expected.output[is.na(expected.output)] <- 0
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
        checkOperatorOutput(input, expected.output, verbs.operator,
                            remove.missing = FALSE)
        ## Check recycling of lower dim inputs
        denom.matrix <- second.operand[, , 1]
        recycled.second.operand <- array(denom.matrix, dim = c(dim(denom.matrix), 2),
                                      dimnames = c(dimnames(denom.matrix),
                                                   list(dimnames(first.operand)[[3]])))
        input <- list(first.operand, denom.matrix)
        expected.output <- sanitizeAttributes(base.operator(first.operand, recycled.second.operand))
        checkOperatorOutput(input, expected.output, verbs.operator,
                            remove.missing = FALSE)
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