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
    recycled.denominator <- rep(5, length(numerator))
    expected.output <- numerator/recycled.denominator
    checkDivideOutput(list(numerator, denominator), expected.output, match.rows = "No")
    expect_warning(checkDivideOutput(list(numerator, denominator), expected.output,
                                     match.rows = "No", warn = TRUE),
                   "A scalar element was recycled to a vector with 5 rows")
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
})

test_that("Divide: matrices, tables", {
    # Basic matrix
    numerator <- matrix(1:12, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:4]))
    denominator <- matrix(12:1, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:4]))
    input <- list(numerator, denominator)
    expected.output <- numerator/denominator
    checkDivideOutput(input, expected.output)
    # basic matrix with matching
    shuffled.denominator <- denominator[c(2, 3, 1), c(2, 4, 3, 1)]
    input <- list(numerator, shuffled.denominator)
    checkDivideOutput(input, expected.output)
    # Basic matrix with extra dims
    diff.denominator <- array(20:1, dim = c(4, 5), dimnames = list(letters[1:4], LETTERS[1:5]))
    diff.denominator <- diff.denominator[sample(1:4), sample(1:5)]
    larger.numerator <- rbind(cbind(numerator, E = NA), d = NA)
    expected.output <- larger.numerator/diff.denominator[letters[1:4], LETTERS[1:5]]
    input <- list(numerator, diff.denominator)
    checkDivideOutput(input, expected.output, remove.missing = FALSE)
    # Basic matrix with no matching
    expected.output <- numerator/shuffled.denominator
    input <- list(numerator, shuffled.denominator)
    checkDivideOutput(input, expected.output, match.rows = "No", match.columns = "No")
    # Basic matrix with recycled vector (n x 1) -> (n x m)
    denominator <- 1:3
    recycled.denominator <- array(denominator, dim = 3:4)
    expected.output <- numerator/recycled.denominator
    input <- list(numerator, denominator)
    checkDivideOutput(input, expected.output, match.rows = "No", match.columns = "No")
    ## Check reverse order
    input <- rev(input)
    expected.output <- 1/expected.output
    checkDivideOutput(input, expected.output, match.rows = "No", match.columns = "No")
    # Basic matrix with recycled vector (m x 1) -> (n x m)
    denominator <- 1:4
    recycled.denominator <- array(rep(denominator, each = 3), dim = 3:4)
    expected.output <- numerator/recycled.denominator
    input <- list(numerator, denominator)
    checkDivideOutput(input, expected.output, match.rows = "No", match.columns = "No")
    ## Check reverse order
    input <- rev(input)
    expected.output <- 1/expected.output
    checkDivideOutput(input, expected.output, match.rows = "No", match.columns = "No")
    # Basic matrix with recycled column vector (n x 1) -> (n x m)
    denominator <- matrix(1:3, nrow = 3)
    recycled.denominator <- array(denominator, dim = 3:4)
    expected.output <- numerator/recycled.denominator
    input <- list(numerator, denominator)
    checkDivideOutput(input, expected.output, match.rows = "No", match.columns = "No")
    ## Check reverse order
    input <- rev(input)
    expected.output <- 1/expected.output
    checkDivideOutput(input, expected.output, match.rows = "No", match.columns = "No")
    # Basic matrix with recycled row vector (1 x m) -> (n x m)
    denominator <- matrix(1:4, nrow = 1)
    recycled.denominator <- array(rep(denominator, each = 3), dim = 3:4)
    expected.output <- numerator/recycled.denominator
    input <- list(numerator, denominator)
    checkDivideOutput(input, expected.output, match.rows = "No", match.columns = "No")
    ## Check reverse order
    input <- rev(input)
    expected.output <- 1/expected.output
    checkDivideOutput(input, expected.output, match.rows = "No", match.columns = "No")
    # Basic matrix with scalar denominator
    input <- list(numerator, 3)
    expected.output <- numerator/3
    checkDivideOutput(input, expected.output, match.rows = "No", match.columns = "No")
    ## Check reverse order
    input <- rev(input)
    expected.output <- 1/expected.output
    checkDivideOutput(input, expected.output, match.rows = "No", match.columns = "No")
})

test_that("QTables handled (flattened)", {
    load("numeric.grid.with.multiple.stats.qtable.rda")
    numerator <- numeric.grid.with.multiple.stats.qtable
    denominator <- numerator + array(runif(length(numerator)),
                                     dim = dim(numerator),
                                     dimnames = dimnames(numerator))
    input <- list(numerator, denominator)
    expected.output <- sanitizeAttributes(numerator/denominator)
    checkDivideOutput(input, expected.output)
    ## Row and column removal is working
    table.dimnames <- dimnames(numerator)
    numerator.subset <- numerator[!table.dimnames[[1L]] == "SUM",
                                  !table.dimnames[[2L]] == "SUM",]
    denominator.subset <- denominator[!table.dimnames[[1L]] == "SUM",
                                      !table.dimnames[[2L]] == "SUM",]
    expected.output <- numerator.subset/denominator.subset
    checkDivideOutput(input, expected.output,
                      remove.rows = "SUM", remove.columns = "SUM")
    ## Extra unmatched is ok
    # Hack in an extra row, it is inaccurate (dont have the original data)
    # but is sufficient for testing purposes
    numerator.extra <- numerator
    extra.rows <- colSums(numerator[c("Breakfast", "Lunch", "Dinner"), , ])
    new.dimnames <- dimnames(numerator)
    new.dimnames[[1L]] <- c(new.dimnames[[1L]], "Breakfast + Lunch + Dinner")
    numerator.extra <- array(c(rbind(numerator[,, 1], extra.rows[, 1]),
                               rbind(numerator[,, 2], extra.rows[, 2])),
                             dim = dim(numerator) + c(1, 0, 0),
                             dimnames = new.dimnames)
    numerator.extra <- numerator.extra[c(1:7, 9:8), , ]
    numerator.extra <- CopyAttributes(numerator.extra, numerator)
    denominator.extra <- array(c(rbind(denominator[,, 1], NA),
                                 rbind(denominator[,, 2], NA)),
                               dim = dim(numerator) + c(1, 0, 0),
                               dimnames = new.dimnames)
    denominator.extra <- denominator.extra[c(1:7, 9:8), , ]
    expected.output <- sanitizeAttributes(numerator.extra/denominator.extra)
    input <- list(numerator.extra, denominator)
    checkDivideOutput(input, expected.output, remove.missing = FALSE)
    ## Check recycling of lower dim inputs
    denom.matrix <- denominator[, , 1]
    recycled.denominator <- array(denom.matrix, dim = c(dim(denom.matrix), 2),
                                  dimnames = c(dimnames(denom.matrix),
                                               list(dimnames(numerator)[[3]])))
    input <- list(numerator, denom.matrix)
    expected.output <- sanitizeAttributes(numerator/recycled.denominator)
    checkDivideOutput(input, expected.output, remove.missing = FALSE)
})
