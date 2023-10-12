context("Sum")

load("variable.Text.rda")
load("variable.Binary.rda")
load("variable.Nominal.rda")
load("variable.Numeric.rda")
load("variable.Time.rda")
load("variable.Date.rda")

quoted.function <- sQuote("Sum")

bothElementsMissing <- function(x)
{
    missing <- lapply(x, is.na)
    missing[[1L]] & missing[[2L]]
}

test_that("Variables", {
    text.error <- capture_error(throwErrorInvalidDataForNumericFunc("Text", quoted.function))[["message"]]
    expect_error(Sum(variable.Text), text.error)
    expect_equal(Sum(variable.Binary), 155)
    expect_equal(Sum(variable.Numeric), 12606)
    datetime.error <- capture_error(throwErrorInvalidDataForNumericFunc("Date/Time", quoted.function))[["message"]]
    expect_error(Sum(variable.Date), datetime.error) # Not that means and the like are defined
    expect_error(Sum(variable.Time), datetime.error) # Not that means and the like are defined
    ## Factors
    # With value attributes
    expect_equal(Sum(variable.Nominal), 12606)
    expect_warning(Sum(variable.Nominal), NA)
    # Without value attributes
    factor.conversion.warning <- capture_warnings(flipTransformations::AsNumeric(factor(1:2), binary = FALSE))
    expect_warning(basic.factor <- Sum(factor(1:10)), factor.conversion.warning, fixed = TRUE)
    expect_equal(basic.factor, sum(1:10))
    # Warnings about missing values
    missing.value.warning <- capture_condition(warnAboutMissingValuesIgnored())
    observed.warn <- capture_condition(Sum(variable.Binary, warn = TRUE))
    expect_equal(observed.warn, missing.value.warning)
    # Missing values in calculations
    expect_true(is.na(Sum(variable.Binary, remove.missing = FALSE)))
    expect_true(is.na(Sum(variable.Numeric, remove.missing = FALSE)))
    # Multiple variables
    expected.sum <- as.array(as.vector(variable.Binary + variable.Numeric))
    expect_equivalent(Sum(variable.Binary, variable.Numeric, remove.missing = FALSE, match.elements = "No"),
                      expected.sum)
    inputs <- list(variable.Binary, variable.Numeric)
    both.missing <- bothElementsMissing(inputs)
    expected.inputs <- lapply(inputs, function(x) {
        x[is.na(x) & !both.missing] <- 0
        x
    })
    # Expect no warning about statistics if no missing data is present
    expect_equivalent(Sum(variable.Binary, variable.Numeric, remove.missing = FALSE,
                          warn = TRUE, match.elements = "No"),
                      expected.sum)
    expect_equivalent(Sum(variable.Binary, variable.Numeric, remove.missing = TRUE, match.elements = "No"),
                      as.vector(Reduce(`+`, expected.inputs)))
    # Expect Variable sets to be handled ok
    df1 <- data.frame(x = runif(10), y = runif(10))
    df2 <- data.frame(y = runif(10), z = runif(10))
    expected.out <- as.matrix(data.frame(x = df1[["x"]], y = df1[["y"]] + df2[["y"]], z = df2[["z"]]))
    expect_equivalent(Sum(df1, df2, match.elements = c(rows = "No",
                                                       columns = "Yes - hide unmatched")),
                      expected.out[, "y"])
    expect_equivalent(Sum(df1, df2, match.elements = c(rows = "No",
                                                       columns = "Yes - show unmatched")),
                      expected.out)
})

test_that("Variables with weights, filters (subset), and a combination of the two", {
    subset.missing.out <- !is.na(variable.Numeric)
    expect_equal(Sum(variable.Numeric, subset = subset.missing.out),
                 sum(variable.Numeric, na.rm = TRUE))
    transformed.nom <- flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)
    transformed.nom <- transformed.nom[!is.na(transformed.nom)]
    subset.num <- variable.Numeric[!is.na(variable.Numeric)]
    expect_equivalent(Sum(variable.Numeric, variable.Nominal, subset = subset.missing.out),
                      transformed.nom + subset.num)
    expected.subset.error <- capture_error(throwErrorSubsetOrWeightsWrongSize("subset",
                                                                              length(subset.missing.out),
                                                                              10L))[["message"]]
    expect_error(Sum(variable.Numeric[1:10], subset = subset.missing.out),
                 expected.subset.error)
    expect_error(Sum(variable.Numeric, 1:10, subset = subset.missing.out),
                 paste0(sQuote('Sum'), " requires all input elements to have the same size to be able ",
                        "to apply a filter or weight vector. ",
                        verbs:::determineAppropriateContact()),
                 fixed = TRUE)
    weights <- runif(length(variable.Numeric))
    expect_equal(Sum(variable.Numeric, weights = weights),
                 sum(variable.Numeric * weights, na.rm = TRUE))
    nominal.to.numeric.var <- flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)
    expected.output <- variable.Numeric * weights + nominal.to.numeric.var * weights
    both.missing <- bothElementsMissing(list(variable.Numeric, nominal.to.numeric.var))
    expected.output[is.na(expected.output) & !both.missing] <- 0
    expect_equal(Sum(variable.Numeric, variable.Nominal,
                     weights = weights),
                 sanitizeAttributes(expected.output))
    expected.output <- sum(data.frame(variable.Numeric , nominal.to.numeric.var)  * weights, na.rm = TRUE)
    expect_equal(Sum(data.frame(variable.Numeric, variable.Nominal),
                     weights = weights),
                 expected.output)
    expected.weights.error <- capture_error(throwErrorSubsetOrWeightsWrongSize("weights",
                                                                               10L,
                                                                               length(variable.Numeric)))[["message"]]
    expect_error(Sum(variable.Numeric, weights = weights[1:10]), expected.weights.error)
})


load("table1D.Average.rda")
load("table1D.Percentage.rda")
load("table.1D.MultipleStatistics.rda")
test_that("Table 1D",
{
    expect_equal(Sum(table1D.Percentage, remove.rows = "NET"), 100)
    expect_true(is.nan(Sum(table.1D.MultipleStatistics)))

    captured.warnings <- capture_warnings(Sum(table.1D.MultipleStatistics, warn = TRUE))
    stat.names <- dimnames(table.1D.MultipleStatistics)[[2]]
    expected.warnings <- capture_warnings({
        throwWarningAboutDifferentStatistics(colnames(table.1D.MultipleStatistics),
                                             quoted.function)
        warnAboutOppositeInfinities(TRUE, quoted.function)})
    expect_setequal(captured.warnings, expected.warnings)
    # Removal of row categories in a 1D table
    expect_equal(Sum(table1D.Average, remove.rows = "SUM"), sum(table1D.Average[1:3]))
    expect_equal(Sum(table1D.Average,
                     remove.rows = NULL), sum(table1D.Average[1:4]))

    # Missing values
    z = table1D.Average
    z[2] = NA
    expect_equal(Sum(z, remove.rows = "SUM"), sum(z[1:3], na.rm = TRUE))
    expect_true(is.na(Sum(z, remove.missing = FALSE)))
})

load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")
test_that("Table 2D",
{
    # Expect elements in the table to be summed, ignoring the NET
    expect_equal(Sum(table2D.Percentage, remove.columns = "NET"), 600)
    expect_equal(Sum(table2D.PercentageNaN, remove.columns = "NET", remove.rows = "NET"),
                 sum(table2D.PercentageNaN[-8, -10], na.rm = TRUE))
    # Note that while we represent this as a 3D array, from the user's perspective
    # this is a 2D table, where the third dimension is stacked into the rows.
    expect_equal(Sum(table2D.PercentageAndCount, remove.columns = "NET", remove.rows = "NET"), 2562)

    # Warning for dodgy calculation
    expected.warning <- capture_warnings(throwWarningAboutDifferentStatistics(dimnames(table2D.PercentageAndCount)[[3]],
                                                                              quoted.function))
    expect_warning(Sum(table2D.PercentageAndCount, warn = TRUE),
                   expected.warning, fixed = TRUE)

    # Extra category removed removed
    expect_equal(Sum(table2D.PercentageNaN, remove.rows = c("NET", "None of these"), remove.columns = "NET"),
                 sum(table2D.PercentageNaN[-7:-8, -10], na.rm = TRUE))

    # Missing values
    expect_true(is.na(Sum(table2D.PercentageNaN, remove.missing = FALSE)))
})

.removeAttributes <- function(x)
{
    attr.out <- setdiff(names(attributes(x)),
                        c("dim", "names", "dimnames"))
    for (a in attr.out)
        attr(x, a) <- NULL
    x
}

test_that("Q Tables: Check warning of different statistics thrown or suppressed", {
    # Matching statistics (No warnings)
    # warning already suppressed by default
    inputs <- list(table2D.Percentage, table2D.PercentageNaN[-(7:8), ])
    inputs <- lapply(inputs, .removeAttributes)
    expected.table.out <- Reduce(`+`, inputs)
    dimnames(expected.table.out)[[2L]] <- paste0(colnames(table2D.Percentage), " + ",
                                                 colnames(table2D.PercentageNaN))
    x <- table2D.Percentage
    y <- table2D.PercentageNaN
    row.names(y)[1L] <- "Coca-Cola"
    expect_equal(Sum(x, y,
                     remove.missing = FALSE,
                     remove.rows = c("None of these", "NET"),
                     match.elements = c(rows = "Yes - show unmatched",
                                        columns = "No")),
                 expected.table.out)
    sanitized.inputs <- lapply(inputs, function(x) {
        x[is.na(x)] <- 0
        x
    })
    expected.sanitized.out <- Reduce(`+`, sanitized.inputs)
    dimnames(expected.sanitized.out)[[2L]] <- paste0(colnames(x), " + ", colnames(y))
    expect_equal(Sum(x, y,
                     remove.missing = TRUE,
                     remove.rows = c("None of these", "NET"),
                     match.elements = c(rows = "Yes - hide unmatched",
                                        columns = "No")),
                 expected.sanitized.out)
    expect_equal(Sum(x, y,
                     remove.missing = TRUE,
                     remove.rows = c("None of these", "NET"),
                     match.elements = c(rows = "Yes - show unmatched",
                                         columns = "No")),
                 expected.sanitized.out)
    # No warning even if warn = TRUE
    inputs <- list(table2D.Percentage, table2D.Percentage)
    inputs <- lapply(inputs, .removeAttributes)
    expect_equal(Sum(table2D.Percentage, table2D.Percentage,
                     remove.rows = NULL,
                     remove.columns = NULL,
                     warn = TRUE),
                 Reduce(`+`, inputs))
    # Expect warning if statistic of second table isn't matching
    table.with.non.matching.stat <- table2D.Percentage
    attr(table.with.non.matching.stat, "statistic") <- "Column %"
    diff.stats <- vapply(list(table2D.Percentage, table.with.non.matching.stat),
                         attr, character(1L),
                         which = "statistic")
    diff.stat.warning <- capture_warnings(throwWarningAboutDifferentStatistics(diff.stats, quoted.function))
    expect_warning(computed.sum <- Sum(table2D.Percentage,
                                       table.with.non.matching.stat,
                                       remove.rows = NULL,
                                       remove.columns = NULL,
                                       warn = TRUE),
                   diff.stat.warning,
                   fixed = TRUE)
    inputs <- list(table2D.Percentage, table.with.non.matching.stat)
    inputs <- lapply(inputs, .removeAttributes)
    expected.out <- Reduce(`+`, inputs)
    expect_equal(computed.sum, expected.out)
})

test_that("Works with more than two Q Tables", {
    # If elements are congruent, then works as expected
    expected.out <- 3 * table1D.Average
    expected.out <- .removeAttributes(expected.out)
    expected.out <- array(
        as.vector(3 * table1D.Average),
        dim = length(table1D.Average),
        dimnames = dimnames(table1D.Average)
    )
    expect_equal(
        Sum(table1D.Average, table1D.Average, table1D.Average),
        expected.out
    )
})

test_that("One Q Table with one matrix/array/vector (non-Q Table)", {
    test.qtab <- table1D.Average
    basic.matrix <- matrix(1:length(test.qtab),
                           nrow = NROW(test.qtab), ncol = NCOL(test.qtab),
                           dimnames = dimnames(test.qtab))
    expect_equal(Sum(table1D.Average, basic.matrix),
                 as.matrix(test.qtab) + basic.matrix)
    basic.matrix <- table2D.Percentage[TRUE, TRUE]
    expect_equal(Sum(table2D.Percentage, basic.matrix),
                 array(table2D.Percentage,
                       dim = dim(table2D.Percentage),
                       dimnames = dimnames(table2D.Percentage)) + basic.matrix)
    basic.array <- table1D.Average[TRUE] # Removes attributes
    expected.out <- array(
        as.vector(table1D.Average) + as.vector(basic.array),
        dim = dim(table1D.Average),
        dimnames = dimnames(table1D.Average)
    )
    expect_equal(
        Sum(table1D.Average, basic.array),
        expected.out
    )
})

test_that("Sum matrix and vector",
{
## n x p + n x 1 works
## n x p + 1 x p works
## else error
## respects argument specifying how to match names
    matrix.np  <- matrix(1:24, nrow = 6, dimnames = list(letters[1:6], LETTERS[1:4]))
    matrix.n1  <- matrix(runif(6), nrow = 6, dimnames = list(letters[1:6], NULL))
    matrix.1p  <- matrix(runif(4), nrow = 1, dimnames = list(NULL, letters[1:4]))
    matrix.2n1 <- matrix(runif(12), nrow = 12)
    # n x p + n x 1 (and opposite order)
    expected.output <- matrix.np + array(matrix.n1, dim = dim(matrix.np))
    dimnames(expected.output) <- dimnames(matrix.np)
    expect_equal(Sum(matrix.np, matrix.n1,
                     match.elements = c(rows = "No", columns = "No")),
                 expected.output)
    expect_equal(Sum(matrix.n1, matrix.np,
                     match.elements = c(rows = "No", columns = "No")),
                 expected.output)
    # n x p + 1 x p (and opposite order)
    expected.output <- matrix.np + array(rep(matrix.1p, each = nrow(matrix.np)),
                                         dim = dim(matrix.np))
    dimnames(expected.output)[[2L]] <- paste0(colnames(matrix.np), " + ", colnames(matrix.1p))
    expect_equal(Sum(matrix.np, matrix.1p,
                     match.elements = c(rows = "No", columns = "No")),
                 expected.output)
    dimnames(expected.output)[[2L]] <-  paste0(colnames(matrix.1p), " + ", colnames(matrix.np))
    expect_equal(Sum(matrix.1p, matrix.np,
                     match.elements = c(rows = "No", columns = "No")),
                 expected.output)
    # n x 1 + 1 x p (and opposite order), both get recycled
    expected.output <- array(matrix.n1, dim = dim(matrix.np)) +
                        array(rep(matrix.1p, each = nrow(matrix.np)),
                              dim = dim(matrix.np))
    dimnames(expected.output) <- list(rownames(matrix.n1), colnames(matrix.1p))
    expect_equal(Sum(matrix.n1, matrix.1p,
                     match.elements = c(rows = "No", columns = "No")),
                 expected.output)
    dimnames(expected.output) <- list(rownames(matrix.n1), colnames(matrix.1p))
    expect_equal(Sum(matrix.1p, matrix.n1,
                     match.elements = c(rows = "No", columns = "No")),
                 expected.output)
    # mismatching errors
    dimension.error <- capture_error(throwErrorAboutDimensionMismatch(list(c(6, 4), c(12, 1)), quoted.function))[["message"]]
    expect_error(Sum(matrix.np, matrix.2n1,
                     match.elements = c(rows = "No", columns = "No")),
                 dimension.error)
    dimension.error <- capture_error(throwErrorAboutDimensionMismatch(list(c(12, 1), c(6, 4)), quoted.function))[["message"]]
    expect_error(Sum(matrix.2n1, matrix.np,
                     match.elements = c(rows = "No", columns = "No")),
                 dimension.error)
    dimension.error <- capture_error(throwErrorAboutDimensionMismatch(list(c(6, 1), c(12, 1)), quoted.function))[["message"]]
    expect_error(Sum(matrix.n1, matrix.2n1,
                     match.elements = c(rows = "No", columns = "No")),
                 dimension.error)
    dimension.error <- capture_error(throwErrorAboutDimensionMismatch(list(c(12, 1), c(6, 1)), quoted.function))[["message"]]
    expect_error(Sum(matrix.2n1, matrix.n1,
                     match.elements = c(rows = "No", columns = "No")),
                 dimension.error)
    matrix.1q <- matrix(1:2, nrow = 1)
    dimension.error <- capture_error(throwErrorAboutDimensionMismatch(list(c(1, 4), c(1, 2)), quoted.function))[["message"]]
    expect_error(Sum(matrix.1p, matrix.1q,
                     match.elements = c(rows = "No", columns = "No")),
                 dimension.error)
    dimension.error <- capture_error(throwErrorAboutDimensionMismatch(list(c(1, 2), c(1, 4)), quoted.function))[["message"]]
    expect_error(Sum(matrix.1q, matrix.1p,
                     match.elements = c(rows = "No", columns = "No")),
                 dimension.error)
    matrix.mq <- matrix(1:42, nrow = 7, ncol = 6)
    dimension.error <- capture_error(throwErrorAboutDimensionMismatch(list(c(6, 4), c(7, 6)), quoted.function))[["message"]]
    expect_error(Sum(matrix.np, matrix.mq,
                     match.elements = c(rows = "No", columns = "No")),
                 dimension.error)
    dimension.error <- capture_error(throwErrorAboutDimensionMismatch(list(c(7, 6), c(6, 4)), quoted.function))[["message"]]
    expect_error(Sum(matrix.mq, matrix.np,
                     match.elements = c(rows = "No", columns = "No")),
                 dimension.error)
    dimension.error <- capture_error(throwErrorAboutDimensionMismatch(list(c(7, 6), c(6, 10, 2)), quoted.function))[["message"]]
    expect_error(Sum(matrix.mq, table2D.PercentageAndCount,
                     match.elements = c(rows = "No", columns = "No")),
                 dimension.error)
    # Edge case correctly matches columns
    input1 <- cbind("Question 1" = c(`variant a` = 1, `variant b` = 2))
    input2 <- cbind("Question 2" = c(`Variant A` = 1, `variant B` = 2, c= 3))
    expected.output <- cbind("Question 1" = c(`variant a` = 1, `variant b` = 2, c = NA),
                             "Question 2" = c(`variant a` = 1, `variant b` = 2, c = 3))
    expect_equal(Sum(input1, input2,
                     match.elements = c(rows = "Fuzzy - show unmatched",
                                        columns = "Yes - show unmatched")),
                 expected.output)
    expected.warning <- capture_warnings(throwWarningAboutUnmatched("c", quoted.function))
    sum.output <- expect_warning(Sum(input1, input2,
                                     match.elements = c(rows = "Fuzzy - hide unmatched",
                                                        columns = "Yes - show unmatched"),
                                     warn = TRUE),
                                 expected.warning, fixed = TRUE)
    expect_equal(sum.output,
                 expected.output[-3, ])
    matrix.in <- cbind("Coke" = c(a = 1, b = 2, c = 3),
                       "Pepsi" = c(a = 4, b = 5, c = 6))
    vector.to.recycle <- 1:2
    expected.output <- matrix.in + matrix(1:2, byrow = TRUE, nrow = 3, ncol = 2)
    expect_equal(Sum(matrix.in, vector.to.recycle,
                     match.elements = c(rows = "No", columns = "No")),
                 expected.output)
    # Mix of exact and fuzzy matching
    X <- array(1:20, dim = 5:4,
               dimnames = list(c("Hats", "Mats",  "Dogs", "Hogs", "Don't Know"),
                               c("Foo", "Boo", "hello world", "NET")))
    Y <- array(1:25, dim = c(5L, 5L),
               dimnames = list(c("Hats", "Mets", "Dogs", "Hogs", "Don't Know"),
                               c("Foo",  "Bar", "Boo", "hello world", "NET")))
    expected.sum <- X + Y[, -which(!colnames(Y) %in% colnames(X))]
    expect_equal(Sum(X, Y, match.elements = rep("Fuzzy - hide unmatched", 2L)),
                 expected.sum)
    Y.rand <- Y[sample(1:nrow(Y)), sample(1:ncol(Y))]
    expect_equal(Sum(X, Y, match.elements = rep("Fuzzy - hide unmatched", 2L)),
                 expected.sum)
})

test_that("Summing list objects (e.g. model fits) and other R Outputs",
{ ## extracts ChartData and calls Sum again
    var1 <- variable.Numeric
    var2 <- runif(length(var1))
    correlation.output <- flipStatistics::CorrelationMatrix(data.frame(var1, var2))
    expect_equal(Sum(correlation.output), sum(cor(data.frame(var1, var2), use = "complete.obs")))
})

test_that("A single R Output (e.g. a vanilla matrix or vector) selected",
{ ## tries to calls sum() and returns scalar
    matrix.1 <- matrix(1:24, nrow = 6)
    expect_equal(Sum(matrix.1), sum(matrix.1))
    vector.1 <-1:24
    expect_equal(Sum(vector.1), sum(vector.1))
})

test_that("Incompatible inputs", {
    # If elements are not congruent, then error
    expected.error <- capture_error(throwErrorInputsContainVariablesAndTables(quoted.function))[["message"]]
    expect_error(Sum(table1D.Average, table1D.Average, variable.Binary),
                 expected.error, fixed = TRUE)
    # Attempt to use 3d array
    arr <- array(1:24, dim = 2:4)
    expected.error <- capture_error(throwErrorAboutHigherDimArray(3L, quoted.function))[["message"]]
    expect_error(Sum(arr), expected.error, fixed = TRUE)
})

test_that("Warnings", {
    # Warnings about NaN and adding Inf and -Inf
    single.opp.inf.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE), quoted.function))
    all.opp.inf.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, TRUE), quoted.function))
    opp.table.1D.MultipleStatistics <- table.1D.MultipleStatistics
    opp.table.1D.MultipleStatistics[, 5] <- -opp.table.1D.MultipleStatistics[, 5]
    expect_warning(expect_true(any(is.nan(Sum(table.1D.MultipleStatistics,
                                              opp.table.1D.MultipleStatistics,
                                              warn = TRUE)))),
                   single.opp.inf.warning, fixed = TRUE)
    expect_warning(expect_true(is.nan(Sum(c(Inf, -Inf), warn = TRUE))),
                   all.opp.inf.warning, fixed = TRUE)
    # Warnings about missing values
    missing.value.warning <- capture_condition(warnAboutMissingValuesIgnored())
    observed.warn <- capture_condition(Sum(c(1:3, NA), warn = TRUE))
    expect_equal(observed.warn, missing.value.warning)
    x <- table.1D.MultipleStatistics
    x[1, 1] <- NA
    expect_equal(capture_condition(Sum(x, x, warn = TRUE)), missing.value.warning)
    # Throw warning about filter and/or weights being ignored for Q Tables
    table.subset.warning <- capture_warnings(warnSubsetOrWeightsNotApplicable("a filter", 1L, quoted.function))
    expect_warning(Sum(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)), warn = TRUE),
                   table.subset.warning, fixed = TRUE)
    table.weight.warning <- capture_warnings(warnSubsetOrWeightsNotApplicable("weights", 1L, quoted.function))
    expect_warning(Sum(table1D.Average, weights = runif(10), warn = TRUE),
                   table.weight.warning, fixed = TRUE)
    table.sub.and.weight.warning <- capture_warnings(warnSubsetOrWeightsNotApplicable("a filter or weights", 1L, quoted.function))

    expect_warning(Sum(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)),
                       weights = runif(10), warn = TRUE),
                   table.sub.and.weight.warning, fixed = TRUE)
    # Recycling of inputs that partially agree on dimensions
    ## e.g. an n x p matrix and an n x 1 column vector.
    expected.warning <- capture_warning(throwWarningAboutRecycling(3, c(3, 4)))[["message"]]
    expect_warning(Sum(matrix(1:12, nrow = 3), 1:3, warn = TRUE),
                   expected.warning)
    # Transposing of inputs
    x <- 1
    attr(x, "transposed.input") <- TRUE
    expected.warning <- capture_warnings(throwWarningIfTransposedInput(x, quoted.function))
    input <- array(1:4, dim = c(2, 2), dimnames = list(letters[1:2], LETTERS[1:2]))
    transposed.input <- t(input)
    expect_warning(output <- Sum(input, transposed.input, warn = TRUE), expected.warning, fixed = TRUE)
    expect_equal(output, input + t(transposed.input))
    # Single warning of hiding unmatched
    input <- list(matrix(1:6, nrow = 3, dimnames = list(LETTERS[1:3], letters[1:2])))
    input[[2L]] <- matrix(1:12, nrow = 4, dimnames = list(c(LETTERS[1:3], "Z"), letters[1:3]))
    input[[3L]] <- rbind(input[[1L]], "D" = 1:2)
    expected.warning <- capture_warning(throwWarningAboutUnmatched(c("Z", "c", "D"), sQuote("Sum")))[["message"]]
    expect_warning(Sum(input[[1L]], input[[2L]], input[[3L]], warn = TRUE),
                   expected.warning)
})

test_that("Labels when not matching", {
    # Exact match rows but merge columns with good label
    x <- array(1:2, dim = 2:1, dimnames = list(letters[1:2], "Q1"))
    y <- array(1:2, dim = 2:1, dimnames = list(letters[1:2], "Q2"))
    expected <- array(2 * 1:2, dim = 2:1, dimnames = list(rownames(x), "Q1 + Q2"))
    expect_equal(Sum(x, y,
                     match.elements = c(rows = "Yes - hide unmatched",
                                        columns = "No")),
                 expected)
    # Fuzzy match rows and merge columns with good label
    x <- array(1:2, dim = 2:1, dimnames = list(paste0("variant ", letters[1:2]), "Question 1"))
    y <- array(1:3, dim = c(3, 1), dimnames = list(paste0("Variant ", c("A", "B", "c")), "Question 2"))
    expected <- array(c(2, 4, 3), dim = c(3, 1),
                      dimnames = list(c(paste0("variant ", letters[1:2]), "Variant c"),
                                      "Question 1 + Question 2"))
    expect_equal(Sum(x, y,
                     match.elements = c(rows = "Fuzzy - show unmatched", columns = "No")),
                 expected)
    expected.warning <- capture_warnings(throwWarningAboutUnmatched("Variant c", quoted.function))
    expect_warning(sum.output <- Sum(x, y,
                                     match.elements = c(rows = "Fuzzy - hide unmatched",
                                                        columns = "No"),
                                     warn = TRUE),
                   expected.warning, fixed = TRUE)
    expect_equal(sum.output, expected[-3, , drop = FALSE])

    # Adding a scalar
    x <- array(1:3, dim = c(3, 1), dimnames = list(letters[1:3], "Coke"))
    y <- 2
    expected <- array(3:5, dim = c(3, 1), dimnames = list(letters[1:3], "Coke"))
    expect_equal(Sum(x, y,
                     match.elements = c(rows = "No", columns = "No")),
                 expected)
    expected <- array(3:5, dim = c(3, 1), dimnames = list(letters[1:3], "Coke"))
    expect_equal(Sum(y, x,
                     match.elements = c(rows = "No", columns = "No")),
                 expected)
    # Adding a row vector
    x <- array(1:6, dim = c(3, 2), dimnames = list(letters[1:3], c("Coke", "Pepsi")))
    y <- array(1:2, dim = c(1, 2))
    expected <- array(c(2:4, 6:8), dim = c(3, 2), dimnames = list(letters[1:3], c("Coke", "Pepsi")))
    expect_equal(Sum(x, y,
                     match.elements = c(rows = "No", columns = "No")),
                 expected)
    # Adding a column vector
    x <- array(1:6, dim = c(3, 2), dimnames = list(letters[1:3], c("Coke", "Pepsi")))
    y <- array(1:2, dim = c(1, 2))
    expected <- array(c(2:4, 6:8), dim = c(3, 2), dimnames = list(letters[1:3], c("Coke", "Pepsi")))
    expect_equal(Sum(x, y, match.elements = c(rows = "No", columns = "No")),
                 expected)
})

test_that("Correct name resolving for matching", {
    unnamed.array.scalar <- array(1, dim = 1)
    named.array.scalar <- array(1, dim = 1, dimnames = list("a"))
    unnamed.1d.array.vector <- array(1:3, dim = 3)
    named.1d.array.vector <- array(1:3, dim = 3, dimnames = list(letters[1:3]))
    unnamed.matrix <- matrix(1:6, nrow = 3)
    matrix.with.named.rows <- matrix(1:6, nrow = 3,
                                     dimnames = list(letters[1:3], NULL))
    matrix.with.named.columns <- matrix(1:6, nrow = 3,
                                        dimnames = list(NULL, letters[1:2]))
    matrix.with.named.rows.and.columns <- matrix(1:6, nrow = 3,
                                                 dimnames = list(letters[1:3], letters[1:2]))
    test.cases <- list(`unnamed array scalar` = unnamed.array.scalar,
                       `named array scalar` = named.array.scalar,
                       `unnamed 1d array vector` = unnamed.1d.array.vector,
                       `named 1d array vector` = named.1d.array.vector,
                       `unnamed matrix` = unnamed.matrix,
                       `matrix with named rows` = matrix.with.named.rows,
                       `matrix with named columns` = matrix.with.named.columns,
                       `matrix with named rows and columns` = matrix.with.named.rows.and.columns)
    expected.names <- list(`unnamed array scalar` = list(NULL, NULL),
                           `named array scalar` = list("a", NULL),
                           `unnamed 1d array vector` = list(NULL, NULL),
                           `named 1d array vector` = list(letters[1:3], NULL),
                           `unnamed matrix` = list(NULL, NULL),
                           `matrix with named rows` = list(letters[1:3], NULL),
                           `matrix with named columns` = list(NULL, letters[1:2]),
                           `matrix with named rows and columns` = list(letters[1:3], letters[1:2]))
    expect_identical(lapply(test.cases, getDimensionNamesOfInputs), expected.names)
    expected.resolved.dimnames <- list(`unnamed array scalar` = logical(2L),
                                       `named array scalar` = c(TRUE, FALSE),
                                       `unnamed 1d array vector` = logical(2L),
                                       `named 1d array vector` = c(TRUE, FALSE),
                                       `unnamed matrix` = c(FALSE, FALSE),
                                       `matrix with named rows` = c(TRUE, FALSE),
                                       `matrix with named columns` = c(FALSE, TRUE),
                                       `matrix with named rows and columns` = rep(TRUE, 2L))
    expect_identical(lapply(expected.names, dimnamesExist), expected.resolved.dimnames)
})

test_that("Automatic Matching", {
    X <- matrix(1:6, nrow = 3, dimnames = list(1:3, 1:2))
    Y <- matrix(6:1, nrow = 3, dimnames = list(1:3, 1:2))
    expect_error(auto.sum <- Sum(X, Y, match.elements = "Yes"), NA)
    expect_equal(auto.sum,
                 Sum(X, Y, match.elements = c(rows = "No", columns = "No")))
    expect_equal(auto.sum, X + Y)
    expect_equal(auto.sum, Sum(X, Y, match.elements = "No"))
    X.fuzzy <- X
    Y.fuzzy <- Y
    dimnames(X.fuzzy) <- list(paste0("variant ", letters[1:3]), paste0("Question ", letters[1:2]))
    dimnames(Y.fuzzy) <- list(paste0("Variant ", LETTERS[sample(1:3)]), paste0("Question ", LETTERS[2:1]))
    expect_equal(Sum(X.fuzzy, Y.fuzzy),
                 X.fuzzy + Y.fuzzy[paste0("Variant ", LETTERS[1:3]), paste0("Question ", LETTERS[1:2])])
    tX <- t(X)
    expect_equal(Sum(X, tX), 2 * X)
    colnames(tX) <- letters[1:3]
    vector <- setNames(runif(3L), nm = letters[1:3])
    expect_equal(Sum(tX, vector),
                 tX + array(rep(vector, each = nrow(tX)), dim = dim(tX)))
    qtable.3d <- table2D.PercentageAndCount
    tqtable.3d <- aperm(table2D.PercentageAndCount, c(2:1, 3L))
    tqtable.3d <- CopyAttributes(tqtable.3d, qtable.3d)
    expected.table <- array(2* qtable.3d, dim = dim(qtable.3d), dimnames = dimnames(qtable.3d))
    expect_equal(Sum(qtable.3d, tqtable.3d), expected.table)

    captured.error <- capture_error(throwErrorNoMatchingElementsFound(sQuote("Sum")))[["message"]]
    expect_error(Sum(X, c(a = 1)), captured.error)
    # Inputs that have a missing value for one of their names
    X <- array(1:12, dim = 3:4, dimnames = list(letters[1:3], LETTERS[1:4]))
    y <- setNames(1:3, nm = c(letters[1:2], NA_character_))
    expected.warning <- capture_warnings(throwWarningAboutMissingNames(sQuote("Sum")))
    expect_warning(sum.output <- Sum(X, y, warn = TRUE), expected.warning, fixed = TRUE)
    expect_equal(sum.output, X[1:2, ] + array(y[1:2], dim = c(2, 4)))
    expect_equal(Sum(X, setNames(1:3, NA)), X + 1:3)
    x <- setNames(1:3, letters[1:3])
    y <- setNames(1:3, rev(letters)[1:3])
    expect_equal(Sum(x, y, match.elements = "Yes - show unmatched"),
                 c(x, y))
    # Some inputs with same names on entire dimension
    X <- array(1:12, dim = 3:4, dimnames = list(letters[1:3],  rep("same", 4)))
    Y <- array(1:12, dim = 3:4, dimnames = list(letters[1:3],  rep("same", 4)))
    expect_equal(Sum(X, Y), X + Y)
    # Some inputs with some duplicated names
    X <- array(1:12, dim = 3:4, dimnames = list(letters[1:3],  c(rep("same", 2), LETTERS[1:2])))
    Y <- array(1:12, dim = 3:4, dimnames = list(letters[1:3],  c(LETTERS[1:2], rep("same", 2))))
    expected.error <- capture_error(throwErrorAboutDuplicatedNamesWhenMatching(list(rows = NULL,
                                                                                    columns = "same"),
                                                                               function.name = quoted.function))
    expect_error(Sum(X, Y), expected.error[["message"]], fixed = TRUE)
    expect_error(Sum(X, Y, match.elements = c("Yes", "Yes")), expected.error[["message"]], fixed = TRUE)
    expect_error(Sum(X, Y, match.elements = c("No", "Yes")), expected.error[["message"]], fixed = TRUE)
    X <- array(1:12, dim = 3:4, dimnames = list(c(rep("foo", 2), "A"),  c(rep("same", 2), LETTERS[1:2])))
    Y <- array(1:12, dim = 3:4, dimnames = list(letters[1:3],  c(LETTERS[1:2], rep("same", 2))))
    expected.error <- capture_error(throwErrorAboutDuplicatedNamesWhenMatching(list(rows = "foo",
                                                                                    columns = "same"),
                                                                               function.name = quoted.function))
    expect_error(Sum(X, Y), expected.error[["message"]], fixed = TRUE)
    X <- array(1:12, dim = 3:4, dimnames = list(c(rep("foo", 2), "A"),  rep(c("bar", "baz"), c(2, 2))))
    Y <- array(1:12, dim = 3:4, dimnames = list(letters[1:3],  c(LETTERS[1:2], rep("same", 2))))
    expected.error <- capture_error(throwErrorAboutDuplicatedNamesWhenMatching(list(rows = "foo",
                                                                                    columns = c("bar", "baz", "same")),
                                                                               function.name = quoted.function))
    expect_error(Sum(X, Y), expected.error[["message"]], fixed = TRUE)

    X <- array(1:12, dim = 3:4, dimnames = list(c("a", "b", NA),  rep(c("bar", "baz"), c(2, 2))))
    Y <- array(1:12, dim = 3:4, dimnames = list(letters[1:3],  c(LETTERS[1:2], rep("same", 2))))
    expected.warning <- capture_warnings(throwWarningAboutMissingNames(quoted.function))
    observed.warning <- capture_warnings(sum.out <- Sum(X, Y, match.elements = c("Yes", "No")))
    expect_setequal(observed.warning, expected.warning)
    col.names <- apply(vapply(list(X, Y), colNames, character(4L)), 1L, paste0, collapse = " + ")
    expect_equal(sum.out, array(X[1:2, ] + Y[1:2, ], dim = c(2L, 4L),
                                dimnames = list(letters[1:2], col.names)))
    X <- t(X)
    Y <- t(Y)
    observed.warning <- capture_warnings(sum.out <- Sum(X, Y, match.elements = c("No", "Yes")))
    expect_setequal(observed.warning, capture_warnings(throwWarningAboutMissingNames(quoted.function)))
    row.names <- apply(vapply(list(X, Y), rowNames, character(4L)), 1L, paste0, collapse = " + ")
    expect_equal(sum.out, array(X[, 1:2] + Y[, 1:2], dim = c(4L, 2L),
                                dimnames = list(row.names, letters[1:2])))
    row.names(X) <- row.names(Y) <- c(NA, LETTERS[1:3])
    observed.warning <- capture_warnings(sum.out <- Sum(X, Y, match.elements = c("Yes", "Yes")))
    expected.warning <- capture_warnings(throwWarningAboutMissingNames(quoted.function))
    expect_setequal(observed.warning, expected.warning)
})

test_that("Tables with spans correctly flattened",{
    table.with.row.spans <- array(1:18, dim = c(6L, 3L),
                                  dimnames = list(rep(c("Male", "Female", "NET"), 2L),
                                                  c("Low", "Medium", "High")))
    attr(table.with.row.spans, "questions") <- c("BANNER", "Something")
    row.spans <- data.frame(rep(letters[1:2], each = 3),
                            rownames(table.with.row.spans))
    names(row.spans) <- NULL
    col.spans <- data.frame(colnames(table.with.row.spans))
    names(col.spans) <- NULL
    attr(table.with.row.spans, "span") <- list(rows    = row.spans,
                                               columns = col.spans)
    expected.row.array <- table.with.row.spans
    dimnames(expected.row.array)[[1L]] <- apply(row.spans, 1L, paste0, collapse = " - ")
    expected.col.array <- t(table.with.row.spans)
    dimnames(expected.col.array)[[2L]] <- apply(row.spans, 1L, paste0, collapse = " - ")
    table.with.col.spans <- t(table.with.row.spans)
    attr(table.with.col.spans, "span") <- rev(attr(table.with.row.spans, "span"))
    attr(expected.col.array, "span") <- rev(attr(table.with.row.spans, "span"))
    names(attr(table.with.col.spans, "span")) <- names(attr(expected.col.array, "span")) <- c("rows", "columns")
    inputs <- list(table.with.row.spans, table.with.col.spans)
    expected.output <- list(expected.row.array, expected.col.array)
    attr(expected.output[[1L]], "has.row.spans") <- TRUE
    attr(expected.output[[1L]], "has.col.spans") <- FALSE
    attr(expected.output[[2L]], "has.row.spans") <- FALSE
    attr(expected.output[[2L]], "has.col.spans") <- TRUE
    expect_equal(checkInputsAtMost2DOrQTable(inputs), expected.output)
})

test_that("Q Statistic names still identified", {
    table.with.q.names <- array(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 100, 100, 69.7, 30.3,
                                  0, 0, 0, 0, 0, 0, 0, 0, 100, 35.8, 33.5, 2.4, 4.9, 3.5, 5, 5.5,
                                  5, 4.5, 0, 100, 0, 0, 12.4, 18.6, 12.2, 11.8, 14.3, 17.2, 13.5,
                                  0, 100, 36.9, 26.6, 3.7, 6.3, 4.3, 5.1, 5.9, 6.1, 5.1, 0, 100,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 768, 334, 0, 0, 0, 0, 0, 0,
                                  0, 0, 1102, 1018, 952, 68, 139, 100, 141, 157, 141, 129, 0, 2845,
                                  0, 0, 111, 166, 109, 105, 128, 153, 120, 0, 892, 1786, 1286,
                                  179, 305, 209, 246, 285, 294, 249, 1, 4840),
                                dim = c(11L, 5L, 2L),
                                dimnames = list(c("0", "1", "15-18", "19 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "64", "NET"),
                                                c("-44", "0", "Male", "Female", "NET"),
                                                c("Column %", "n")))
    attr(table.with.q.names, "questions") <- c("S1 Age", "S2 Gender")
    attr(table.with.q.names, "questiontypes") <- rep("PickOne", 2L)
    attr(table.with.q.names, "name") <- "some.table"
    table.with.displayr.names <- table.with.q.names
    dimnames(table.with.displayr.names)[[3L]] <- c("Column %", "Sample size")
    expect_warning(outputs <- lapply(list(table.with.q.names, table.with.displayr.names), SumEachRow), NA)
    expect_true(Reduce(identical, lapply(outputs, dim)))
})

test_that("Handling of NAs", {
    expect_equal(Sum(NA, remove.missing = TRUE), NA)
    expect_equal(Sum(NA, remove.missing = FALSE), NA)
    expect_equal(Sum(c(1:3, NA)), sum(1:3))
    expect_equal(Sum(rep(NA, 10L)), NA)
})

test_that("SumEmptyHandling",
{
    expect_equal(SumEmptyHandling(NULL, remove.missing = TRUE,
                                  return.zero.if.null = TRUE), 0)
    expect_equal(SumEmptyHandling(NULL, remove.missing = TRUE,
                                  return.zero.if.null = FALSE), NA)
    expect_equal(SumEmptyHandling(NA, remove.missing = TRUE,
                                  return.zero.if.all.NA = TRUE), 0)
    expect_equal(SumEmptyHandling(NA, remove.missing = TRUE,
                                  return.zero.if.all.NA = FALSE), NA)
})

test_that("Warnings muffled", {
    # Show the missing value warning usually
    input.array <- setNames(c(NA, 1:2), LETTERS[1:3])
    expected.cond <- capture_condition(warnAboutMissingValuesIgnored())
    observed.cond <- capture_condition(Sum(input.array, warn = TRUE))
    expect_equal(observed.cond, expected.cond)
    # Not show the missing value warning when not logical input given
    expect_equal(Sum(input.array, warn = "Foo"), sum(input.array, na.rm = TRUE))
})

test_that("NULL handling", {
    expect_equal(Sum(), NA)
    expect_equal(Sum(1:5, NULL), 1:5)
})

test_that("Variable Set + Variable matching", {
    # No matching but recyling if input 1 is df and input 2 is variable (vector) but has matching variable label
    df1 <- structure(data.frame(A = structure(runif(10), label = "A"),
                                B = structure(runif(10), label = "B"),
                                C = structure(runif(10), label = "C")),
                     questiontype = "NumberMulti", dataset = "fakedata", question = "A + B")
    var1 <- structure(runif(10), label = "B", name = "B", questiontype = "Number", dataset = "fakedata")
    stripped.var <- var1
    attr(stripped.var, "questiontype") <- attr(stripped.var, "dataset") <- NULL
    recyc.df2 <- replicate(3L, stripped.var, simplify = FALSE)
    recyc.df2 <- as.data.frame(recyc.df2)
    colnames(recyc.df2) <- LETTERS[1:3]
    expected.sum <- df1 + recyc.df2
    attr(expected.sum, "row.names") <- as.character(seq_len(nrow(df1)))
    attr(expected.sum[["A"]], "name") <- "A"
    attr(expected.sum[["C"]], "name") <- "C"
    expect_equal(Sum(df1, var1), expected.sum)
    expect_equal(Sum(df1, var1), Sum(df1, var1, match.elements = "No"))
    # Matching occurs without issue if variable coerced to data.frame
    var1.as.df <- singleVariableAsDataFrame(var1)
    input <- list(df1, var1)
    expected.output <- list(df1, var1.as.df)
    expect_equal(lapply(input, singleVariableAsDataFrame), expected.output)
    # Remove label and check name used instead.
    var1.no.label <- var1
    attr(var1.no.label, "label") <- NULL
    expect_equal(colnames(singleVariableAsDataFrame(var1.no.label)), attr(var1.no.label, "name"))
    expected.sum <- df1
    expected.sum[[2]] <- expected.sum[[2]] + var1
    attr(expected.sum, "row.names") <- as.character(seq_len(nrow(df1)))
    hidden.expected.sum <- expected.sum[2]
    expect_equal(Sum(df1, var1.as.df), hidden.expected.sum)
    attr(expected.sum[["A"]], "name") <- "A"
    attr(expected.sum[["C"]], "name") <- "C"
    attr(expected.sum[["B"]], "questiontype") <- attr(expected.sum[["B"]], "dataset") <- NULL
    attr(expected.sum, "questiontype") <- attr(expected.sum, "dataset") <- attr(expected.sum, "question") <- NULL
    expect_equal(Sum(df1, var1.as.df, match.elements = "Yes - show unmatched"), expected.sum)
})
