context("Sum")

load("variable.Text.rda")
load("variable.Binary.rda")
load("variable.Nominal.rda")
load("variable.Numeric.rda")
load("variable.Time.rda")
load("variable.Date.rda")

test_that("Variables", {
    expect_error(Sum(variable.Text),
                 paste0("Text data has been supplied but ", sQuote("Sum"), " requires numeric data."))
    expect_equal(Sum(variable.Binary), 155)
    expect_equal(Sum(variable.Numeric), 12606)
    expect_error(Sum(variable.Date), paste0("Date/Time data has been supplied but ", sQuote("Sum"), " requires numeric data.")) # Not that means and the like are defined
    expect_error(Sum(variable.Time), paste0("Date/Time data has been supplied but ", sQuote("Sum"), " requires numeric data.")) # Not that means and the like are defined
    ## Factors
    # With value attributes
    expect_equal(Sum(variable.Nominal), 12606)
    expect_warning(Sum(variable.Nominal), NA)
    # Without value attributes
    expect_warning(basic.factor <- Sum(factor(1:10)),
                   "Data has been automatically converted to numeric")
    expect_equal(basic.factor, sum(1:10))
    # Warnings about missing values
    expect_warning(Sum(variable.Binary, warn = TRUE),
                   "Missing values have been ignored in calculation.")
    # Missing values in calculations
    expect_true(is.na(Sum(variable.Binary, remove.missing = FALSE)))
    expect_true(is.na(Sum(variable.Numeric, remove.missing = FALSE)))
    # Multiple variables
    expected.sum <- as.array(as.vector(variable.Binary + variable.Numeric))
    expect_equivalent(Sum(variable.Binary, variable.Numeric, remove.missing = FALSE),
                      expected.sum)
    expected.inputs <- lapply(list(variable.Binary, variable.Numeric), function(x) {
        x[is.na(x)] <- 0
        x
    })
    # Expect no warning about statistics if no missing data is present
    expect_equivalent(Sum(variable.Binary, variable.Numeric, remove.missing = FALSE, warn = TRUE),
                      expected.sum)
    expect_equivalent(Sum(variable.Binary, variable.Numeric, remove.missing = TRUE),
                      as.vector(Reduce(`+`, expected.inputs)))
    # Expect Variable sets to be handled ok
    df1 <- data.frame(x = runif(10), y = runif(10))
    df2 <- data.frame(y = runif(10), z = runif(10))
    expected.out <- as.matrix(data.frame(x = df1[["x"]], y = df1[["y"]] + df2[["y"]], z = df2[["z"]]))
    expect_equivalent(Sum(df1, df2), expected.out)
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
    expect_error(Sum(variable.Numeric[1:10], subset = subset.missing.out),
                 paste0("The subset vector has length 327. However, it needs to ",
                        "have length 10 to match the number of cases in the supplied input data."))
    expect_error(Sum(variable.Numeric, 1:10, subset = subset.missing.out),
                 paste0(sQuote('Sum'), " requires all input elements to have the same size to be able ",
                        "to apply a filter or weight vector. ",
                        verbs:::determineAppropriateContact()),
                 fixed = TRUE)
    weights <- runif(length(variable.Numeric))
    expect_equal(Sum(variable.Numeric, weights = weights),
                 sum(variable.Numeric * weights, na.rm = TRUE))
    nominal.to.numeric.var <- flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)
    expect_equivalent(Sum(variable.Numeric, variable.Nominal,
                          weights = weights,
                          subset = subset.missing.out),
                      ((variable.Numeric + nominal.to.numeric.var) * weights)[subset.missing.out])
    expect_error(Sum(variable.Numeric, weights = weights[1:10]),
                 paste0("The weights vector has length 10. However, it needs to ",
                        "have length 327 to match the number of cases in the supplied input data."))
})


load("table1D.Average.rda")
load("table1D.Percentage.rda")
load("table.1D.MultipleStatistics.rda")
test_that("Table 1D",
{
    expect_equal(Sum(table1D.Percentage, remove.rows = "NET"), 100)
    expect_true(is.na(Sum(table.1D.MultipleStatistics)))

    captured.warnings <- capture_warnings(Sum(table.1D.MultipleStatistics, warn = TRUE))
    stat.names <- dimnames(table.1D.MultipleStatistics)[[2]]
    expect_setequal(captured.warnings,
                    c(paste0("The input data contains statistics of different types (i.e., ",
                             paste0(stat.names, collapse = ", "),
                             "), it may not be appropriate to compute ", sQuote("Sum"), "."),
                      paste0(sQuote('Sum'), " cannot be computed as the data contains both Inf and -Inf.")))
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

    expect_warning(Sum(table2D.PercentageAndCount, warn = TRUE),
                   paste0("The input data contains statistics of different types ",
                          "(i.e., Row %, Count), it may not be appropriate to compute ",
                          sQuote("Sum"), "."), fixed = TRUE)

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
                     match.rows = "Yes",
                     match.columns = "No"),
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
                     match.rows = "Yes",
                     match.columns = "No"),
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
    expect_warning(computed.sum <- Sum(table2D.Percentage,
                                       table.with.non.matching.stat,
                                       remove.rows = NULL,
                                       remove.columns = NULL,
                                       warn = TRUE),
                   paste0("The input data contains statistics of different types ",
                          "(i.e., Row %, Column %), it may not be appropriate to ",
                          "compute ", sQuote("Sum"), "."),
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
    expect_equal(Sum(table1D.Average, table1D.Average, table1D.Average),
                 expected.out)
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
    expect_equal(Sum(table1D.Average, basic.array),
                 array(table1D.Average, dim = 4, dimnames = dimnames(table1D.Average)) +
                     basic.array)
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
    expect_equal(Sum(matrix.np, matrix.n1, match.rows = "No", match.columns = "No"),
                 expected.output)
    expect_equal(Sum(matrix.n1, matrix.np, match.rows = "No", match.columns = "No"),
                 expected.output)
    # n x p + 1 x p (and opposite order)
    expected.output <- matrix.np + array(rep(matrix.1p, each = nrow(matrix.np)),
                                         dim = dim(matrix.np))
    dimnames(expected.output)[[2L]] <- paste0(colnames(matrix.np), " + ", colnames(matrix.1p))
    expect_equal(Sum(matrix.np, matrix.1p, match.rows = "No", match.columns = "No"),
                 expected.output)
    dimnames(expected.output)[[2L]] <- paste0(colnames(matrix.1p), " + ", colnames(matrix.np))
    expect_equal(Sum(matrix.1p, matrix.np, match.rows = "No", match.columns = "No"),
                 expected.output)
    # n x 1 + 1 x p (and opposite order), both get reshaped
    expected.output <- array(matrix.n1, dim = dim(matrix.np)) +
                        array(rep(matrix.1p, each = nrow(matrix.np)),
                              dim = dim(matrix.np))
    dimnames(expected.output) <- list(rownames(matrix.n1), colnames(matrix.1p))
    expect_equal(Sum(matrix.n1, matrix.1p, match.rows = "No", match.columns = "No"),
                 expected.output)
    dimnames(expected.output) <- list(rownames(matrix.n1), colnames(matrix.1p))
    expect_equal(Sum(matrix.1p, matrix.n1, match.rows = "No", match.columns = "No"),
                 expected.output)
    # mismatching errors
    err.msg <- paste0(sQuote("Sum"), " requires multiple elements to have the same dimension ",
                      "or partially agreeing dimensions. In this case, the inputs are two ",
                      "matrices with 6 rows and 4 columns and 12 rows and 1 column ",
                      "respectively. Please ensure the inputs have the same or partially ",
                      "agreeing dimensions before attempting to recompute ", sQuote("Sum"))
    expect_error(Sum(matrix.np, matrix.2n1, match.rows = "No", match.columns = "No"),
                 err.msg)
    err.msg <- sub("6 rows and 4 columns and 12 rows and 1 column",
                   "12 rows and 1 column and 6 rows and 4 columns",
                   err.msg)
    expect_error(Sum(matrix.2n1, matrix.np, match.rows = "No", match.columns = "No"),
                 err.msg)
    err.msg <- sub("12 rows and 1 column and 6 rows and 4 columns",
                   "6 rows and 1 column and 12 rows and 1 column",
                   err.msg)
    expect_error(Sum(matrix.n1, matrix.2n1, match.rows = "No", match.columns = "No"),
                 err.msg)
    err.msg <- sub("6 rows and 1 column and 12 rows and 1 column",
                   "12 rows and 1 column and 6 rows and 1 column",
                   err.msg)
    expect_error(Sum(matrix.2n1, matrix.n1, match.rows = "No", match.columns = "No"),
                 err.msg)
    matrix.1q <- matrix(1:2, nrow = 1)
    err.msg <- sub("12 rows and 1 column and 6 rows and 1 column",
                   "1 row and 4 columns and 1 row and 2 columns",
                   err.msg)
    expect_error(Sum(matrix.1p, matrix.1q, match.rows = "No", match.columns = "No"),
                 err.msg)
    err.msg <- sub("1 row and 4 columns and 1 row and 2 columns",
                   "1 row and 2 columns and 1 row and 4 columns",
                   err.msg)
    expect_error(Sum(matrix.1q, matrix.1p, match.rows = "No", match.columns = "No"),
                 err.msg)
    matrix.mq <- matrix(1:42, nrow = 7, ncol = 6)
    err.msg <- sub("1 row and 2 columns and 1 row and 4 columns",
                   "6 rows and 4 columns and 7 rows and 6 columns",
                   err.msg)
    expect_error(Sum(matrix.np, matrix.mq, match.rows = "No", match.columns = "No"),
                 err.msg)
    err.msg <- sub("6 rows and 4 columns and 7 rows and 6 columns",
                   "7 rows and 6 columns and 6 rows and 4 columns",
                   err.msg)
    expect_error(Sum(matrix.mq, matrix.np, match.rows = "No", match.columns = "No"),
                 err.msg)
    err.msg <- sub("the inputs are two matrices",
                   "the inputs are a matrix and Q Table",
                   err.msg)
    err.msg <- sub("6 rows and 4 columns",
                   "6 rows, 10 columns and 2 statistics",
                   err.msg)
    expect_error(Sum(matrix.mq, table2D.PercentageAndCount, match.rows = "No", match.columns = "No"),
                 err.msg)
    # Edge case correctly matches columns
    input1 <- cbind("Q1" = c(a = 1, b = 2))
    input2 <- cbind("Q2" = c(A = 1, B = 2, c= 3))
    expected.output <- cbind("Q1" = c(a = 1, b = 2, c= 0),
                             "Q2" = c(a = 1, b = 2, c = 3))
    expect_equal(Sum(input1, input2, match.rows = "Fuzzy", match.columns = "Yes"),
                 expected.output)
    matrix.in <- cbind("Coke" = c(a = 1, b = 2, c = 3),
                       "Pepsi" = c(a = 4, b = 5, c = 6))
    vector.to.reshape <- 1:2
    expected.out <- cbind("Coke + 1" = c(a = 2, b = 3, c = 4),
                          "Pepsi + 2" = c(a = 6, b = 7, c = 8))
    expect_equal(Sum(matrix.in, vector.to.reshape, match.rows = "No", match.columns = "No"),
                 expected.out)
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
    expect_error(Sum(table1D.Average, table1D.Average, variable.Binary),
                 paste0(sQuote("Sum"), " requires input elements to be of the same type. ",
                        "However, both QTables and Variables have been used as ",
                        "inputs. It is not possible to use ", sQuote("Sum"), " with multiple ",
                        "inputs of different types. ",
                        verbs:::determineAppropriateContact()),
                 fixed = TRUE)
    # Attempt to use 3d array
    arr <- array(1:24, dim = 2:4)
    expect_error(Sum(arr),
                 paste0(sQuote("Sum"), " only supports inputs that have 1 or 2 dimensions. ",
                        "A supplied input has 3 dimensions. ",
                        verbs:::determineAppropriateContact()),
                 fixed = TRUE)
})

test_that("Warnings", {
    # Warnings about NaN and adding Inf and -Inf
    opp.table.1D.MultipleStatistics <- table.1D.MultipleStatistics
    opp.table.1D.MultipleStatistics[, 5] <- -opp.table.1D.MultipleStatistics[, 5]
    expect_warning(expect_true(any(is.nan(Sum(table.1D.MultipleStatistics,
                                              opp.table.1D.MultipleStatistics,
                                              warn = TRUE)))),
                   paste0(sQuote("Sum"), " cannot compute some values as the data contains both Inf and -Inf."),
                   fixed = TRUE)
    expect_warning(expect_true(is.nan(Sum(c(Inf, -Inf), warn = TRUE))),
                   paste0(sQuote("Sum"), " cannot be computed as the data contains both Inf and -Inf."),
                   fixed = TRUE)
    # Warnings about missing values
    expect_warning(Sum(c(1:3, NA), warn = TRUE), "Missing values have been ignored in calculation.")
    x <- table.1D.MultipleStatistics
    x[1, 1] <- NA
    expect_warning(Sum(x, x, warn = TRUE), "Missing values have been ignored in calculation.")
    # Throw warning about filter and/or weights being ignored for Q Tables
    expect_warning(Sum(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)), warn = TRUE),
                   paste0(sQuote("Sum"), " is unable to apply a filter to the input Q Table ",
                          "since the original variable data is unavailable."),
                   fixed = TRUE)
    expect_warning(Sum(table1D.Average, weights = runif(10), warn = TRUE),
                   paste0(sQuote("Sum"), " is unable to apply weights to the input Q Table ",
                          "since the original variable data is unavailable."),
                   fixed = TRUE)
    expect_warning(Sum(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)),
                       weights = runif(10), warn = TRUE),
                   paste0(sQuote("Sum"), " is unable to apply a filter or weights to the input Q Table ",
                          "since the original variable data is unavailable."),
                   fixed = TRUE)
    # Recycling of inputs that partially agree on dimensions
    ## e.g. an n x p matrix and an n x 1 column vector.
    # expect_warning(Sum(matrix(1:12, nrow = 3), 1:3, match.columns = "No")
})

test_that("Labels when not matching", {
    # Exact match rows but merge columns with good label
    x <- array(1:2, dim = 2:1, dimnames = list(letters[1:2], "Q1"))
    y <- array(1:2, dim = 2:1, dimnames = list(letters[1:2], "Q2"))
    expected <- array(2 * 1:2, dim = 2:1, dimnames = list(letters[1:2], "Q1 + Q2"))
    expect_equal(Sum(x, y, match.rows = "Yes", match.columns = "No"),
                 expected)
    # Fuzzy match rows and merge columns with good label
    x <- array(1:2, dim = 2:1, dimnames = list(letters[1:2], "Q1"))
    y <- array(1:3, dim = c(3, 1), dimnames = list(c("A", "B", "c"), "Q2"))
    expected <- array(c(2, 4, 3), dim = c(3, 1), dimnames = list(letters[1:3], "Q1 + Q2"))
    expect_equal(Sum(x, y, match.rows = "Fuzzy", match.columns = "No"),
                 expected)
    # Adding a scalar
    x <- array(1:3, dim = c(3, 1), dimnames = list(letters[1:3], "Coke"))
    y <- 2
    expected <- array(3:5, dim = c(3, 1), dimnames = list(letters[1:3], "Coke + 2"))
    expect_equal(Sum(x, y, match.rows = "No", match.columns = "No"), expected)
    expected <- array(3:5, dim = c(3, 1), dimnames = list(letters[1:3], "2 + Coke"))
    expect_equal(Sum(y, x, match.rows = "No", match.columns = "No"), expected)
    # Adding a row vector
    x <- array(1:6, dim = c(3, 2), dimnames = list(letters[1:3], c("Coke", "Pepsi")))
    y <- array(1:2, dim = c(1, 2))
    expected <- array(c(2:4, 6:8), dim = c(3, 2), dimnames = list(letters[1:3], c("Coke + 1", "Pepsi + 2")))
    expect_equal(Sum(x, y, match.rows = "No", match.columns = "No"), expected)
    # Adding a column vector
    x <- array(1:6, dim = c(3, 2), dimnames = list(letters[1:3], c("Coke", "Pepsi")))
    y <- array(1:2, dim = c(1, 2))
    expected <- array(c(2:4, 6:8), dim = c(3, 2), dimnames = list(letters[1:3], c("Coke + 1", "Pepsi + 2")))
    expect_equal(Sum(x, y, match.rows = "No", match.columns = "No"), expected)
})


test_that("Check function call correctly identified", {
    correct.tests <- expand.grid(c("", "verbs::", "verbs:::"), c("Sum", "SumRows", "SumColumns"))
    correct.tests <- apply(correct.tests, 1, paste0, collapse = "")
    expect_true(all(isACallStartingWithSum(correct.tests)))
    expect_warning(expect_equal(vapply(correct.tests,
                                       function(f) eval(expr = parse(text = paste0(f, "(NA)", collapse = ""))),
                                       numeric(1L)),
                                rep(0, length(correct.tests)), check.attributes = FALSE),
                   NA)
    expect_false(isACallStartingWithSum("sum"))
    list.of.numeric.elems <- replicate(3, runif(5), simplify = FALSE)
    list.with.bad.elem <- list.of.numeric.elems
    list.with.bad.elem[[1L]] <- "Hello"
    expect_equal(lapply(list.of.numeric.elems, Sum), lapply(list.of.numeric.elems, sum))
    expect_error(lapply(list.with.bad.elem, Sum, call. = TRUE),
                 paste0("Text data has been supplied but ", sQuote("Sum"), " requires numeric data."))
    expect_error(verbs::Sum("Foo", call. = TRUE),
                 paste0("Text data has been supplied but ", sQuote("Sum"), " requires numeric data."))
})
