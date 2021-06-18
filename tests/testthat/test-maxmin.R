context("Min and Max")

load("variable.Text.rda")
load("variable.Binary.rda")
load("variable.Nominal.rda")
load("variable.Numeric.rda")
load("variable.Time.rda")
load("variable.Date.rda")

quoted.function <- sQuote("Min")


test_that("Variables", {
    expect_equal(Max(variable.Binary), 1)
    expect_equal(Min(variable.Numeric), 21)
    ## Factors
    # With value attributes
    expect_equal(Max(variable.Nominal), 77)
    expect_equal(Min(variable.Nominal), 21)
    # Without value attributes
    factor.conversion.warning <- capture_warnings(flipTransformations::AsNumeric(factor(1:2), binary = FALSE))
    expect_warning(basic.factor <- Min(factor(1:10)), factor.conversion.warning, fixed = TRUE)
    expect_equal(basic.factor, min(1:10))
    # Missing values in calculations
    expect_true(is.na(Min(variable.Binary, remove.missing = FALSE)))
    expect_true(is.na(Max(variable.Numeric, remove.missing = FALSE)))
    # Multiple variables
    expect_equivalent(Min(variable.Numeric, variable.Numeric+1, remove.missing = FALSE),
                      variable.Numeric)
    # Expect Variable sets to be handled ok
    df1 <- data.frame(x = runif(10), y = runif(10))
    df2 <- data.frame(y = runif(10), z = runif(10))
    expected.out <- as.matrix(data.frame(x = df1[["x"]], y = pmax(df1[["y"]], df2[["y"]]),
                                         z = df2[["z"]]))
    expect_equivalent(Min(df1, df2, match.elements = c(rows = "No",
                                                       columns = "Yes - hide unmatched")),
                      pmin(df1[["y"]], df2[["y"]]))
    expect_equivalent(Max(df1, df2, match.elements = c(rows = "No",
                                                       columns = "Yes - show unmatched")),
                      expected.out)
})

test_that("Variables with filters (subset), and a combination of the two", {
    subset.missing.out <- !is.na(variable.Numeric)
    expect_equal(Max(variable.Numeric, subset = subset.missing.out),
                 max(variable.Numeric, na.rm = TRUE))
    transformed.nom <- flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)
    expect_equivalent(Max(variable.Numeric, variable.Nominal, subset = subset.missing.out),
                      pmax(transformed.nom[subset.missing.out], variable.Numeric[subset.missing.out]))
})


load("table1D.Average.rda")
load("table1D.Percentage.rda")
load("table.1D.MultipleStatistics.rda")
test_that("Table 1D",
{
    expect_equal(Max(table1D.Percentage, remove.rows = "NET"), 15.59633, tolerance = 1e-4)

    captured.warnings <- capture_warnings(Min(table.1D.MultipleStatistics, warn = TRUE))
    stat.names <- dimnames(table.1D.MultipleStatistics)[[2]]
    expected.warnings <- capture_warnings(
        throwWarningAboutDifferentStatistics(colnames(table.1D.MultipleStatistics),
                                             quoted.function))

    expect_equal(captured.warnings, expected.warnings)
    # Removal of row categories in a 1D table
    expect_equal(Max(table1D.Average, remove.rows = "SUM"), Max(table1D.Average[1:3]))
    expect_equal(Min(table1D.Average,
                     remove.rows = NULL), min(table1D.Average))

    # Missing values
    z  <-  table1D.Average
    z[2]  <-  NA
    expect_equal(Min(z, remove.rows = "SUM"), min(z[1:3], na.rm = TRUE))
    expect_true(is.na(Max(z, remove.missing = FALSE)))
})

load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")
test_that("Table 2D",
{
    expect_equal(Max(table2D.Percentage, remove.columns = "NET"),
                 max(table2D.Percentage[, !colnames(table2D.Percentage) %in% "NET"]))
    expect_equal(Min(table2D.PercentageNaN, remove.columns = "NET", remove.rows = "NET"),
                 min(table2D.PercentageNaN[-8, -10], na.rm = TRUE))
    # Note that while we represent this as a 3D array, from the user's perspective
    # this is a 2D table, where the third dimension is stacked into the rows.
    expected.max <- apply(table2D.PercentageAndCount[, -10, ], 3, max)
    expect_equal(Max(table2D.PercentageAndCount, remove.columns = "NET", remove.rows = "NET"),
                 expected.max)

    # Missing values
    expect_true(is.na(Max(table2D.PercentageNaN, remove.missing = FALSE)))

    mat <- matrix(50, nrow(table2D.Percentage), ncol(table2D.Percentage))
    expect_equivalent(Max(table2D.Percentage, mat),
                 pmax(table2D.Percentage, mat, na.rm = TRUE))
})

.removeAttributes <- function(x)
{
    attr.out <- setdiff(names(attributes(x)),
                        c("dim", "names", "dimnames"))
    for (a in attr.out)
        attr(x, a) <- NULL
    x
}

test_that("Works with two or more Q Tables", {
    # Expect warning if statistic of second table isn't matching
    table.with.non.matching.stat <- table2D.Percentage
    attr(table.with.non.matching.stat, "statistic") <- "Column %"
    diff.stats <- vapply(list(table2D.Percentage, table.with.non.matching.stat),
                         attr, character(1L),
                         which = "statistic")
    diff.stat.warning <- capture_warnings(throwWarningAboutDifferentStatistics(diff.stats, quoted.function))
    expect_warning(computed.min <- Min(table2D.Percentage,
                                       table.with.non.matching.stat,
                                       remove.rows = NULL,
                                       remove.columns = NULL,
                                       warn = TRUE),
                   diff.stat.warning,
                   fixed = TRUE)
    expect_equivalent(computed.min, pmin(table2D.Percentage, table.with.non.matching.stat,
                                         na.rm = TRUE))

    # If elements are congruent, then works as expected
    expected.out <- table1D.Average + 1
    expected.out <- .removeAttributes(expected.out)
    expected.out <- setNames(as.vector(expected.out), nm = names(expected.out))
    expect_equal(Max(table1D.Average-1, table1D.Average, table1D.Average+1),
                 expected.out)
})

test_that("Min/Max matrix and vector",
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
    expected.output <- pmin(matrix.np, array(matrix.n1, dim = dim(matrix.np)))
    dimnames(expected.output) <- dimnames(matrix.np)
    expect_equal(Min(matrix.np, matrix.n1,
                     match.elements = c(rows = "No", columns = "No")),
                 expected.output)
    expected.output <- pmax(matrix.np, array(matrix.n1, dim = dim(matrix.np)))
    dimnames(expected.output) <- dimnames(matrix.np)
    expect_equal(Max(matrix.n1, matrix.np,
                     match.elements = c(rows = "No", columns = "No")),
                 expected.output)

    # n x p + 1 x p (and opposite order)
    expected.output <- pmax(matrix.np, array(rep(matrix.1p, each = nrow(matrix.np)),
                                         dim = dim(matrix.np)))
    dimnames(expected.output)[[2L]] <- paste0(colnames(matrix.np), ", ", colnames(matrix.1p))
    expect_equal(Max(matrix.np, matrix.1p,
                     match.elements = c(rows = "No", columns = "No")),
                 expected.output)
    expected.output <- pmin(matrix.np, array(rep(matrix.1p, each = nrow(matrix.np)),
                                         dim = dim(matrix.np)))
    dimnames(expected.output)[[2L]] <-  paste0(colnames(matrix.1p), ", ", colnames(matrix.np))
    expect_equal(Min(matrix.1p, matrix.np,
                     match.elements = c(rows = "No", columns = "No")),
                 expected.output)

    # n x 1 + 1 x p (and opposite order), both get recycled
    expected.output <- pmin(array(matrix.n1, dim = dim(matrix.np)),
                        array(rep(matrix.1p, each = nrow(matrix.np)),
                              dim = dim(matrix.np)))
    dimnames(expected.output) <- list(rownames(matrix.n1), colnames(matrix.1p))
    expect_equal(Min(matrix.n1, matrix.1p,
                     match.elements = c(rows = "No", columns = "No")),
                 expected.output)
    expected.output <- pmax(array(matrix.n1, dim = dim(matrix.np)),
                        array(rep(matrix.1p, each = nrow(matrix.np)),
                              dim = dim(matrix.np)))
    dimnames(expected.output) <- list(rownames(matrix.n1), colnames(matrix.1p))
    expect_equal(Max(matrix.1p, matrix.n1,
                     match.elements = c(rows = "No", columns = "No")),
                 expected.output)

    # mismatching errors
    dimension.error <- capture_error(throwErrorAboutDimensionMismatch(list(c(6, 4), c(12, 1)),
                                                                      quoted.function))[["message"]]
    expect_error(Min(matrix.np, matrix.2n1,
                     match.elements = c(rows = "No", columns = "No")),
                 dimension.error)
    dimension.error <- capture_error(throwErrorAboutDimensionMismatch(list(c(12, 1), c(6, 4)),
                                                                      sQuote("Max")))[["message"]]
    expect_error(Max(matrix.2n1, matrix.np,
                     match.elements = c(rows = "No", columns = "No")),
                 dimension.error)
})

test_that("Min/Max uses ChartData if available",
{
    var1 <- variable.Numeric
    var2 <- runif(length(var1))
    correlation.output <- flipStatistics::CorrelationMatrix(data.frame(var1, var2))
    expect_equal(Min(correlation.output),
                 min(attr(correlation.output, "ChartData")))
})

test_that("A single R Output (e.g. a vanilla matrix or vector) selected",
{
    matrix.1 <- matrix(1:24, nrow = 6)
    expect_equal(Max(matrix.1), max(matrix.1))
    vector.1 <-1:24
    expect_equal(Min(vector.1), min(vector.1))
})

test_that("Incompatible inputs", {
    # If elements are not congruent, then error
    expected.error <- capture_error(throwErrorInputsContainVariablesAndTables(quoted.function))[["message"]]
    expect_error(Min(table1D.Average, table1D.Average, variable.Binary),
                 expected.error, fixed = TRUE)
    # Attempt to use 3d array
    arr <- array(1:24, dim = 2:4)
    expected.error <- capture_error(throwErrorAboutHigherDimArray(3L, sQuote("Max")))[["message"]]
    expect_error(Max(arr), expected.error, fixed = TRUE)
})

test_that("Min/Max with scalar",
{
    x <- table2D.Percentage
    y <- 10
    expect_equivalent(Max(x, y, remove.columns = "NET"),
                      pmax(x[, !colnames(x) %in% "NET"], y))
})

test_that("Automatic Matching", {
    X <- matrix(1:6, nrow = 3, dimnames = list(1:3, 1:2))
    Y <- matrix(6:1, nrow = 3, dimnames = list(3:1, 2:1))
    expect_equal(Max(X, Y, match.elements = "Yes"), X)
    expect_equivalent(max.out <- Max(X, Y, match.elements = c(rows = "No", columns = "No")),
                 pmax(X, Y))
    expect_equal(colnames(max.out), c("1, 2", "2, 1"))
    tX <- t(X)
    expect_equal(Min(X, tX), X)
})