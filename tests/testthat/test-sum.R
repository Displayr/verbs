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
    expected.sum <- as.vector(variable.Binary + variable.Numeric)
    expect_equal(Sum(variable.Binary, variable.Numeric, remove.missing = FALSE),
                 expected.sum)
    expected.inputs <- lapply(list(variable.Binary, variable.Numeric), function(x) {
        x[is.na(x)] <- 0
        x
    })
    expect_equal(Sum(variable.Binary, variable.Numeric, remove.missing = TRUE),
                 as.vector(Reduce(`+`, expected.inputs)))
})

test_that("Variables with weights, filters (subset), and a combination of the two", {
    subset.missing.out <- !is.na(variable.Numeric)
    expect_equal(Sum(variable.Numeric, subset = subset.missing.out),
                 sum(variable.Numeric, na.rm = TRUE))
    transformed.nom <- flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)
    transformed.nom <- transformed.nom[!is.na(transformed.nom)]
    subset.num <- variable.Numeric[!is.na(variable.Numeric)]
    expect_equal(Sum(variable.Numeric, variable.Nominal, subset = subset.missing.out),
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
    expect_equal(Sum(variable.Numeric, variable.Nominal,
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
    expect_equal(Sum(table2D.Percentage, table2D.PercentageNaN,
                     remove.missing = FALSE,
                     remove.rows = c("None of these", "NET")),
                 expected.table.out)
    sanitized.inputs <- lapply(inputs, function(x) {
        x[is.na(x)] <- 0
        x
    })
    expected.sanitized.out <- Reduce(`+`, sanitized.inputs)
    expect_equal(Sum(table2D.Percentage, table2D.PercentageNaN,
                     remove.missing = TRUE,
                     remove.rows = c("None of these", "NET")),
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
                 sum(table1D.Average[1:3], basic.matrix[1:3, ]))
    basic.matrix <- table2D.Percentage[TRUE, TRUE]
    expect_equal(Sum(table2D.Percentage, basic.matrix[, 1:9]),
                 sum(table2D.Percentage[, 1:9], basic.matrix[, 1:9]))
    basic.array <- table1D.Average[TRUE] # Removes attributes
    expect_equal(Sum(table1D.Average, basic.array),
                 sum(table1D.Average[1:3], basic.array[1:3]))
})

test_that("Sum matrix and vector",
{
## n x m + n x 1 works
## n x m + 1 x m works
## else error
## respects argument specifying how to match names
    matrix.1 <- matrix(1:24, nrow = 6)
    matrix.2 <- matrix(runif(6), nrow = 6)
    matrix.3 <- matrix(runif(8), nrow = 2)
    matrix.4 <- matrix(runif(12), nrow = 12)
    expect_equal(Sum(matrix.1, matrix.2), sum(matrix.1, matrix.2))
    expect_equal(Sum(matrix.1, matrix.3), sum(matrix.1, matrix.3))
    expect_error(Sum(matrix.1, matrix.4),
                 paste0(sQuote("Sum"), " requires inputs to have the same number of rows ",
                        "or the same number of columns. ",
                        determineAppropriateContact()))
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
    captured.warnings <- capture_warnings(Sum(table1D.Average, table1D.Average, warn = TRUE))
    # Only a single warning despite rows being removed from two tables.
    expect_equal(captured.warnings, "These categories have been removed from the rows: SUM.")
    SUM.col <- matrix(rowSums(table.1D.MultipleStatistics), ncol = 1, dimnames = list(rep("", 4), "NET"))
    table.1D.MultipleStatistics.with.SUM.col <- cbind(table.1D.MultipleStatistics, SUM.col)
    table.1D.MultipleStatistics.with.SUM.col[1, 1] <- NA
    table.1D.MultipleStatistics.with.SUM.col <- CopyAttributes(table.1D.MultipleStatistics.with.SUM.col, table.1D.MultipleStatistics)
    captured.warnings <- capture_warnings(expect_true(is.nan(Sum(table.1D.MultipleStatistics.with.SUM.col,
                                                                 table.1D.MultipleStatistics.with.SUM.col,
                                                                 warn = TRUE))))
    multi.stat.warn <- paste0("The input data contains statistics of different types ",
                              "(i.e., Average, Effective Sample Size, t-Statistic, d.f., ",
                              "z-Statistic, Corrected p), it may not be appropriate to compute ", sQuote("Sum"), ".")
    expect_setequal(captured.warnings,
                    c(multi.stat.warn,
                      "Missing values have been ignored in calculation.",
                      "These categories have been removed from the rows: SUM.",
                      "These categories have been removed from the columns: NET.",
                      paste0(sQuote("Sum"), " cannot be computed as the data contains both Inf and -Inf.")))
    captured.warnings <- capture_warnings(expect_true(is.na(Sum(table.1D.MultipleStatistics.with.SUM.col,
                                                                table.1D.MultipleStatistics.with.SUM.col,
                                                                remove.missing = FALSE,
                                                                warn = TRUE))))
    expect_setequal(captured.warnings,
                    c(multi.stat.warn,
                      "These categories have been removed from the rows: SUM.",
                      "These categories have been removed from the columns: NET."))
    # Throw warning about filter and/or weights being ignored for Q Tables
    captured.warnings <- capture_warnings(Sum(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)), warn = TRUE))
    expect_setequal(captured.warnings,
                    c("These categories have been removed from the rows: SUM.",
                      paste0(sQuote("Sum"), " is unable to apply a filter to the input Q Table ",
                             "since the original variable data is unavailable.")))
    captured.warnings <- capture_warnings(Sum(table1D.Average, weights = runif(10), warn = TRUE))
    expect_setequal(captured.warnings,
                    c("These categories have been removed from the rows: SUM.",
                      paste0(sQuote("Sum"), " is unable to apply weights to the input Q Table ",
                             "since the original variable data is unavailable.")))
    captured.warnings <- capture_warnings(Sum(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)),
                                              weights = runif(10), warn = TRUE))
    expect_setequal(captured.warnings,
                    c("These categories have been removed from the rows: SUM.",
                      paste0(sQuote("Sum"), " is unable to apply a filter or weights to the input Q Table ",
                             "since the original variable data is unavailable.")))
})

