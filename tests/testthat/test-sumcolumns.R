context("SumColumns")

load("variable.Text.rda")
load("variable.Binary.rda")
load("variable.Nominal.rda")
load("variable.Numeric.rda")
load("variable.Time.rda")
load("variable.Date.rda")

test_that("Variables", {
    expect_error(SumColumns(variable.Text),
                 paste0("Text data has been supplied but ", sQuote('SumColumns'), " requires numeric data."))
    expect_error(SumColumns(variable.Binary, variable.Text),
                 paste0("Text data has been supplied but ", sQuote('SumColumns'), " requires numeric data."))
    expect_error(SumColumns(variable.Date, variable.Binary),
                 paste0("Date/Time data has been supplied but ", sQuote('SumColumns'), " requires numeric data."))
    expect_error(SumColumns(variable.Time, variable.Numeric),
                 paste0("Date/Time data has been supplied but ", sQuote('SumColumns'), " requires numeric data."))
    expect_equal(SumColumns(variable.Nominal), c(Age = 12606))
    expect_equal(SumColumns(variable.Binary, variable.Numeric, variable.Nominal),
                 c("Coca-Cola" = 155, Age = 12606, Age = 12606))
    expect_equal(SumColumns(data.frame(variable.Binary, variable.Nominal)),
                 c("variable.Binary" = 155, "variable.Nominal" = 12606))
    # Warnings for factors
    ## No extra warning for variables that are converted using value attributes
    captured.warnings <- capture_warnings(SumColumns(variable.Binary, variable.Nominal, warn = TRUE))
    expect_equal(captured.warnings, "Missing values have been ignored in calculation.")
    ## AsNumeric warning should be appearing when factor converted that has no value attributes
    expect_warning(SumColumns(1:5, factor(1:5), warn = TRUE),
                   paste0("Data has been automatically converted to numeric. ",
                          "Values are assigned according to the labels of the ",
                          "categories. To use alternative numeric values, ",
                          "transform the data prior including it in this ",
                          "analysis (e.g. by changing its structure)."),
                   fixed = TRUE)
    # Missing values
    expect_equal(SumColumns(variable.Binary, variable.Numeric, variable.Nominal,
                            remove.missing = FALSE),
                 c("Coca-Cola" = NA_integer_, Age = NA_integer_, Age = NA_integer_))
})

test_that("Variables with weights, filters (subset), and a combination of the two", {
    # Variables and multiple variables
    subset.missing.out <- !is.na(variable.Numeric)
    nominal.to.numeric <- flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)
    expect_equal(SumColumns(variable.Numeric, subset = subset.missing.out),
                 c(Age = sum(variable.Numeric, na.rm = TRUE)))
    expect_equal(SumColumns(variable.Numeric, variable.Nominal, subset = subset.missing.out),
                 c(Age = sum(variable.Numeric, na.rm = TRUE),
                   Age = sum(nominal.to.numeric, na.rm = TRUE)))
    expect_error(SumColumns(variable.Numeric[1:10], subset = subset.missing.out),
                 paste0("The subset vector has length 327. However, it needs to ",
                        "have length 10 to match the number of cases in the supplied input data."))
    expect_error(SumColumns(variable.Numeric, 1:10, subset = subset.missing.out),
                 paste0(sQuote("SumColumns"), " requires all input elements to have the same size to be able ",
                        "to apply a filter or weight vector. ",
                        verbs:::determineAppropriateContact()))
    weights <- runif(length(variable.Numeric))
    expect_equal(SumColumns(variable.Numeric, weights = weights),
                 c(Age = sum(variable.Numeric * weights, na.rm = TRUE)))
    expect_equal(SumColumns(variable.Numeric, variable.Binary, weights = weights),
                 c(Age = sum(variable.Numeric * weights, na.rm = TRUE),
                   `Coca-Cola` = sum(variable.Binary * weights, na.rm = TRUE)))
    expect_equal(SumColumns(variable.Numeric, variable.Nominal,
                            weights = weights,
                            subset = subset.missing.out),
                 c(Age = sum(variable.Numeric * weights, na.rm = TRUE),
                   Age = sum(nominal.to.numeric * weights, na.rm = TRUE)))
    expect_error(SumColumns(variable.Numeric, weights = weights[1:10]),
                 paste0("The weights vector has length 10. However, it needs to ",
                        "have length 327 to match the number of cases in the supplied input data."))
    # Variable sets and data.frames
    expect_equal(SumColumns(data.frame(variable.Binary, variable.Nominal),
                            subset = subset.missing.out, remove.missing = FALSE),
                 c("variable.Binary" = NA, "variable.Nominal" = 12606))
    subset.binary <- !is.na(variable.Binary)
    expected.weighted.bin <- sum(variable.Binary * weights, na.rm = TRUE)
    expect_equal(SumColumns(data.frame(variable.Binary, variable.Nominal),
                            subset = subset.binary, weights = weights,
                            remove.missing = FALSE),
                 c("variable.Binary" = expected.weighted.bin, "variable.Nominal" = NA))
    df <- data.frame(variable.Binary,
                     variable.Nominal = flipTransformations::AsNumeric(variable.Nominal, binary = FALSE))
    weighted.df <- df * weights
    expected.sum <- colSums(weighted.df, na.rm = TRUE)
    expect_equal(SumColumns(data.frame(variable.Binary, variable.Nominal),
                            weights = weights,
                            remove.missing = TRUE),
                 expected.sum)

})

load("table1D.Average.rda")
load("table1D.Percentage.rda")
load("table.1D.MultipleStatistics.rda")

test_that("Table 1D", {
    expect_equal(SumColumns(table1D.Percentage), c(`table.Age` = 100))
    expect_equal(SumColumns(table.1D.MultipleStatistics),
                 colSums(table.1D.MultipleStatistics[-4,], na.rm = TRUE))
    expect_warning(SumColumns(table.1D.MultipleStatistics, warn = TRUE),
                   paste0(sQuote("SumColumns"), " cannot compute some values as the ",
                          "data contains both Inf and -Inf."))
})

load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")
test_that("Table 2D", {
    expect_equal(SumColumns(table2D.Percentage),
                 colSums(table2D.Percentage, na.rm = TRUE))
    expect_equal(SumColumns(table2D.PercentageNaN),
                 colSums(table2D.PercentageNaN[-8, ], na.rm = TRUE))
    col.summed.2d.table.multi.stats <- cbind(`Row %` = colSums(table2D.PercentageAndCount[, , 1]),
                                             `Count` = colSums(table2D.PercentageAndCount[, , 2]))
    expect_equal(SumColumns(table2D.PercentageAndCount),
                 col.summed.2d.table.multi.stats)
    # Extra category removed removed and warn about missing value removal
    expect_equal(expect_warning(output.wo.missing <- SumColumns(table2D.PercentageNaN,
                                                                remove.rows = c("NET", "None of these"),
                                                                remove.missing = TRUE,
                                                                warn = TRUE),
                                "Missing values have been ignored in calculation"),
                 colSums(table2D.PercentageNaN[1:6, ], na.rm = TRUE))
    # Missing values
    expect_true(anyNA(SumRows(table2D.PercentageNaN, remove.missing = FALSE)))
    expect_false(anyNA(output.wo.missing))
})

test_that("Multiple tables and multiple basic inputs", {
    expect_equal(SumColumns(table.1D.MultipleStatistics, table1D.Average),
                 c(colSums(table.1D.MultipleStatistics[-4, ]),
                   c(`table.Frequency.of.drinking` = sum(table1D.Average[-4]))))
    input.matrix <- matrix(c(1, 2, 2, 5), nrow = 2, dimnames = list(letters[1:2], c("Q1", "Q2")))
    input.column.vector <- array(c(1:3, 6), dim = c(4, 1), dimnames = list(c(letters[1:3], "SUM"), "Q3"))
    expect_equal(SumColumns(input.matrix, input.column.vector),
                 c("Q1" = 3, "Q2" = 7, "Q3" = 6))
})

test_that("Warnings", {
    expect_warning(expect_equal(SumColumns(table2D.PercentageNaN,
                                           remove.rows = c("None of these", "NET"),
                                           warn = TRUE),
                                colSums(table2D.PercentageNaN[-(7:8), ], na.rm = TRUE)),
                   "Missing values have been ignored in calculation.")
    SUM.col <- matrix(rowSums(table.1D.MultipleStatistics), ncol = 1, dimnames = list(rep("", 4), "NET"))
    table.1D.MultipleStatistics.with.SUM.col <- cbind(table.1D.MultipleStatistics, SUM.col)
    table.1D.MultipleStatistics.with.SUM.col[1, 1] <- -Inf
    table.1D.MultipleStatistics.with.SUM.col <- CopyAttributes(table.1D.MultipleStatistics.with.SUM.col, table.1D.MultipleStatistics)
    expect_warning(expect_equal(SumColumns(table.1D.MultipleStatistics.with.SUM.col,
                                           warn = TRUE),
                                colSums(table.1D.MultipleStatistics.with.SUM.col[-4, ], na.rm = TRUE)),
                   paste0(sQuote("SumColumns"), " cannot compute some values as the data contains both Inf and -Inf."))
    ## Same situation with data.frame
    df.input <- as.data.frame(table.1D.MultipleStatistics.with.SUM.col)
    expect_warning(expect_equal(SumColumns(df.input, warn = TRUE),
                                colSums(df.input[-4, ], na.rm = TRUE)),
                   paste0(sQuote("SumColumns"), " cannot compute some values as the data contains both Inf and -Inf."))
    table.1D.MultipleStatistics.with.SUM.col[1, ] <- Inf
    table.1D.MultipleStatistics.with.SUM.col[2, ] <- -Inf
    expect_warning(expect_true(all(is.nan(SumColumns(table.1D.MultipleStatistics.with.SUM.col,
                                                     warn = TRUE)))),
                   paste0(sQuote("SumColumns"), " cannot be computed as the data contains both Inf and -Inf."))
    df.input <- as.data.frame(table.1D.MultipleStatistics.with.SUM.col)
    expect_warning(expect_true(all(is.nan(SumColumns(df.input, warn = TRUE)))),
                   paste0(sQuote("SumColumns"), " cannot be computed as the data contains both Inf and -Inf."))
    # Throw warning about filter and/or weights being ignored for Q Tables
    expect_warning(expect_equal(SumColumns(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)), warn = TRUE),
                                c(`table.Frequency.of.drinking` = sum(table1D.Average[-4]))),
                   paste0(sQuote("SumColumns"), " is unable to apply a filter to the input Q Table ",
                          "since the original variable data is unavailable."))
    expect_warning(expect_equal(SumColumns(table1D.Average, weights = runif(10), warn = TRUE),
                                c(`table.Frequency.of.drinking` = sum(table1D.Average[-4]))),
                   paste0(sQuote("SumColumns"), " is unable to apply weights to the input Q Table ",
                          "since the original variable data is unavailable."))
    expect_warning(expect_equal(SumColumns(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)),
                                           weights = runif(10), warn = TRUE),
                                c(`table.Frequency.of.drinking` = sum(table1D.Average[-4]))),
                   paste0(sQuote("SumColumns"), " is unable to apply a filter or weights to the input Q Table ",
                          "since the original variable data is unavailable."))
    input.matrix <- matrix(c(Inf, -Inf, 1, 2), nrow = 2, dimnames = list(NULL, letters[1:2]))
    input.vect <- c(Inf, -Inf)
    expect_warning(SumColumns(input.matrix, input.vect, warn = TRUE),
                   paste0(sQuote("SumColumns"), " cannot compute some values as the data contains both Inf and -Inf."))
    expect_warning(SumColumns(input.matrix[, -2], input.vect, warn = TRUE),
                   paste0(sQuote("SumColumns"), " cannot be computed as the data contains both Inf and -Inf."))
})
