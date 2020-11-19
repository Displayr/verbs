context("SumColumns")

data(variable.Text)
data(variable.Binary)
data(variable.Nominal)
data(variable.Numeric)
data(variable.Time)
data(variable.Date)

test_that("Variables", {
    expect_error(SumColumns(variable.Text),
                 "Text data has been supplied but 'SumColumns' requires numeric data.")
    expect_error(SumColumns(variable.Binary, variable.Text),
                 "Text data has been supplied but 'SumColumns' requires numeric data.")
    expect_error(SumColumns(variable.Date, variable.Binary),
                 "Date/Time data has been supplied but 'SumColumns' requires numeric data.")
    expect_error(SumColumns(variable.Time, variable.Numeric),
                 "Date/Time data has been supplied but 'SumColumns' requires numeric data.")
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
                 paste0("'SumColumns' requires all input elements to have the same size to be able ",
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

data(table1D.Average)
data(table1D.Percentage)
data(table.1D.MultipleStatistics)

test_that("Table 1D", {
    expect_equal(SumColumns(table1D.Percentage), 100)
    expect_equal(SumColumns(table.1D.MultipleStatistics),
                 colSums(table.1D.MultipleStatistics[-4,], na.rm = TRUE))
    expect_equal(SumColumns(table.1D.MultipleStatistics,
                            remove.rows = c()),
                colSums(table.1D.MultipleStatistics, na.rm = TRUE))

    expect_equal(SumColumns(table.1D.MultipleStatistics,
                            remove.columns = "z-Statistic"),
                 colSums(table.1D.MultipleStatistics[-4, -5], na.rm = TRUE))

    captured.warnings <- capture_warnings(SumColumns(table.1D.MultipleStatistics,
                                                     remove.columns = "z-Statistic",
                                                     warn = TRUE))
    expect_setequal(captured.warnings,
                   c("These categories have been removed from the columns: z-Statistic.",
                     "These categories have been removed from the rows: SUM.",
                     paste0("The input data contains statistics of different types ",
                            "(i.e., Average, Effective Sample Size, t-Statistic, ",
                            "d.f., Corrected p), it may not be appropriate to compute 'SumColumns'.")))
})

data(table2D.Percentage)
data(table2D.PercentageAndCount)
data(table2D.PercentageNaN)
test_that("Table 2D", {
    expect_equal(SumColumns(table2D.Percentage),
                 colSums(table2D.Percentage[, colnames(table2D.Percentage) != "NET"], na.rm = TRUE))
    expect_equal(SumColumns(table2D.PercentageNaN),
                 colSums(table2D.PercentageNaN[-8, -10], na.rm = TRUE))
    col.summed.2d.table.multi.stats <- cbind(`Row %` = colSums(table2D.PercentageAndCount[, , 1]),
                                             `Count` = colSums(table2D.PercentageAndCount[, , 2]))
    col.summed.2d.table.multi.stats.woithout.net <- col.summed.2d.table.multi.stats[-10, ]
    expect_equal(SumColumns(table2D.PercentageAndCount),
                 col.summed.2d.table.multi.stats.woithout.net)
    expect_equal(SumColumns(table2D.PercentageAndCount, remove.columns = NULL),
                 col.summed.2d.table.multi.stats)
    # Warning for dodgy calculation
    expect_warning(SumColumns(table2D.PercentageAndCount, remove.columns = NULL, warn = TRUE),
                   paste0("The input data contains statistics of different types ",
                          "(i.e., Row %, Count), it may not be appropriate to compute 'SumColumns'."),
                   fixed = TRUE)
    # Extra category removed removed and warn about missing value removal
    expect_equal(expect_warning(output.wo.missing <- SumColumns(table2D.PercentageNaN,
                                                                remove.columns = c("NET", "Feminine"),
                                                                remove.rows = c("NET", "None of these"),
                                                                remove.missing = TRUE,
                                                                warn = TRUE),
                                "Missing values have been ignored in calculation"),
                 colSums(table2D.PercentageNaN[1:6, -c(1, 10)], na.rm = TRUE))
    # Missing values
    expect_true(anyNA(SumRows(table2D.PercentageNaN, remove.missing = FALSE)))
    expect_false(anyNA(output.wo.missing))
})

test_that("Error if incompatible inputs", {
    expect_error(SumColumns(table.1D.MultipleStatistics, table1D.Average),
                 paste0(sQuote("SumColumns"), " does not support multiple inputs unless they are all ",
                        "individual variables or vectors. One of the inputs here is a Q Table. ",
                        "Contact support at opensource@displayr.com or raise an issue at ",
                        "https://github.com/Displayr/verbs if you wish this to be changed."))
})
