context("SumRows")

data(variable.Text)
data(variable.Binary)
data(variable.Nominal)
data(variable.Numeric)
data(variable.Time)
data(variable.Date)
test_that("Variables", {
    expect_error(SumRows(variable.Text),
                 "Text data has been supplied but 'SumRows' requires numeric data.")
    expect_error(SumRows(variable.Numeric, variable.Date),
                 "Date/Time data has been supplied but 'SumRows' requires numeric data.")
    expect_error(SumRows(variable.Binary, variable.Text),
                 "Text data has been supplied but 'SumRows' requires numeric data.")
    vars.as.matrix <- matrix(c(variable.Binary,
                               variable.Numeric,
                               flipTransformations::AsNumeric(variable.Nominal,
                                                              binary = FALSE)),
                             ncol = 3)
    expected.sum.rows.missing.removed <- rowSums(vars.as.matrix, na.rm = TRUE)
    expected.sum.rows.missing.kept <- rowSums(vars.as.matrix)
    expect_equivalent(SumRows(variable.Binary, variable.Numeric, variable.Nominal,
                              remove.missing = TRUE)[TRUE],
                      expected.sum.rows.missing.removed)
    expect_equal(SumRows(variable.Binary, variable.Numeric, variable.Nominal,
                         remove.missing = FALSE)[TRUE],
                 expected.sum.rows.missing.kept)

    # Warnings for factors
    ## No extra warning for variables that are converted using value attributes
    captured.warnings <- capture_warnings(SumRows(variable.Binary,
                                                  variable.Nominal,
                                                  warn = TRUE))
    expect_length(captured.warnings, 1L)
    expect_equal(captured.warnings, "Missing values have been ignored in calculation.")
    ## AsNumeric warning should be appearing when factor converted that has no value attributes
    expect_warning(SumRows(1:5, factor(1:5), warn = TRUE),
                   paste0("Data has been automatically converted to numeric. ",
                          "Values are assigned according to the labels of the ",
                          "categories. To use alternative numeric values, ",
                          "transform the data prior including it in this ",
                          "analysis (e.g. by changing its structure)."),
                   fixed = TRUE)
})


data(table1D.Average)
data(table1D.Percentage)
data(table.1D.MultipleStatistics)

test_that("Table 1D", {
    table.without.most.attr <- as.vector(table1D.Percentage)
    names(table.without.most.attr) <- names(table1D.Percentage)
    expect_equivalent(SumRows(table1D.Percentage, remove.rows = NULL),
                      table.without.most.attr)
    expect_equivalent(SumRows(table1D.Percentage, remove.rows = "NET"),
                      table.without.most.attr[names(table.without.most.attr) != "NET"])
    expect_equivalent(SumRows(table.1D.MultipleStatistics, remove.rows = NULL),
                              c(`Colas (e.g., Coca-Cola, Pepsi Max)?` = Inf, `Sparkling mineral water` = -Inf,
                                Coffee = 670.523888977345, SUM = Inf))
    expect_equivalent(SumRows(table.1D.MultipleStatistics),
                      c(`Colas (e.g., Coca-Cola, Pepsi Max)?` = Inf, `Sparkling mineral water` = -Inf,
                        Coffee = 670.523888977345))
    expect_equal(SumRows(table.1D.MultipleStatistics, remove.rows = NULL,
                         remove.columns = "z-Statistic"),
                 rowSums(table.1D.MultipleStatistics[, colnames(table.1D.MultipleStatistics) != "z-Statistic"]))
    expect_equal(SumRows(table.1D.MultipleStatistics, remove.rows = "SUM",
                         remove.columns = "z-Statistic"),
                 rowSums(table.1D.MultipleStatistics[-4, colnames(table.1D.MultipleStatistics) != "z-Statistic"]))
    captured.warnings <- capture_warnings(SumRows(table.1D.MultipleStatistics,
                                                  remove.rows = "SUM",
                                                  remove.columns = "z-Statistic",
                                                  warn = TRUE))
    expect_setequal(captured.warnings,
                    c(paste0("The input data contains statistics of different types ",
                             "(i.e., Average, Effective Sample Size, t-Statistic, d.f., ",
                             "Corrected p), it may not be appropriate to compute 'SumRows'."),
                      "These categories have been removed from the rows: SUM.",
                      "These categories have been removed from the columns: z-Statistic."))
})

data(table2D.Percentage)
data(table2D.PercentageAndCount)
data(table2D.PercentageNaN)
test_that("Table 2D", {
    expect_equal(SumRows(table2D.Percentage),
                 c(`Coca-Cola` = 100, `Diet Coke` = 100,
                   `Coke Zero` = 100, Pepsi = 100,
                   `Diet Pepsi` = 100, `Pepsi Max` = 100))
    expect_equal(SumRows(table2D.PercentageNaN),
                 rowSums(table2D.PercentageNaN[-8, -10], na.rm = TRUE))
    # Note that while we represent this as a 3D array, from the user's perspective
    # this is a 2D table, where the third dimension is stacked into the rows.
    # The labeling of the rows should show the
    # z1 <- rowSums(table2D.PercentageAndCount[,-10, "Row %"])
    # names(z1) <- paste(names(z1), "Row %")
    # z2 <- rowSums(table2D.PercentageAndCount[,-10, "Count"])
    # names(z2) <- paste(names(z2), "Count")
    # z <- c(z1, z2)[c(1,7,2,8,3,9,4,10,5,11,6,12)]
    # expect_equal(SumRows(table2D.PercentageAndCount), z)
    summary.stat.cols <- colnames(table2D.PercentageAndCount) == "NET"
    row.summed.2d.table.multi.stats <- table2D.PercentageAndCount[, summary.stat.cols, ]
    expect_equal(SumRows(table2D.PercentageAndCount),
                 row.summed.2d.table.multi.stats)
    # Warning for dodgy calculation
    expect_warning(SumRows(table2D.PercentageAndCount, warn = TRUE),
                   paste0("The input data contains statistics of different types ",
                          "(i.e., Row %, Count), it may not be appropriate to compute 'SumRows'."),
                   fixed = TRUE)

    # Extra category removed removed
    expect_equal(SumRows(table2D.PercentageNaN, remove.rows = c("NET", "None of these")),
                 rowSums(table2D.PercentageNaN[-7:-8, -10], na.rm = TRUE))

    # Warning about missing values
    expect_warning(SumRows(table2D.PercentageNaN, warn = TRUE),
                   "Missing values have been ignored in calculation.")


    # Missing values
    expect_true(anyNA(SumRows(table2D.PercentageNaN, remove.missing = FALSE)))
})
#
#
# test_that("Two Q Tables selected: no stats match",
#           {
#               # Show a warning
#           })
#
# test_that("Two Q Tables selected: missing values (exclude by default)")
#
# test_that("Two Q Tables with some unmatched names: error if requested",
#           { # check rownames
#               # check colnames
#
#           })
#
# test_that("Two Q Tables with some unmatched names: warn and continue")
#
# test_that("Two Q Tables with some unmatched names: fuzzy match error")
#
# test_that("Two Q Tables with some unmatched names: fuzzy match warning")
#
# test_that("Two Q Tables with some unmatched names: ignore names requested")
# test_that("Two Q Tables with rows to exclude provided")
#
# test_that("Two Q Tables with columns to exclude provided")
