context("Sum")

# data(variable.Text)
# data(variable.Binary)
# data(variable.Nominal)
# data(variable.Numeric)
# data(variable.Time)
# data(variable.Date)
# test_that("Variables",
#           {
#               expect_error(SumColumns(variable.Text),
#                            "It is not possible to 'Sum' Text")
#               expect_error(SumRows(variable.Binary, variable.Text),
#                            "It is not possible to 'Sum' Text")
#               expect_error(SumRows(variable.Date, variable.Text),
#                            "'It is not possible to 'Sum' dates.")
#               expect_error(SumRows(variable.Time, variable.Text),
#                            "'It is not possible to 'Sum' dates/times.")
#               expect_equal(SumColumns(variable.Nominal), Age = 12606)
#               expect_equal(SumColumns(variable.Binary, variable.Numeric, variable.Nominal),
#                            c("Coca-Cola" = 155, Age = 12606, Age = 12606))
#
#
#               ## Warnings for factors
#               # No warning
#               expect_warning(SumColumns(variable.Binary, variable.Nominal, warn = TRUE), NA)
#               # AsNumeric warning should be appearing
#               expect_warning(SumColumns(1:5, factor(1:5), warn = TRUE), "blah")
#
#               # Missing values
#               expect_equal(SumColumns(variable.Binary, variable.Numeric, variable.Nominal,
#                                       ignore.missing = FALSE),
#                            c("Coca-Cola" = NA, Age = NA, Age = NA))
#               expect_warning(SumColumns(variable.BinarySum, warn = TRUE),
#                              "Missing values have been ignored in calculation.")
#           })
#
# test_that("Variables with weights, filters (subet), and a combination of the two",
#           {
#           })
#
#
# data(table1D.Average)
# data(table1D.Percentage)
# data(table.1D.MultipleStatistics)
#
# test_that("Table 1D",
#           {
#               expect_equal(SumColumns(table1D.Percentage), 100)
#               expect_true(SumColumns(table.1D.MultipleStatistics,
#                                      sumColumns(table.1D.MultipleStatistics[-4,],
#                                                 na.rm = TRUE)))
#               expect_true(SumColumns(table.1D.MultipleStatistics,
#                                      remove.rows = c()),
#                                      sumColumns(table.1D.MultipleStatistics,
#                                                 na.rm = TRUE))
#
#               expect_equal(SumColumns(table.1D.MultipleStatistics,
#                                         remove.columns = "z-Statistic"),
#                              colSums(table.1D.MultipleStatistics[-4, -5], na.rm = TRUE))
#
#               expect_warning(SumColumns(table.1D.MultipleStatistics,
#                                         remove.columns = "z-Statistic",
#                                         warn = TRUE),
#                   "These categories have beeen removed from the columns: z-Statistic.")
#
#               expect_warning(SumColumns(table.1D.MultipleStatistics,
#                                         remove.columns = "z-Statistic",
#                                         warn = TRUE),
#                              "These categories have beeen removed from the rows: SUM.")
#
#               expect_warning(SumColumns(table.1D.MultipleStatistics,
#                                         remove.columns = "z-Statistic",
#                                         warn = TRUE),
#                              "These categories have beeen removed from the columns: z-Statistic.")
#
#
#           })
#
# data(table2D.Percentage)
# data(table2D.PercentageAndCount)
# data(table2D.PercentageNaN)
# test_that("Table 2D",
#           {
#           })
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
#             # check colnames
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
