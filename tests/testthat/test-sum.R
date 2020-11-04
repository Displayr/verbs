context("Sum")
# data(variable.Text)
# data(variable.Binary)
# data(variable.Nominal)
# data(variable.Numeric)
# data(variable.Time)
# data(variable.Date)
# test_that("Variables",
#           {
#             expect_error(Sum(variable.Text), "'Sum' requires numeric data.")
#             expect_equal(Sum(variable.Binary), 155)
#             expect_equal(Sum(variable.Numeric), 12606)
#             expect_error(Sum(variable.Date), "It is not possible to 'Sum' dates.") # Not that means and the like are defined
#             expect_error(Sum(variable.Time), "It is not possible to 'Sum' date-times") # Not that means and the like are defined
#
#             ## Factors
#             # With value attributes
#             expect_equal(Sum(variable.Nominal), 12606)
#             expect_warning(Sum(variable.Nominal), NA)
#             # Without value attributes
#             expect_equal(Sum(factor(1:10)), sum(1:10))
#             expect_warning(Sum(variable.Nominal), "blah") #We should be throwing the AsNumeric warning
#
#             # Warnings about missing values
#             expect_warning(Sum(variable.Binary, warn = TRUE),
#                            "Missing values have been ignored in calculation.")
#
#             # Missing values in calculations
#             expect_true(is.na(Sum(variable.Binary, ignore.missing = FALSE)))
#             expect_true(is.na(Sum(variable.Numeric, ignore.missing = FALSE)))
#
#             # Multiple variables
#             expect_equal(Sum(variable.Binary, variable.Numeric), 155 + 12606)
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
# test_that("Table 1D",
# {
#     expect_equal(Sum(table1D.Percentage), 100)
#     expect_true(is.na(Sum(table.1D.MultipleStatistics)))
#
#     expect_warning(Sum(table.1D.MultipleStatistics, warn = TRUE),
#                  "Sum cannot be computed as the data contains both Inf and -Inf.")
#
#     # Warning for categories removed
#     expect_warning(Sum(table1D.Average), NA)
#     expect_warning(Sum(table1D.Average, warn = TRUE),
#                    "These categories have beeen removed from the rows: SUM.")
#
#     # Removal of row categories in a 1D table
#     expect_equal(Sum(table1D.Average), sum(table1D.Average[1:3]))
#     expect_equal(Sum(table1D.Average,
#                      remove.rows = NULL), sum(table1D.Average[1:4]))
#
#     # Missing values
#     z = table1D.Average
#     z[2] = NA
#     expect_equal(Sum(z), sum(z[1:3], na.rm = TRUE))
#     expect_true(is.na(Sum(z, remove.missing = FALSE)))
#
# })
#
# data(table2D.Percentage)
# data(table2D.PercentageAndCount)
# data(table2D.PercentageNaN)
# test_that("Table 2D",
# {
#     # These tests will fail until ignore.columns is implemented
#     expect_equal(Sum(table2D.Percentage), 600)
#     expect_equal(Sum(table2D.PercentageNaN),
#                  sum(table2D.PercentageNaN[-8, -10], na.rm = TRUE))
#     # Note that while we represent this as a 3D array, from the user's perspective
#     # this is a 2D table, where the third dimension is stacked into the rows.
#     expect_equal(Sum(table2D.PercentageAndCount), 2562)
#
#     # Warning for dodgy calculation
#     expect_warning(Sum(table2D.PercentageAndCount,
#                        warn = TRUE), "The input data may contains statistics of different types (i.e., Row %, Count), it may not be appropriate to compute their 'Sum'.")
#
#     # Extra category removed removed
#     expect_error(Sum(table2D.PercentageNaN, remove.rows = c("NET", "None of these")),
#                  sum(table2D.PercentageNaN[-7:-8, -10], na.rm = TRUE))
#
#     # Missing values
#     expect_true(is.na(SumRows(table2D.PercentageNaN, remove.missing = FALSE)))
# })
#
#
# test_that("Works with more than two Q Tables")
#
# test_that("One Q Table with one matrix/array/vector (non-Q Table)")
#
# test_that("Multiple Tables",
# {"E.g., Two Q Tables selected: throw a warning if the stats don't match (if known)"})
#
#
# test_that("Sum matrix and vector",
# {
# ## n x m + n x 1 works
# ## n x m + 1 x m works
# ## else error
# ## respects argument specifying how to match names
# })
#
# test_that("Summing list objects (e.g. model fits) and other R Outputs",
# { ## extracts ChartData and calls Sum again
#   ## Multiple regression/machine learning outputs could create ensemble?
#
# })
#
# test_that("A single R Output (e.g. a vanilla matrix or vector) selected",
# {
# ## tries to calls sum() and returns scalar
# })
#
