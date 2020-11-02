context("Sum")

data(variable.Text)
data(variable.Binary)
data(variable.Nominal)
data(variable.Numeric)
data(variable.Time)
data(variable.Date)
test_that("Variables",
          {
              # Note that with the variables, we're usign the generic 'Sum' rather than
              # SumRows in the errors.
              expect_error(SumRows(variable.Text),
                           "It is not possible to 'Sum' text")
              expect_error(SumRows(variable.numeric, variable.Date),
                           "It is not possible to 'Sum' dates.")
              expect_error(SumRows(variable.numeric, variable.Date),
                           "It is not possible to 'Sum' date-times")
              expect_error(SumRows(variable.Binary, variable.Text),
                           "It is not possible to 'Sum' text")
              expect_equal(SumRows(variable.Binary, variable.Numeric, variable.Nominal)[1:20],
                           c(54, 94, 54, 55, 120, 120, 104, 1, 154, 94, 95, 121, 120, 64,
                             154, 55, 64, 105, 1, 42))

              expect_equal(SumRows(variable.Binary, variable.Numeric, variable.Nominal,
                                   ignore.missing = FALSE)[1:20],
                           c(NA, 94, 54, 55, 120, 120, 104, NA, 154, 94, 95, 121, 120, 64,
                             154, 55, 64, 105, NA, 42))

              ## Warnings for factors
              # No warning
              expect_warning(SumRows(variable.Binary, variable.Nominal, warn = TRUE), NA)
              # AsNumeric warning should be appearing
              expect_warning(SumRows(1:5, factor(1:5), warn = TRUE), "blah")

          })


data(table1D.Average)
data(table1D.Percentage)
data(table.1D.MultipleStatistics)

test_that("Table 1D",
          {
              expect_equal(SumRows(table1D.Percentage),
                           c(`18 to 24` = 13.4556574923547,
                             `25 to 29` = 11.9266055045872,
                             `30 to 34` = 10.0917431192661,
                             `35 to 39` = 11.0091743119266,
                             `40 to 44` = 10.7033639143731,
                             `45 to 49` = 8.25688073394496,
                             `50 to 54` = 12.2324159021407,
                             `55 to 64` = 15.5963302752294,
                             `65 or more` = 6.72782874617737,
                             NET = 100))
              expect_true(SumRows(table.1D.MultipleStatistics,
                                  `Colas (e.g., Coca-Cola, Pepsi Max)?` = Inf, `Sparkling mineral water` = -Inf,
                                  Coffee = 670.523888977345, SUM = Inf))

              expect_equal(SumRows(table.1D.MultipleStatistics,
                                   remove.columns = "z-Statistic"),
                           rowSums(table.1D.MultipleStatistics[, -5]))

              expect_warning(Sum(table1D.Average, warn = TRUE),
                             "These categories have beeen removed from the columns: z-Statistic.")


          })

data(table2D.Percentage)
data(table2D.PercentageAndCount)
data(table2D.PercentageNaN)
test_that("Table 2D",
          {
              # These tests will fail until ignore.columns is implemented
              expect_equal(SumRows(table2D.Percentage),
                           c(`Coca-Cola` = 100, `Diet Coke` = 100,
                             `Coke Zero` = 100, Pepsi = 100,
                             `Diet Pepsi` = 100, `Pepsi Max` = 100))

              expect_equal(SumRows(table2D.PercentageNaN),
                           rowSums(table2D.PercentageNaN[-8, -10], na.rm = TRUE))
              # Note that while we represent this as a 3D array, from the user's perspective
              # this is a 2D table, where the third dimension is stacked into the rows.
              # The labeling of the rows should show the
              z1 <- rowSums(table2D.PercentageAndCount[,-10, "Row %"])
              names(z1) <- paste(names(z1), "Row %")
              z2 <- rowSums(table2D.PercentageAndCount[,-10, "Count"])
              names(z2) <- paste(names(z2), "Row %")
              z <- c(z1, z2)[c(1,7,2,8,3,9,4,10,5,11,6,12)]
              expect_equal(SumRows(table2D.PercentageAndCount), z)

              # Warning for dodgy calculation
              expect_warning(SumRows(table2D.PercentageAndCount,
                                     warn = TRUE), "The input data may contains statistics of different types (i.e., Row %, Count), it may not be appropriate to compute their 'Sum'.")

              # Extra category removed removed
              expect_error(SumRows(table2D.PercentageNaN, remove.rows = c("NET", "None of these")),
                           rowSums(table2D.PercentageNaN[-7:-8, -10], na.rm = TRUE))

              # Warning about missing values
              expect_warning(SumRows(table2D.PercentageNaN, warn = TRUE),
                             "Missing values have been ignored in calculation.")


              # Missing values
              expect_true(is.na(SumRows(table2D.PercentageNaN, remove.missing = FALSE)))
          })


test_that("Two Q Tables selected: no stats match",
          {
              # Show a warning
          })

test_that("Two Q Tables selected: missing values (exclude by default)")

test_that("Two Q Tables with some unmatched names: error if requested",
          { # check rownames
              # check colnames

          })

test_that("Two Q Tables with some unmatched names: warn and continue")

test_that("Two Q Tables with some unmatched names: fuzzy match error")

test_that("Two Q Tables with some unmatched names: fuzzy match warning")

test_that("Two Q Tables with some unmatched names: ignore names requested")
test_that("Two Q Tables with rows to exclude provided")

test_that("Two Q Tables with columns to exclude provided")
