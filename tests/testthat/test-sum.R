context("Sum")

test_that("One Q Table selected: error",
{  # replicates behaviour of Q: Insert Ready-Made Formula
   # suggests SumRows or SumColumns
})

test_that("Two Q Tables selected: stats and dimensions match")

test_that("Two Q Tables selected: some stats match",
{  # e.g. 1D array/Q Table with 2D array/Q Table with multiple stats
   # e.g. 2D array/Q Table crosstab with 3D array/Q Table crosstab with mutliple stats

})

test_that("Two Q Tables selected: no stats match")

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

test_that("Two Q Tables with subset argument/QFilter")

test_that("Two Q Tables with weight argument/QPopulatonWeight")

test_that("Works with more than two Q Tables")

test_that("One Q Table with one matrix/array/vector (non-Q Table)")

test_that("Single-variable Variable Set input: throws error (as in Q)")

test_that("One multi-variable Variable Set input: sums within case")

test_that("Sum two variable sets: error if different data sets/number of cases")

test_that("Two variable sets: works with underlying values")

test_that("Two variable sets: works with data reductions")

test_that("Two variable sets: missing values excluded/included")

test_that("Two variable sets: subset/QFilter")

test_that("Two variable sets: weight/QPopulatonWeight")

test_that("Sum three or more variable sets")

test_that("Sum matrix and vector",
{
## n x m + n x 1 works
## n x m + 1 x m works
## else error
## respects argument specifying how to match names
})

test_that("Summing list objects (e.g. model fits) and other R Outputs",
{ ## extracts ChartData and calls Sum again
  ## Multiple regression/machine learning outputs could create ensemble?

})

test_that("A single R Output (e.g. a vanilla matrix or vector) selected",
{
## tries to calls sum() and returns scalar
})




