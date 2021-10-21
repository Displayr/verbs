context("SumRows")

load("variable.Text.rda")
load("variable.Binary.rda")
load("variable.Nominal.rda")
load("variable.Numeric.rda")
load("variable.Time.rda")
load("variable.Date.rda")

if (flipU::IsRServer())
{
    contact.msg <- "support@displayr.com if you wish this to be changed."
} else
    contact.msg <- paste0("opensource@displayr.com or raise an issue ",
                          "at https://github.com/Displayr/verbs if you wish this to be changed.")

quoted.function <- sQuote("SumRows")

rowSumsNaAdjusted <- function(x, remove.missing = TRUE)
{
    if (!remove.missing)
        return(rowSums(x))
    y <- rowSums(x, na.rm = TRUE)
    if (any(all.missing <- apply(x, 1L, allNA)))
        y[all.missing] <- NA
    y
}

test_that("Variables", {
    text.error <- capture_error(throwErrorInvalidDataForNumericFunc("Text", quoted.function))[["message"]]
    expect_error(SumRows(variable.Text), text.error, fixed = TRUE)
    datetime.error <- capture_error(throwErrorInvalidDataForNumericFunc("Date/Time", quoted.function))[["message"]]
    expect_error(SumRows(variable.Date), datetime.error)
    numeric.var.expected <- as.vector(variable.Numeric)
    numeric.var.expected.wo.missing <- numeric.var.expected
    expect_equal(SumRows(variable.Numeric, remove.missing = FALSE), numeric.var.expected)
    expect_equal(SumRows(variable.Numeric, remove.missing = TRUE), numeric.var.expected.wo.missing)
    nominal.var.expected <- as.vector(flipTransformations::AsNumeric(variable.Nominal, binary = FALSE))
    nominal.var.expected.wo.missing <- nominal.var.expected
    expect_equal(SumRows(variable.Nominal, remove.missing = FALSE), nominal.var.expected)
    expect_equal(SumRows(variable.Nominal, remove.missing = TRUE), nominal.var.expected.wo.missing)
    binary.var.expected <- as.vector(variable.Binary)
    binary.var.expected.wo.missing <- binary.var.expected
    expect_equal(SumRows(variable.Binary, remove.missing = FALSE), binary.var.expected)
    expect_equal(SumRows(variable.Binary, remove.missing = TRUE), binary.var.expected.wo.missing)
    df <- data.frame(variable.Numeric, variable.Date)
    expect_error(SumRows(df), datetime.error)
    df <- data.frame(variable.Binary, variable.Text)
    expect_error(SumRows(df), text.error)
    nominal.to.numeric <- flipTransformations::AsNumeric(variable.Nominal,
                                                         binary = FALSE)
    vars.in.df <- data.frame(variable.Binary,
                             variable.Numeric,
                             nominal.to.numeric)
    expected.2.sum.rows.missing.removed <- setNames(rowSumsNaAdjusted(vars.in.df[-3],
                                                                      remove.missing = TRUE),
                                                    1:nrow(vars.in.df))
    expected.2.sum.rows.missing.kept <- setNames(rowSums(vars.in.df[-3]),
                                                 1:nrow(vars.in.df))
    expect_equal(SumRows(vars.in.df[-3], remove.missing = TRUE),
                 expected.2.sum.rows.missing.removed)
    expect_equal(SumRows(vars.in.df[-3], remove.missing = FALSE),
                 expected.2.sum.rows.missing.kept)
    expected.3.sum.rows.missing.removed <- setNames(rowSumsNaAdjusted(vars.in.df,
                                                                      remove.missing = TRUE),
                                                    1:nrow(vars.in.df))
    expected.3.sum.rows.missing.kept <- setNames(rowSumsNaAdjusted(vars.in.df, remove.missing = FALSE),
                                                 1:nrow(vars.in.df))
    expect_equal(SumRows(vars.in.df, remove.missing = TRUE),
                 expected.3.sum.rows.missing.removed)
    expect_equal(SumRows(vars.in.df, remove.missing = FALSE),
                 expected.3.sum.rows.missing.kept)
    # Warnings for factors
    ## No extra warning for variables that are converted using value attributes
    df <- data.frame(variable.Binary, variable.Nominal)
    captured.warnings <- capture_condition(SumRows(df, warn = TRUE))
    missing.value.warning <- capture_condition(warnAboutMissingValuesIgnored())
    expect_equal(captured.warnings, missing.value.warning)
    factor.values.warning <- capture_warnings(flipTransformations::AsNumeric(factor(1:2), binary = FALSE))
    ## AsNumeric warning should be appearing when factor converted that has no value attributes
    expect_warning(SumRows(data.frame(1:5, factor(1:5)), warn = TRUE),
                   factor.values.warning,
                   fixed = TRUE)
})

load("table1D.Average.rda")
load("table1D.Percentage.rda")
load("table.1D.MultipleStatistics.rda")

test_that("Table 1D", {
    expect_equivalent(SumRows(table1D.Percentage), table1D.Percentage)
    expect_equal(SumRows(table.1D.MultipleStatistics), rowSums(table.1D.MultipleStatistics))
    expected.output <- rowSums(table.1D.MultipleStatistics[, -which(colnames(table.1D.MultipleStatistics) == "z-Statistic")])
    expect_equal(SumRows(table.1D.MultipleStatistics,
                         remove.columns = "z-Statistic"),
                 expected.output)
    stats.used <- colnames(table.1D.MultipleStatistics)
    stats.used <- stats.used[stats.used != "z-Statistic"]
    diff.stats.warning <- capture_warnings(throwWarningAboutDifferentStatistics(stats.used, quoted.function))
    captured.warnings <- capture_warnings(SumRows(table.1D.MultipleStatistics,
                                                  remove.columns = "z-Statistic",
                                                  warn = TRUE))
    expect_setequal(captured.warnings, diff.stats.warning)
})

load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")
test_that("Table 2D", {
    expected.2d.row.sums <- rowSums(table2D.Percentage[, -10])
    expect_equal(SumRows(table2D.Percentage), expected.2d.row.sums)
    expected.2d.row.sums <- rowSums(table2D.PercentageNaN[, -10], na.rm = TRUE)
    expect_equal(SumRows(table2D.PercentageNaN), expected.2d.row.sums)
    summary.stat.cols <- colnames(table2D.PercentageAndCount) == "NET"
    row.summed.2d.table.multi.stats <- table2D.PercentageAndCount[, summary.stat.cols, ]
    expect_equal(SumRows(table2D.PercentageAndCount),
                 row.summed.2d.table.multi.stats)
    # Warning about missing values
    missing.value.warning <- capture_condition(warnAboutMissingValuesIgnored())[["message"]]
    expect_condition(SumRows(table2D.PercentageNaN, warn = TRUE), missing.value.warning)
    # Missing values
    expect_true(anyNA(SumRows(table2D.PercentageNaN, remove.missing = FALSE)))
    expect_false(anyNA(SumRows(table2D.PercentageNaN)))
    # Test subsetted 2D QTable with multiple statistics to a single statistic
    ## i.e. the case when the dims are a 3d array with (n, p, 1)
    subsetted.qtable <- table2D.PercentageAndCount[, , 1, drop = FALSE]
    subsetted.qtable <- CopyAttributes(subsetted.qtable, table2D.PercentageAndCount)
    expect_equal(SumRows(subsetted.qtable),
                 rowSums(table2D.PercentageAndCount[, -10, 1], na.rm = TRUE))
    # Check opposite infinities
    table.opp.inf <- table.1D.MultipleStatistics
    table.opp.inf[1, 1] <- -Inf
    expected.out <- rowSums(table.opp.inf)
    captured.warnings <- capture_warnings(expect_equal(SumRows(table.opp.inf, warn = TRUE),
                                                       expected.out))
    diff.stats.warning <- capture_warnings(throwWarningAboutDifferentStatistics(colnames(table.1D.MultipleStatistics),
                                                                                quoted.function))
    some.opposite.infinities.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE), quoted.function))
    all.opposite.infinities.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, TRUE), quoted.function))
    expect_setequal(captured.warnings, c(diff.stats.warning, some.opposite.infinities.warning))
    table.opp.inf[, 1] <- Inf * c(-1, 1, 1, 1)
    table.opp.inf[3:4, 2] <- -Inf
    expected.out <- rowSums(table.opp.inf)
    captured.warnings <- capture_warnings(expect_equal(SumRows(table.opp.inf, warn = TRUE),
                                                       expected.out))
    expect_setequal(captured.warnings, c(diff.stats.warning, all.opposite.infinities.warning))
    df <- data.frame(x = runif(5), y = runif(5))
    expect_warning(checkOppositeInifinitiesByRow(rowSums(df), df, function.name = "foo"),
                   NA)
    df[1, ] <- c(Inf, -Inf)
    single.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE),
                                                                   function.name = "foo"))
    expect_warning(checkOppositeInifinitiesByRow(rowSums(df), df, function.name = "foo"),
                   single.warning)
    df[2:5] <- c(Inf, -Inf)
    all.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, TRUE),
                                                                function.name = "foo"))
    expect_warning(checkOppositeInifinitiesByRow(rowSums(df), df, function.name = "foo"),
                   all.warning)
    fake.qtable <- array(1:24, dim = 4:2, dimnames = list(1:4, letters[1:3], LETTERS[1:2]))
    attr(fake.qtable, "questions") <- "Foo"
    expect_warning(checkOppositeInifinitiesByRow(sumRows(fake.qtable, remove.missing = FALSE),
                                                 fake.qtable, "foo"), NA)
    fake.qtable[1, , 1] <- c(Inf, 1, -Inf)
    expect_warning(checkOppositeInifinitiesByRow(sumRows(fake.qtable, remove.missing = FALSE),
                                                 fake.qtable, "foo"),
                   single.warning)
    fake.qtable <- array(rep(c(Inf, -Inf), each = 4), dim = c(4, 2, 2),
                         dimnames = list(NULL, letters[1:2], LETTERS[1:2]))
    attr(fake.qtable, "questions") <- "Foo"
    expect_warning(checkOppositeInifinitiesByRow(sumRows(fake.qtable, remove.missing = FALSE),
                                                 fake.qtable, "foo"),
                   all.warning)

})

test_that("Higher dim Q tables", {
    load("numeric.grid.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.with.multiple.stats.qtable
    expect_equal(SumRows(curr.table),
                 apply(curr.table[, -which(dimnames(curr.table)[[2L]] == "SUM"), ], c(1, 3), sum, na.rm = TRUE))
    load("numeric.grid.nominal.qtable.rda")
    curr.table <- numeric.grid.nominal.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.col.names <- dimnames(flattened.table)[[2L]]
    expect_equal(SumRows(curr.table),
                 rowSums(flattened.table[, flat.col.names != "SUM"],
                         na.rm = TRUE))
    load("numeric.grid.nominal.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.nominal.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    expect_equal(SumRows(curr.table),
                 apply(flattened.table, c(1, 3), sum,
                       na.rm = TRUE))
    load("nominal.multi.nominal.qtable.rda")
    curr.table <- nominal.multi.nominal.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    expect_equal(SumRows(curr.table),
                 rowSums(flattened.table, na.rm = TRUE))
    load("nominal.multi.nominal.with.multiple.stats.qtable.rda")
    curr.table <- nominal.multi.nominal.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    expect_equal(SumRows(curr.table),
                 apply(flattened.table, c(1, 3), sum, na.rm = TRUE))
    load("nominal.multi.nominal.multi.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    expect_equal(SumRows(curr.table),
                 rowSums(flattened.table, na.rm = TRUE))
    load("nominal.multi.nominal.multi.with.multiple.stats.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    expect_equal(SumRows(curr.table),
                 apply(flattened.table, c(1, 3), sum, na.rm = TRUE))
})

test_that("Q Tables: Check warning of different statistics thrown or suppressed", {
    # Matching statistics (No warnings)
    # warning already suppressed by default
    expected.out <- rowSums(table.1D.MultipleStatistics)
    # Don't warn when default warn = FALSE
    expect_equal(SumRows(table.1D.MultipleStatistics), expected.out)
    diff.stat.warning <- capture_warnings(throwWarningAboutDifferentStatistics(colnames(table.1D.MultipleStatistics),
                                                                               sQuote('SumRows')))
    captured.warnings <- capture_warnings(expect_equal(SumRows(table.1D.MultipleStatistics, warn = TRUE),
                                                       expected.out))
    expect_setequal(captured.warnings, diff.stat.warning)
    # No warning even if warn = TRUE when only a single statistic
    expect_equivalent(SumRows(table1D.Average),
                      table1D.Average)
    expected.warning <- capture_warnings(throwWarningAboutCalculationWithSingleElement(table1D.Average, 2L, quoted.function))
    expect_warning(sum.output <- SumRows(table1D.Average, warn = TRUE),
                   expected.warning)
    expect_equivalent(sum.output, table1D.Average)
    expect_warning(SumRows(data.frame(x = runif(10L)), warn = TRUE),
                   expected.warning)
    expected.out <- rowSums(table2D.Percentage)
    expect_equal(SumRows(table2D.Percentage, remove.columns = NULL),
                 expected.out)
})

test_that("A single R Output (e.g. a vanilla matrix or vector) selected", {
    ## tries to calls sum() and returns scalar
    matrix.1 <- matrix(1:24, nrow = 6)
    expect_equal(SumRows(matrix.1), rowSums(matrix.1))
    vector.1 <-1:24
    expect_equal(SumRows(vector.1), vector.1)
    # Don't support higher arrays
    array.1 <- array(1:504, dim = 7:9)
    higher.dim.array.error <- capture_error(throwErrorAboutHigherDimArray(3L, quoted.function))[["message"]]
    expect_error(SumRows(array.1), higher.dim.array.error)
})

test_that("Column names conflicting with function argument names won't cause an error", {
    argument.names <- formalArgs(Sum)[-1] # Don't bother with the ... argument
    input <- replicate(length(argument.names), runif(10))
    colnames(input) <- argument.names
    expect_equal(SumRows(input), rowSums(input))
})

test_that("SumEachRow alias working", {
    expect_equal(SumEachRow, SumRows)
    expect_equal(SumRows(table2D.Percentage),
                 SumEachRow(table2D.Percentage))
})

test_that("Handling of NAs", {
    expect_equal(SumRows(array(NA, dim = c(5L, 1L)), remove.missing = TRUE, warn = FALSE),
                 rep(NA_integer_, 5L))
    expect_equal(SumRows(NA, remove.missing = FALSE),
                 NA_integer_)
    expect_equal(SumRows(NA, remove.missing = TRUE),
                 NA_integer_)
    expect_equal(SumRows(array(c(1:3, NA), dim = c(4L, 1L))), c(1:3, NA))
    n <- 10L
    dat.with.missing <- as.data.frame(replicate(n, runif(n), simplify = FALSE))
    names(dat.with.missing) <- letters[1:ncol(dat.with.missing)]
    all.na.rows <- 1:n %in% sample.int(nrow(dat.with.missing), size = n/2)
    dat.with.missing[all.na.rows, ] <- NA
    dat.with.missing[!all.na.rows, 1L] <- NA
    expect_equal(SumRows(dat.with.missing),
                 setNames(rowSumsNaAdjusted(dat.with.missing, remove.missing = TRUE),
                          1:n))
    expect_equal(SumRows(dat.with.missing, remove.missing = FALSE),
                 setNames(rowSumsNaAdjusted(dat.with.missing, remove.missing = FALSE),
                          1:n))
})

test_that("Warnings muffled", {
    # Not show the missing value warning
    input.array <- array(1:12, dim = 3:4, dimnames = list(LETTERS[1:3], NULL))
    is.na(input.array) <- 1:3
    expect_equal(SumRows(input.array, warn = "Foo"), rowSums(input.array[, -1L]))
})
