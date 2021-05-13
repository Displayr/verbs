context("AverageEachRow")

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

quoted.function <- sQuote("AverageEachRow")

test_that("Variables", {
    text.error <- capture_error(throwErrorInvalidDataForNumericFunc("Text", quoted.function))[["message"]]
    datetime.error <- capture_error(throwErrorInvalidDataForNumericFunc("Date/Time", quoted.function))[["message"]]
    expect_error(AverageEachRow(variable.Text), text.error)
    expect_error(AverageEachRow(variable.Date), datetime.error)
    numeric.var.expected <- as.vector(variable.Numeric)
    numeric.var.expected.wo.missing <- numeric.var.expected
    numeric.var.expected.wo.missing[is.na(numeric.var.expected.wo.missing)] <- NaN
    expect_equal(AverageEachRow(variable.Numeric, remove.missing = FALSE), numeric.var.expected)
    expect_equal(AverageEachRow(variable.Numeric, remove.missing = TRUE), numeric.var.expected.wo.missing)
    nominal.var.expected <- as.vector(flipTransformations::AsNumeric(variable.Nominal, binary = FALSE))
    nominal.var.expected.wo.missing <- nominal.var.expected
    nominal.var.expected.wo.missing[is.na(nominal.var.expected.wo.missing)] <- NaN
    expect_equal(AverageEachRow(variable.Nominal, remove.missing = FALSE), nominal.var.expected)
    expect_equal(AverageEachRow(variable.Nominal, remove.missing = TRUE), nominal.var.expected.wo.missing)
    binary.var.expected <- as.vector(variable.Binary)
    binary.var.expected.wo.missing <- binary.var.expected
    binary.var.expected.wo.missing[is.na(binary.var.expected.wo.missing)] <- NaN
    expect_equal(AverageEachRow(variable.Binary, remove.missing = FALSE), binary.var.expected)
    expect_equal(AverageEachRow(variable.Binary, remove.missing = TRUE), binary.var.expected.wo.missing)
    df <- data.frame(variable.Numeric, variable.Date)
    expect_error(AverageEachRow(df), datetime.error)
    df <- data.frame(variable.Binary, variable.Text)
    expect_error(AverageEachRow(df), text.error)
    nominal.to.numeric <- flipTransformations::AsNumeric(variable.Nominal,
                                                         binary = FALSE)
    vars.as.matrix <- matrix(c(variable.Binary,
                               variable.Numeric,
                               nominal.to.numeric),
                             ncol = 3,
                             dimnames = list(1:length(variable.Binary), NULL))
    expected.2.sum.rows.missing.removed <- setNames(rowSums(vars.as.matrix[, -3], na.rm = TRUE),
                                                    1:length(variable.Binary))
    n.sum <- apply(!is.na(vars.as.matrix[, -3]), 1L, sum)
    expected.2.sum.rows.missing.kept <- setNames(rowSums(vars.as.matrix[, -3]),
                                                 1:length(variable.Binary))
    df <- data.frame(variable.Binary, variable.Numeric)
    expect_equal(AverageEachRow(df, remove.missing = TRUE),
                 expected.2.sum.rows.missing.removed / n.sum)
    df <- data.frame(variable.Binary, variable.Numeric)
    expect_equal(AverageEachRow(df, remove.missing = FALSE),
                 expected.2.sum.rows.missing.kept / n.sum)
    expected.3.sum.rows.missing.removed <- rowSums(vars.as.matrix, na.rm = TRUE)
    expected.3.sum.rows.missing.kept <- rowSums(vars.as.matrix)
    n.sum <- apply(!is.na(vars.as.matrix), 1L, sum)
    expect_equal(AverageEachRow(data.frame(variable.Binary, variable.Numeric, variable.Nominal),
                             remove.missing = TRUE),
                 expected.3.sum.rows.missing.removed / n.sum)
    expect_equal(AverageEachRow(data.frame(variable.Binary, variable.Numeric, variable.Nominal),
                             remove.missing = FALSE),
                 expected.3.sum.rows.missing.kept / n.sum)
    # Warnings for factors
    ## No extra warning for variables that are converted using value attributes
    captured.warnings <- capture_warnings(AverageEachRow(data.frame(variable.Binary,
                                                                 variable.Nominal),
                                                      warn = TRUE))
    missing.value.warning <- capture_warnings(throwWarningAboutMissingValuesIgnored())
    expect_length(captured.warnings, 1L)
    expect_equal(captured.warnings, missing.value.warning)
    ## AsNumeric warning should be appearing when factor converted that has no value attributes
    factor.conv.warning <- capture_warnings(flipTransformations::AsNumeric(factor(1:2), binary = FALSE))
    expect_warning(AverageEachRow(data.frame(1:5, factor(1:5)), warn = TRUE),
                   factor.conv.warning, fixed = TRUE)
    expected.warning <- capture_warning(throwWarningAboutCalculationWithSingleElement(variable.Nominal, 2L, quoted.function))[["message"]]
    expect_warning(AverageEachRow(variable.Nominal, warn = TRUE), expected.warning)
})

load("table1D.Average.rda")
load("table1D.Percentage.rda")
load("table.1D.MultipleStatistics.rda")

test_that("Table 1D", {
    expect_equivalent(AverageEachRow(table1D.Percentage), table1D.Percentage)
    expect_equal(AverageEachRow(table.1D.MultipleStatistics), rowMeans(table.1D.MultipleStatistics))
    expected.output <- rowMeans(table.1D.MultipleStatistics[, -which(colnames(table.1D.MultipleStatistics) == "z-Statistic")])
    expect_equal(AverageEachRow(table.1D.MultipleStatistics,
                         remove.columns = "z-Statistic"),
                 expected.output)
    captured.warnings <- capture_warnings(AverageEachRow(table.1D.MultipleStatistics,
                                                  remove.columns = "z-Statistic",
                                                  warn = TRUE))
    diff.stats <- colnames(table.1D.MultipleStatistics)
    diff.stats <- diff.stats[diff.stats != "z-Statistic"]
    expected.warning <- capture_warnings(throwWarningAboutDifferentStatistics(diff.stats, quoted.function))
    expect_setequal(captured.warnings,
                    expected.warning)
})

load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")
test_that("Table 2D", {
    missing.value.warning <- capture_warnings(throwWarningAboutMissingValuesIgnored())
    expected.2d.row.sums <- rowMeans(table2D.Percentage[, -10])
    expect_equal(AverageEachRow(table2D.Percentage), expected.2d.row.sums)
    expected.2d.row.sums <- rowMeans(table2D.PercentageNaN[, -10], na.rm = TRUE)
    expect_equal(AverageEachRow(table2D.PercentageNaN), expected.2d.row.sums)
    summary.stat.cols <- colnames(table2D.PercentageAndCount) == "NET"
    row.summed.2d.table.multi.stats <- table2D.PercentageAndCount[, summary.stat.cols, ]
    expect_equal(AverageEachRow(table2D.PercentageAndCount),
                 row.summed.2d.table.multi.stats / sum(!summary.stat.cols))
    # Warning about missing values
    expect_warning(AverageEachRow(table2D.PercentageNaN, warn = TRUE), missing.value.warning)
    # Missing values
    expect_true(anyNA(AverageEachRow(table2D.PercentageNaN, remove.missing = FALSE)))
    expect_false(anyNA(AverageEachRow(table2D.PercentageNaN)))
    # Test subsetted 2D QTable with mulitple statistics to a single statistic
    ## i.e. the case when the dims are a 3d array with (n, p, 1)
    subsetted.qtable <- table2D.PercentageAndCount[, , 1, drop = FALSE]
    subsetted.qtable <- CopyAttributes(subsetted.qtable, table2D.PercentageAndCount)
    expect_equal(AverageEachRow(subsetted.qtable),
                 rowMeans(table2D.PercentageAndCount[, -10, 1], na.rm = TRUE))
    # Check opposite infinities
    table.opp.inf <- table.1D.MultipleStatistics
    table.opp.inf[1, 1] <- -Inf
    expected.out <- rowMeans(table.opp.inf)
    captured.warnings <- capture_warnings(expect_equal(AverageEachRow(table.opp.inf, warn = TRUE),
                                                       expected.out))
    diff.stats <- colnames(table.1D.MultipleStatistics)
    diff.stat.warning <- capture_warnings(throwWarningAboutDifferentStatistics(diff.stats, quoted.function))
    single.opp.inf.warn <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE), quoted.function))
    all.opp.inf.warn <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, TRUE), quoted.function))
    expect_setequal(captured.warnings, c(diff.stat.warning, single.opp.inf.warn))
    table.opp.inf[, 1] <- Inf * c(-1, 1, 1, 1)
    table.opp.inf[3:4, 2] <- -Inf
    expected.out <- rowSums(table.opp.inf)
    captured.warnings <- capture_warnings(expect_equal(AverageEachRow(table.opp.inf, warn = TRUE),
                                                       expected.out))
    expect_setequal(captured.warnings, c(diff.stat.warning, all.opp.inf.warn))
})

test_that("Higher dim Q tables", {
    load("numeric.grid.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.with.multiple.stats.qtable
    expect_equal(AverageEachRow(curr.table),
                 apply(curr.table[, -which(dimnames(curr.table)[[2L]] == "SUM"), ], c(1, 3), mean, na.rm = TRUE))
    load("numeric.grid.nominal.qtable.rda")
    curr.table <- numeric.grid.nominal.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.col.names <- dimnames(flattened.table)[[2L]]
    expect_equal(AverageEachRow(curr.table),
                 rowMeans(flattened.table[, flat.col.names != "SUM"],
                         na.rm = TRUE))
    load("numeric.grid.nominal.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.nominal.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    expect_equal(AverageEachRow(curr.table),
                 apply(flattened.table, c(1, 3), mean,
                       na.rm = TRUE))
    load("nominal.multi.nominal.qtable.rda")
    curr.table <- nominal.multi.nominal.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    expect_equal(AverageEachRow(curr.table),
                 rowMeans(flattened.table, na.rm = TRUE))
    load("nominal.multi.nominal.with.multiple.stats.qtable.rda")
    curr.table <- nominal.multi.nominal.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    expect_equal(AverageEachRow(curr.table),
                 apply(flattened.table, c(1, 3), mean, na.rm = TRUE))
    load("nominal.multi.nominal.multi.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    expect_equal(AverageEachRow(curr.table),
                 rowMeans(flattened.table, na.rm = TRUE))
    load("nominal.multi.nominal.multi.with.multiple.stats.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    expect_equal(AverageEachRow(curr.table),
                 apply(flattened.table, c(1, 3), mean, na.rm = TRUE))
    ## remove rows and columns
    ## Removal of rows and columns
    x <- table2D.Percentage
    remove.cols <- grep("Once", colnames(x), value = TRUE)
    remove.rows <- grep("Pepsi", rownames(x), value = TRUE)
    include.cols <- setdiff(colnames(x), remove.cols)
    include.rows <- setdiff(rownames(x), remove.rows)
    out <- AverageEachRow(x, remove.rows = remove.rows,
                             remove.columns = remove.cols)
    expect_equal(out, rowMeans(x[include.rows, include.cols]))
})

test_that("Q Tables: Check warning of different statistics thrown or suppressed", {
    # Matching statistics (No warnings)
    # warning already suppressed by default
    expected.out <- rowMeans(table.1D.MultipleStatistics)
    # Don't warn when default warn = FALSE
    expect_equal(AverageEachRow(table.1D.MultipleStatistics), expected.out)
    diff.stat.warning <- capture_warnings(throwWarningAboutDifferentStatistics(colnames(table.1D.MultipleStatistics), quoted.function))
    captured.warnings <- capture_warnings(expect_equal(AverageEachRow(table.1D.MultipleStatistics, warn = TRUE), expected.out))
    expect_setequal(captured.warnings, diff.stat.warning)
    # No warning even if warn = TRUE when only a single statistic
    expect_equivalent(AverageEachRow(table1D.Average),
                      table1D.Average)
    expected.warning <- capture_warning(throwWarningAboutCalculationWithSingleElement(table1D.Average, 2L, quoted.function))[["message"]]
    expect_warning(redundant.calc <- AverageEachRow(table1D.Average, warn = TRUE),
                   expected.warning)
    expect_equivalent(redundant.calc, table1D.Average)
    expected.out <- rowMeans(table2D.Percentage)
    expect_equal(AverageEachRow(table2D.Percentage, remove.columns = NULL),
                 expected.out)
})

test_that("A single R Output (e.g. a vanilla matrix or vector) selected", {
    ## tries to calls sum() and returns scalar
    matrix.1 <- matrix(1:24, nrow = 6)
    expect_equal(AverageEachRow(matrix.1), rowMeans(matrix.1))
    vector.1 <-1:24
    expect_equal(AverageEachRow(vector.1), vector.1)
    # Don't support higher arrays
    array.1 <- array(1:504, dim = 7:9)
    expected.error <- capture_error(throwErrorAboutHigherDimArray(3, quoted.function))[["message"]]
    expect_error(AverageEachRow(array.1), expected.error)
})

test_that("NULL or entirely missing inputs handled correctly", {
    expect_true(is.nan(AverageEachRow(NULL)))
    expect_true(is.nan(AverageEachRow(NA, remove.missing = TRUE)))
    expect_true(is.na(AverageEachRow(NA, remove.missing = FALSE)))
})

