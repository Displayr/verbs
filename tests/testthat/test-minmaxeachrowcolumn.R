context("MinEachRow, MaxEachRow, MaxEachColumn, MinEachColumn")

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

quoted.function <- sQuote("MinEachColumn")

extremeCalc <- function(x, fun) {
    if (allNA(x))
        return(NA)
    fun(x[!is.na(x)])
}

test_that("Variables", {
    text.error <- capture_error(throwErrorInvalidDataForNumericFunc("Text", quoted.function))[["message"]]
    expect_error(MinEachColumn(variable.Text), text.error)
    bad.df <- data.frame(`Coca-Cola` = variable.Binary,
                         `Living arrangements - other` = variable.Text,
                         check.names = FALSE)
    expect_error(MinEachColumn(bad.df), text.error)
    bad.df <- data.frame(variable.Date, variable.Binary)
    datetime.error <- capture_error(throwErrorInvalidDataForNumericFunc("Date/Time", quoted.function))[["message"]]
    expect_error(MinEachColumn(bad.df), datetime.error)
    bad.df <- data.frame(variable.Time, variable.Numeric)
    expect_error(MinEachColumn(bad.df), datetime.error)

    expected.min <- min(AsNumeric(variable.Nominal, FALSE), na.rm = TRUE)
    expect_equal(MinEachColumn(variable.Nominal), c(Age = expected.min))

    df <- data.frame(`Coca-Cola` = variable.Binary, Age = variable.Numeric,
                     Age = variable.Nominal, check.names = FALSE)
    expected.min <- vapply(df, function(x) min(AsNumeric(x, FALSE), na.rm = TRUE), 0)
    expect_equal(MinEachColumn(df),
                 expected.min)
    # Names deduced from the variable attributes in each data.frame element
    df <- data.frame(variable.Binary, variable.Nominal)
    expected.max <- vapply(df, function(x) max(AsNumeric(x, FALSE), na.rm = TRUE), 0)
    names(expected.max) <- vapply(df, attr, "", "label")
    expect_equal(MaxEachColumn(df), expected.max)

    # Warnings for factors
    ## No extra warning for variables that are converted using value attributes
    captured.warnings <- capture_condition(MinEachColumn(data.frame(variable.Binary, variable.Nominal),
                                                         warn = TRUE))
    missing.value.warning <- capture_condition(warnAboutMissingValuesIgnored())
    expect_equal(captured.warnings, missing.value.warning)
    ## AsNumeric warning should be appearing when factor converted that has no value attributes
    factor.values.warning <- capture_warnings(flipTransformations::AsNumeric(factor(1:2), binary = FALSE))
    expect_warning(MinEachColumn(data.frame(1:5, factor(1:5)), warn = TRUE),
                   factor.values.warning,
                   fixed = TRUE)
    # Missing values
    expect_equal(MaxEachColumn(data.frame("Coca-Cola" = variable.Binary,
                                       Age = variable.Numeric,
                                       Age = variable.Nominal,
                                       check.names = FALSE),
                            remove.missing = FALSE),
                 c("Coca-Cola" = NA_integer_, Age = NA_integer_, Age = NA_integer_))

    # Subset
    idx <- logical(length(variable.Numeric))
    idx[1:10] <- TRUE
    expect_equal(MinEachColumn(variable.Numeric, subset = idx),
                 c(Age = min(variable.Numeric[1:10], na.rm = TRUE)))
    nominal.to.numeric <- flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)
    subset.missing.out <- !is.na(variable.Numeric)
    expect_equal(MaxEachColumn(data.frame(Age = variable.Numeric,
                                       Age = variable.Nominal,
                                       check.names = FALSE),
                            subset = subset.missing.out),
                 c(Age = max(variable.Numeric, na.rm = TRUE),
                   Age = max(nominal.to.numeric, na.rm = TRUE)))
})

test_that("Variables - Each Row",
{
    nominal.var.expected <- as.vector(flipTransformations::AsNumeric(variable.Nominal, binary = FALSE))
    nominal.var.expected.wo.missing <- nominal.var.expected
    expect_equal(MinEachRow(variable.Nominal, remove.missing = FALSE), nominal.var.expected)
    expect_equal(MaxEachRow(variable.Nominal, remove.missing = TRUE), nominal.var.expected)

    nominal.to.numeric <- flipTransformations::AsNumeric(variable.Nominal,
                                                         binary = FALSE)
    vars.in.df <- data.frame(variable.Binary,
                             variable.Numeric,
                             nominal.to.numeric)
    expected.min <- setNames(suppressWarnings(apply(vars.in.df, 1, min, na.rm = TRUE)),
                                             1:nrow(vars.in.df))
    expected.max <- setNames(suppressWarnings(apply(vars.in.df, 1, max, na.rm = FALSE)),
                             1:nrow(vars.in.df))
    expect_equal(as.vector(MinEachRow(vars.in.df, remove.missing = TRUE)),
                 apply(vars.in.df, 1L, extremeCalc, fun = min))
    expect_equal(MaxEachRow(vars.in.df, remove.missing = FALSE), expected.max)
})

load("table1D.Average.rda")
load("table1D.Percentage.rda")
load("table.1D.MultipleStatistics.rda")

test_that("Table 1D", {
    expect_equal(MinEachColumn(table1D.Percentage), c(`table.Age` = min(table1D.Percentage)))
    expect_equal(MaxEachColumn(table.1D.MultipleStatistics),
                 apply(table.1D.MultipleStatistics[-4,], 2, max, na.rm = TRUE))

    expect_equivalent(MinEachRow(table1D.Percentage), table1D.Percentage)
    expect_equal(MaxEachRow(table.1D.MultipleStatistics), apply(table.1D.MultipleStatistics, 1, max))
    idx <- which(colnames(table.1D.MultipleStatistics) == "z-Statistic")
    expected.output <- apply(table.1D.MultipleStatistics[, -idx], 1, min)
    expect_equal(MinEachRow(table.1D.MultipleStatistics,
                            remove.columns = "z-Statistic"),
                 expected.output)

})

load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")
test_that("Table 2D EachColumn", {
    expect_equal(MaxEachColumn(table2D.Percentage),
                 apply(table2D.Percentage, 2, max, na.rm = TRUE))
    idx <- !rownames(table2D.PercentageNaN) %in% c("NET", "SUM", "Total")
    expect_equal(MinEachColumn(table2D.PercentageNaN),
                 apply(table2D.PercentageNaN[idx, ], 2, min, na.rm = TRUE))
    expect_equal(MaxEachColumn(table2D.Percentage),
                 MaxEachRow(t(table2D.Percentage)))
    expect_equal(MinEachColumn(t(table2D.Percentage)),
                 -MaxEachRow(-1*table2D.Percentage))
    col.min.2d.table.multi.stats <- cbind(`Row %` = apply(table2D.PercentageAndCount[, , 1], 2, min,
                                                          na.rm = TRUE),
                                          `Count` = apply(table2D.PercentageAndCount[, , 2], 2, min,
                                                          na.rm = TRUE))
    expect_equal(MinEachColumn(table2D.PercentageAndCount),
                 col.min.2d.table.multi.stats)
    transposed.table <- aperm(table2D.PercentageAndCount, c(2, 1, 3))
    attr(transposed.table, "questions") <- attr(table2D.PercentageAndCount, "questions")
    attr(transposed.table, "name") <- attr(table2D.PercentageAndCount, "name")
    expect_equal(MinEachColumn(transposed.table), MinEachRow(table2D.PercentageAndCount))
    # Extra category removed removed and warn about missing value removal
    missing.value.warning <- capture_condition(warnAboutMissingValuesIgnored())
    output.wo.missing <- quote(MaxEachColumn(table2D.PercentageNaN,
                                             remove.rows = c("NET", "None of these"),
                                             remove.missing = TRUE,
                                             warn = TRUE))
    observed.warn <- capture_condition(eval(output.wo.missing))
    expect_equal(observed.warn, missing.value.warning)
    output.wo.missing[["warn"]] <- FALSE
    output.wo.missing <- eval(output.wo.missing)
    expect_equal(output.wo.missing, apply(table2D.PercentageNaN[1:6, ], 2, max, na.rm = TRUE))
    # Missing values
    expect_false(anyNA(output.wo.missing))
    expect_true(anyNA(MinEachColumn(table2D.PercentageNaN, remove.missing = FALSE)))
})

test_that("Table 2D EachRow",
{
    expected.min <- apply(table2D.Percentage[, -10], 1, min)
    expect_equal(MinEachRow(table2D.Percentage), expected.min)
    expected.max <- apply(table2D.PercentageNaN[, -10], 1, max, na.rm = TRUE)
    expect_equal(MaxEachRow(table2D.PercentageNaN), expected.max)
    summary.stat.cols <- colnames(table2D.PercentageAndCount) == "NET"
    expected.min <- apply(table2D.PercentageAndCount[, !summary.stat.cols, ], c(1, 3), min)
    expect_equal(MinEachRow(table2D.PercentageAndCount),
                 expected.min)
    # Warning about missing values
    missing.value.warning <- capture_condition(warnAboutMissingValuesIgnored())
    observed.warn <- capture_condition(MaxEachRow(table2D.PercentageNaN, warn = TRUE))
    expect_equal(observed.warn, missing.value.warning)
    # Missing values
    expect_equal(out <- MinEachRow(table2D.PercentageNaN, remove.missing = FALSE),
                 apply(table2D.PercentageNaN, 1, min))
    expect_true(anyNA(out))
    expect_false(anyNA(MaxEachRow(table2D.PercentageNaN)))
    # Test subsetted 2D QTable with multiple statistics to a single statistic
    ## i.e. the case when the dims are a 3d array with (n, p, 1)
    subsetted.qtable <- table2D.PercentageAndCount[, , 1, drop = FALSE]
    subsetted.qtable <- CopyAttributes(subsetted.qtable, table2D.PercentageAndCount)
    expect_equal(MinEachRow(subsetted.qtable),
                 apply(table2D.PercentageAndCount[, -10, 1], 1, min, na.rm = TRUE))

})

test_that("A single R Output (e.g. a vanilla matrix or vector) selected", {
    ## tries to calls sum() and returns scalar
    matrix.1 <- matrix(1:24, nrow = 6)
    expect_equal(MinEachRow(matrix.1), apply(matrix.1, 1, min, na.rm = TRUE))
    expect_equal(MaxEachColumn(matrix.1), apply(matrix.1, 2, max, na.rm = TRUE))
    vector.1 <-1:24
    expect_equal(MaxEachRow(vector.1), vector.1)
    expect_equal(MinEachColumn(vector.1), min(vector.1, na.rm = TRUE))
    # Don't support higher arrays
    array.1 <- array(1:504, dim = 7:9)
    expected.error <- capture_error(throwErrorAboutHigherDimArray(3L, sQuote("MinEachRow")))[["message"]]
    expect_error(MinEachRow(array.1), expected.error)
    expected.error <- capture_error(throwErrorAboutHigherDimArray(3L, sQuote("MaxEachColumn")))[["message"]]
    expect_error(MaxEachColumn(array.1), expected.error)
    # Check edge cases
    x <- 1:4
    df <- data.frame(x)
    expect_equivalent(MinEachRow(df), df)
    x.1d <- array(x, dim = c(1L, 4L))
    observed <- capture_warnings(MaxEachColumn(x.1d, warn = TRUE))
    expected <- capture_warnings(throwWarningAboutCalculationWithSingleElement(x.1d,
                                                                              dimension = 1L,
                                                                              sQuote("MaxEachColumn")))
    expect_equal(observed, expected)
    expected <- capture_warnings(throwWarningAboutCalculationWithSingleElement(x.1d,
                                                                              dimension = 2L,
                                                                              sQuote("MaxEachRow")))
    observed <- capture_warnings(MaxEachRow(t(x.1d), warn = TRUE))
    expect_equal(observed, expected)
})

load("numeric.grid.with.multiple.stats.qtable.rda")
load("numeric.grid.nominal.qtable.rda")
load("numeric.grid.nominal.with.multiple.stats.qtable.rda")
load("nominal.multi.nominal.qtable.rda")
load("nominal.multi.nominal.multi.qtable.rda")
load("nominal.multi.nominal.multi.with.multiple.stats.qtable.rda")
load("nominal.multi.nominal.with.multiple.stats.qtable.rda")
test_that("Higher dim Q tables Each Column", {

    x <- numeric.grid.with.multiple.stats.qtable
    expect_equal(MaxEachColumn(x, remove.columns = "SUM"),
                 apply(flattenQTableKeepingMultipleStatistics(x)[rownames(x) != "SUM",
                                                                 colnames(x) != "SUM", ],
                         c(2, 3), max, na.rm = TRUE))
    expect_equal(MinEachRow(x, remove.rows = "SUM"),
                 apply(flattenQTableKeepingMultipleStatistics(x)[rownames(x) != "SUM",
                                                                 colnames(x) != "SUM", ],
                         c(1, 3), min, na.rm = TRUE))

    x <- numeric.grid.nominal.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(x)
    flat.row.names <- row.names(as.matrix(flattened.table))
    expected.min <-  apply(flattened.table[flat.row.names != "SUM", ],
                           2, extremeCalc, fun = min)
    expect_equal(MinEachColumn(x), expected.min)
    expected.max <- apply(flattened.table, 1L,
                          extremeCalc, fun = max)
    expect_equal(MaxEachRow(x), expected.max)


    x <- numeric.grid.nominal.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(x)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expected.max <- apply(flattened.table[flat.row.names != "SUM", ,],
                          c(2, 3), extremeCalc, fun = max)
    expect_equal(MaxEachColumn(x, remove.row = "SUM"), expected.max)
    expect_equal(MinEachRow(x, remove.rows = "SUM"),
                 apply(flattened.table[flat.row.names != "SUM", ,],
                       c(1, 3), extremeCalc, fun = min))

    x <- nominal.multi.nominal.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(x)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expect_equal(MinEachColumn(x),
                 apply(flattened.table[flat.row.names != "SUM", ],
                       2, min, na.rm = TRUE))
    expect_equal(MaxEachRow(x),
                 apply(flattened.table[flat.row.names != "SUM", ],
                       1, max, na.rm = TRUE))

    x <- nominal.multi.nominal.multi.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(x)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expect_equal(MaxEachColumn(x),
                 apply(flattened.table[flat.row.names != "SUM",],
                         2, max, na.rm = TRUE))
    expect_equal(MinEachRow(x),
                 apply(flattened.table[flat.row.names != "SUM",],
                         1, min, na.rm = TRUE))

    x <- nominal.multi.nominal.multi.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(x)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expect_equal(MinEachColumn(x),
                 apply(flattened.table[flat.row.names != "SUM", ,],
                       c(2, 3), min,  na.rm = TRUE))
    expect_equal(MaxEachRow(x),
                 apply(flattened.table,
                       c(1, 3), max,  na.rm = TRUE))

    x <- nominal.multi.nominal.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(x)
    expect_equal(MaxEachColumn(x),
                 apply(flattened.table, c(2, 3), max, na.rm = TRUE))
    expect_equal(MinEachRow(x),
                 apply(flattened.table, c(1, 3), min, na.rm = TRUE))
})

test_that("Aliases working", {
    expect_equal(MinEachColumn, MinColumns)
    expect_equal(MaxEachRow, MaxRows)
    expect_equal(MaxColumns(table2D.Percentage),
                 MaxEachColumn(table2D.Percentage))
    expect_equal(MaxRows(table2D.Percentage),
                 MaxEachRow(table2D.Percentage))

})

test_that("Warnings muffled", {
    # Not show the missing value warning
    input.array <- array(1:12, dim = 3:4, dimnames = list(LETTERS[1:3], letters[1:4]))
    is.na(input.array) <- 1:3
    # Input array
    #    a b c d
    # A NA 4 7 10
    # B NA 5 8 11
    # C NA 6 9 12
    expected.vals <- list(MinEachRow    = c(A = 4,  B = 5,  C = 6),
                          MinEachColumn = c(a = NA, b = 4,  c = 7, d = 10),
                          MaxEachRow    = c(A = 10, B = 11, C = 12),
                          MaxEachColumn = c(a = NA, b = 6,  c = 9, d = 12))
    expect_equal(MinEachRow(input.array, warn = "Foo"),    expected.vals[["MinEachRow"]])
    expect_equal(MaxEachRow(input.array, warn = "Foo"),    expected.vals[["MaxEachRow"]])
    expect_equal(MinEachColumn(input.array, warn = "Foo"), expected.vals[["MinEachColumn"]])
    expect_equal(MaxEachColumn(input.array, warn = "Foo"), expected.vals[["MaxEachColumn"]])
})
