context("SumEachColumn")

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

quoted.function <- sQuote("SumEachColumn")

colSumsNaAdjusted <- function(x, remove.missing)
{
    if (!remove.missing)
        return(colSums(x))
    y <- colSums(x, na.rm = TRUE)
    if (any(all.missing <- apply(x, 2:length(DIM(x)), allNA)))
        y[all.missing] <- NA
    y
}

test_that("Variables", {
    text.error <- capture_error(throwErrorInvalidDataForNumericFunc("Text", quoted.function))[["message"]]
    expect_error(SumEachColumn(variable.Text), text.error)
    bad.df <- data.frame(`Coca-Cola` = variable.Binary,
                         `Living arrangements - other` = variable.Text,
                         check.names = FALSE)
    expect_error(SumEachColumn(bad.df), text.error)
    bad.df <- data.frame(variable.Date, variable.Binary)
    datetime.error <- capture_error(throwErrorInvalidDataForNumericFunc("Date/Time", quoted.function))[["message"]]
    expect_error(SumEachColumn(bad.df), datetime.error)
    bad.df <- data.frame(variable.Time, variable.Numeric)
    expect_error(SumEachColumn(bad.df), datetime.error)
    expect_equal(SumEachColumn(variable.Nominal), c(Age = 12606))
    df <- data.frame(`Coca-Cola` = variable.Binary, Age = variable.Numeric,
                     Age = variable.Nominal, check.names = FALSE)
    expect_equal(SumEachColumn(df),
                 c("Coca-Cola" = 155, Age = 12606, Age = 12606))
    # Names deduced from the variable attributes in each data.frame element
    expect_equal(SumEachColumn(data.frame(variable.Binary, variable.Nominal)),
                 c("Coca-Cola" = 155, "Age" = 12606))
    # Warnings for factors
    ## No extra warning for variables that are converted using value attributes
    captured.warnings <- capture_condition(SumEachColumn(data.frame(variable.Binary, variable.Nominal), warn = TRUE))
    missing.value.warning <- capture_condition(warnAboutMissingValuesIgnored())
    expect_equal(captured.warnings, missing.value.warning)
    ## AsNumeric warning should be appearing when factor converted that has no value attributes
    factor.values.warning <- capture_warnings(flipTransformations::AsNumeric(factor(1:2), binary = FALSE))
    expect_warning(SumEachColumn(data.frame(1:5, factor(1:5)), warn = TRUE),
                   factor.values.warning,
                   fixed = TRUE)
    # Missing values
    expect_equal(SumEachColumn(data.frame("Coca-Cola" = variable.Binary,
                                       Age = variable.Numeric,
                                       Age = variable.Nominal,
                                       check.names = FALSE),
                            remove.missing = FALSE),
                 c("Coca-Cola" = NA_integer_, Age = NA_integer_, Age = NA_integer_))
})

test_that("Variables with weights, filters (subset), and a combination of the two", {
    # Variables and multiple variables
    subset.missing.out <- !is.na(variable.Numeric)
    nominal.to.numeric <- flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)
    expect_equal(SumEachColumn(variable.Numeric, subset = subset.missing.out),
                 c(Age = sum(variable.Numeric, na.rm = TRUE)))
    expect_equal(SumEachColumn(data.frame(Age = variable.Numeric,
                                       Age = variable.Nominal,
                                       check.names = FALSE),
                            subset = subset.missing.out),
                 c(Age = sum(variable.Numeric, na.rm = TRUE),
                   Age = sum(nominal.to.numeric, na.rm = TRUE)))
    expected.error <- capture_error(throwErrorSubsetOrWeightsWrongSize("subset",
                                                                       length(subset.missing.out),
                                                                       10L))[["message"]]
    expect_error(SumEachColumn(variable.Numeric[1:10], subset = subset.missing.out),
                 expected.error)
    weights <- runif(length(variable.Numeric))
    expect_equal(SumEachColumn(variable.Numeric, weights = weights),
                 c(Age = sum(variable.Numeric * weights, na.rm = TRUE)))
    expect_equal(SumEachColumn(data.frame(Age = variable.Numeric,
                                       `Coca-Cola` = variable.Binary,
                                       check.names = FALSE),
                            weights = weights),
                 c(Age = sum(variable.Numeric * weights, na.rm = TRUE),
                   `Coca-Cola` = sum(variable.Binary * weights, na.rm = TRUE)))
    expect_equal(SumEachColumn(data.frame(Age = variable.Numeric,
                                       Age = variable.Nominal,
                                       check.names = FALSE),
                            weights = weights,
                            subset = subset.missing.out),
                 c(Age = sum(variable.Numeric * weights, na.rm = TRUE),
                   Age = sum(nominal.to.numeric * weights, na.rm = TRUE)))
    expected.error <- capture_error(throwErrorSubsetOrWeightsWrongSize("weights",
                                                                       10L,
                                                                       length(variable.Numeric)))[["message"]]
    expect_error(SumEachColumn(variable.Numeric, weights = weights[1:10]), expected.error)
    # Variable sets and data.frames, names deduced from variables inside the df
    expect_equal(SumEachColumn(data.frame(variable.Binary, variable.Nominal),
                            subset = subset.missing.out, remove.missing = FALSE),
                 c("Coca-Cola" = NA, "Age" = 12606))
    subset.binary <- !is.na(variable.Binary)
    expected.weighted.bin <- sum(variable.Binary * weights, na.rm = TRUE)
    expect_equal(SumEachColumn(data.frame(variable.Binary, variable.Nominal),
                            subset = subset.binary, weights = weights,
                            remove.missing = FALSE),
                 c("Coca-Cola" = expected.weighted.bin, "Age" = NA))
    df <- data.frame(variable.Binary,
                     variable.Nominal = flipTransformations::AsNumeric(variable.Nominal, binary = FALSE))
    weighted.df <- df * weights
    expected.sum <- setNames(colSums(weighted.df, na.rm = TRUE), c("Coca-Cola", "Age"))
    expect_equal(SumEachColumn(data.frame(variable.Binary, variable.Nominal),
                            weights = weights,
                            remove.missing = TRUE),
                 expected.sum)

})

load("table1D.Average.rda")
load("table1D.Percentage.rda")
load("table.1D.MultipleStatistics.rda")

test_that("Table 1D", {
    expect_equal(SumEachColumn(table1D.Percentage), c(`table.Age` = 100))
    expect_equal(SumEachColumn(table.1D.MultipleStatistics),
                 colSums(table.1D.MultipleStatistics[-4,], na.rm = TRUE))
    single.opp.inf.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE), quoted.function))
    expect_warning(SumEachColumn(table.1D.MultipleStatistics, warn = TRUE),
                   single.opp.inf.warning)
})

load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")
test_that("Table 2D", {
    expect_equal(SumEachColumn(table2D.Percentage),
                 colSums(table2D.Percentage, na.rm = TRUE))
    expect_equal(SumEachColumn(table2D.PercentageNaN),
                 colSums(table2D.PercentageNaN[-8, ], na.rm = TRUE))
    expect_equal(SumEachColumn(table2D.Percentage),
                 SumEachRow(t(table2D.Percentage)))
    expected.table <- cbind(`Row %` = colSums(table2D.PercentageAndCount[, , 1]),
                            `Count` = colSums(table2D.PercentageAndCount[, , 2]))
    expect_equal(SumEachColumn(table2D.PercentageAndCount), expected.table)
    transposed.table <- aperm(table2D.PercentageAndCount, c(2, 1, 3))
    attr(transposed.table, "questions") <- attr(table2D.PercentageAndCount, "questions")
    attr(transposed.table, "name") <- attr(table2D.PercentageAndCount, "name")
    expect_equal(SumEachColumn(transposed.table), SumEachRow(table2D.PercentageAndCount))
    # Extra category removed removed and warn about missing value removal
    missing.value.warning <- capture_condition(warnAboutMissingValuesIgnored())
    output.wo.missing <- quote(SumEachColumn(table2D.PercentageNaN,
                                          remove.rows = c("NET", "None of these"),
                                          remove.missing = TRUE,
                                          warn = TRUE))
    observed.warn <- capture_condition(eval(output.wo.missing))
    expect_equal(observed.warn, missing.value.warning)
    output.wo.missing[["warn"]] <- FALSE
    output.wo.missing <- eval(output.wo.missing)
    expect_equal(output.wo.missing, colSums(table2D.PercentageNaN[1:6, ], na.rm = TRUE))
    # Missing values
    expect_true(anyNA(SumEachColumn(table2D.PercentageNaN, remove.missing = FALSE)))
    expect_false(anyNA(output.wo.missing))
})

test_that("Higher dim Q tables", {
    load("numeric.grid.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.with.multiple.stats.qtable
    expected <- colSumsNaAdjusted(flattenQTableKeepingMultipleStatistics(curr.table)[rownames(curr.table) != "SUM", , ],
                                  remove.missing = TRUE)
    expect_equal(SumEachColumn(curr.table), expected)
    load("numeric.grid.nominal.qtable.rda")
    curr.table <- numeric.grid.nominal.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.row.names <- row.names(as.matrix(flattened.table))
    expect_equal(SumEachColumn(curr.table),
                 colSumsNaAdjusted(flattenQTableKeepingMultipleStatistics(curr.table)[flat.row.names != "SUM", ],
                                   remove.missing = TRUE))
    load("numeric.grid.nominal.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.nominal.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expect_equal(SumEachColumn(curr.table),
                 colSumsNaAdjusted(flattenQTableKeepingMultipleStatistics(curr.table)[flat.row.names != "SUM", , ],
                                   remove.missing = TRUE))
    load("nominal.multi.nominal.qtable.rda")
    curr.table <- nominal.multi.nominal.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expect_equal(SumEachColumn(curr.table),
                 colSumsNaAdjusted(flattenQTableKeepingMultipleStatistics(curr.table)[flat.row.names != "SUM", ],
                                   remove.missing = TRUE))
    load("nominal.multi.nominal.multi.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expect_equal(SumEachColumn(curr.table),
                 colSumsNaAdjusted(flattenQTableKeepingMultipleStatistics(curr.table)[flat.row.names != "SUM", ],
                                   remove.missing = TRUE))
    load("nominal.multi.nominal.multi.with.multiple.stats.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expect_equal(SumEachColumn(curr.table),
                 colSumsNaAdjusted(flattenQTableKeepingMultipleStatistics(curr.table)[flat.row.names != "SUM", , ],
                                   remove.missing = TRUE))
})


test_that("Warnings", {
    missing.value.warning <- capture_condition(warnAboutMissingValuesIgnored())
    sum.cols.output <- quote(SumEachColumn(table2D.PercentageNaN,
                                        remove.rows = c("None of these", "NET"),
                                        warn = TRUE))
    observed.warn <- capture_condition(eval(sum.cols.output))
    expect_equal(observed.warn, missing.value.warning)
    sum.cols.output[["warn"]] <- FALSE
    expect_equal(eval(sum.cols.output), colSums(table2D.PercentageNaN[- (7:8), ], na.rm = TRUE))
    sum.col <- matrix(rowSums(table.1D.MultipleStatistics), ncol = 1, dimnames = list(rep("", 4), "NET"))
    table.1D.MultiStat.with.SUM.col <- cbind(table.1D.MultipleStatistics, sum.col)
    table.1D.MultiStat.with.SUM.col[1, 1] <- -Inf
    table.1D.MultiStat.with.SUM.col <- CopyAttributes(table.1D.MultiStat.with.SUM.col, table.1D.MultipleStatistics)
    single.opp.inf.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE), quoted.function))
    expect_warning(expect_equal(SumEachColumn(table.1D.MultiStat.with.SUM.col,
                                           warn = TRUE),
                                colSums(table.1D.MultiStat.with.SUM.col[-4, ], na.rm = TRUE)),
                   single.opp.inf.warning)
    ## Same situation with data.frame
    df.input <- as.data.frame(table.1D.MultiStat.with.SUM.col)
    expect_warning(expect_equal(SumEachColumn(df.input, warn = TRUE),
                                colSums(df.input[-4, ], na.rm = TRUE)),
                   single.opp.inf.warning)
    table.1D.MultiStat.with.SUM.col[1, ] <- Inf
    table.1D.MultiStat.with.SUM.col[2, ] <- -Inf
    all.opp.inf.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, TRUE), quoted.function))
    expect_warning(expect_true(all(is.nan(SumEachColumn(table.1D.MultiStat.with.SUM.col,
                                                     warn = TRUE)))),
                   all.opp.inf.warning)
    df.input <- as.data.frame(table.1D.MultiStat.with.SUM.col)
    expect_warning(expect_true(all(is.nan(SumEachColumn(df.input, warn = TRUE)))),
                   all.opp.inf.warning)
    # Throw warning about filter and/or weights being ignored for Q Tables
    subset.warning <- capture_warnings(warnSubsetOrWeightsNotApplicable("a filter", 1, quoted.function))
    expect_warning(expect_equal(SumEachColumn(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)), warn = TRUE),
                                c(`table.Frequency.of.drinking` = sum(table1D.Average[-4]))),
                   subset.warning)
    weights.warning <- capture_warnings(warnSubsetOrWeightsNotApplicable("weights", 1, quoted.function))
    expect_warning(expect_equal(SumEachColumn(table1D.Average, weights = runif(10), warn = TRUE),
                                c(`table.Frequency.of.drinking` = sum(table1D.Average[-4]))),
                   weights.warning)
    both.warning <- capture_warnings(warnSubsetOrWeightsNotApplicable("a filter or weights", 1, quoted.function))
    expect_warning(expect_equal(SumEachColumn(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)),
                                           weights = runif(10), warn = TRUE),
                                c(`table.Frequency.of.drinking` = sum(table1D.Average[-4]))),
                   both.warning)
    input.matrix <- matrix(c(Inf, -Inf, 1, 2), nrow = 2, dimnames = list(NULL, letters[1:2]))
    input.vect <- c(Inf, -Inf)
    expect_warning(SumEachColumn(input.matrix, warn = TRUE),
                   single.opp.inf.warning)
    expect_warning(SumEachColumn(input.vect, warn = TRUE),
                   all.opp.inf.warning)
    fake.qtable <- array(1:24, dim = 4:2, dimnames = list(1:4, letters[1:3], LETTERS[1:2]))
    attr(fake.qtable, "questions") <- "Foo"
    expect_warning(checkOppositeInfinitiesByColumn(sumCols(fake.qtable, remove.missing = FALSE),
                                                   fake.qtable, "foo"), NA)
    fake.qtable[, 1, 1] <- c(Inf, 1:2, -Inf)
    single.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE), "foo"))
    expect_warning(checkOppositeInfinitiesByColumn(sumCols(fake.qtable, remove.missing = FALSE),
                                                   fake.qtable, "foo"),
                   single.warning)
    fake.qtable <- array(c(Inf, -Inf), dim = c(4, 2, 2),
                         dimnames = list(NULL, letters[1:2], LETTERS[1:2]))
    attr(fake.qtable, "questions") <- "Foo"
    all.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, TRUE), "foo"))
    expect_warning(checkOppositeInfinitiesByColumn(sumCols(fake.qtable, remove.missing = FALSE),
                                                   fake.qtable, "foo"),
                   all.warning)
    x <- matrix(1:4, nrow = 1)
    expected.warning <- capture_warnings(throwWarningAboutCalculationWithSingleElement(x,
                                                                                       dimension = 1L,
                                                                                       sQuote("SumEachColumn")))
    expect_warning(SumEachColumn(x, warn = TRUE), expected.warning)
})


test_that("Warnings muffled", {
    # Not show the missing value warning
    input.array <- array(1:12, dim = 3:4, dimnames = list(LETTERS[1:3], NULL))
    is.na(input.array) <- seq(from = 1, to = 12, by = 3)
    expect_equal(SumEachColumn(input.array, warn = "Foo"), colSums(input.array[-1L, ]))
})
