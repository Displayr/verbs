context("SumColumns")

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

quoted.function <- sQuote("SumColumns")

test_that("Variables", {
    text.error <- capture_error(throwErrorInvalidDataForNumericFunc("Text", quoted.function))[["message"]]
    expect_error(SumColumns(variable.Text), text.error)
    bad.df <- data.frame(`Coca-Cola` = variable.Binary,
                         `Living arrangements - other` = variable.Text,
                         check.names = FALSE)
    expect_error(SumColumns(bad.df), text.error)
    bad.df <- data.frame(variable.Date, variable.Binary)
    datetime.error <- capture_error(throwErrorInvalidDataForNumericFunc("Date/Time", quoted.function))[["message"]]
    expect_error(SumColumns(bad.df), datetime.error)
    bad.df <- data.frame(variable.Time, variable.Numeric)
    expect_error(SumColumns(bad.df), datetime.error)
    expect_equal(SumColumns(variable.Nominal), c(Age = 12606))
    df <- data.frame(`Coca-Cola` = variable.Binary, Age = variable.Numeric,
                     Age = variable.Nominal, check.names = FALSE)
    expect_equal(SumColumns(df),
                 c("Coca-Cola" = 155, Age = 12606, Age = 12606))
    # Names deduced from the variable attributes in each data.frame element
    expect_equal(SumColumns(data.frame(variable.Binary, variable.Nominal)),
                 c("Coca-Cola" = 155, "Age" = 12606))
    # Warnings for factors
    ## No extra warning for variables that are converted using value attributes
    captured.warnings <- capture_warnings(SumColumns(data.frame(variable.Binary, variable.Nominal),
                                                     warn = TRUE))
    missing.value.warning <- capture_warnings(throwWarningAboutMissingValuesIgnored())
    expect_setequal(captured.warnings, missing.value.warning)
    ## AsNumeric warning should be appearing when factor converted that has no value attributes
    factor.values.warning <- capture_warnings(flipTransformations::AsNumeric(factor(1:2), binary = FALSE))
    expect_warning(SumColumns(data.frame(1:5, factor(1:5)), warn = TRUE),
                   factor.values.warning,
                   fixed = TRUE)
    # Missing values
    expect_equal(SumColumns(data.frame("Coca-Cola" = variable.Binary,
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
    expect_equal(SumColumns(variable.Numeric, subset = subset.missing.out),
                 c(Age = sum(variable.Numeric, na.rm = TRUE)))
    expect_equal(SumColumns(data.frame(Age = variable.Numeric,
                                       Age = variable.Nominal,
                                       check.names = FALSE),
                            subset = subset.missing.out),
                 c(Age = sum(variable.Numeric, na.rm = TRUE),
                   Age = sum(nominal.to.numeric, na.rm = TRUE)))
    expected.error <- capture_error(throwErrorSubsetOrWeightsWrongSize("subset",
                                                                       length(subset.missing.out),
                                                                       10L))[["message"]]
    expect_error(SumColumns(variable.Numeric[1:10], subset = subset.missing.out),
                 expected.error)
    weights <- runif(length(variable.Numeric))
    expect_equal(SumColumns(variable.Numeric, weights = weights),
                 c(Age = sum(variable.Numeric * weights, na.rm = TRUE)))
    expect_equal(SumColumns(data.frame(Age = variable.Numeric,
                                       `Coca-Cola` = variable.Binary,
                                       check.names = FALSE),
                            weights = weights),
                 c(Age = sum(variable.Numeric * weights, na.rm = TRUE),
                   `Coca-Cola` = sum(variable.Binary * weights, na.rm = TRUE)))
    expect_equal(SumColumns(data.frame(Age = variable.Numeric,
                                       Age = variable.Nominal,
                                       check.names = FALSE),
                            weights = weights,
                            subset = subset.missing.out),
                 c(Age = sum(variable.Numeric * weights, na.rm = TRUE),
                   Age = sum(nominal.to.numeric * weights, na.rm = TRUE)))
    expected.error <- capture_error(throwErrorSubsetOrWeightsWrongSize("weights",
                                                                       10L,
                                                                       length(variable.Numeric)))[["message"]]
    expect_error(SumColumns(variable.Numeric, weights = weights[1:10]), expected.error)
    # Variable sets and data.frames, names deduced from variables inside the df
    expect_equal(SumColumns(data.frame(variable.Binary, variable.Nominal),
                            subset = subset.missing.out, remove.missing = FALSE),
                 c("Coca-Cola" = NA, "Age" = 12606))
    subset.binary <- !is.na(variable.Binary)
    expected.weighted.bin <- sum(variable.Binary * weights, na.rm = TRUE)
    expect_equal(SumColumns(data.frame(variable.Binary, variable.Nominal),
                            subset = subset.binary, weights = weights,
                            remove.missing = FALSE),
                 c("Coca-Cola" = expected.weighted.bin, "Age" = NA))
    df <- data.frame(variable.Binary,
                     variable.Nominal = flipTransformations::AsNumeric(variable.Nominal, binary = FALSE))
    weighted.df <- df * weights
    expected.sum <- setNames(colSums(weighted.df, na.rm = TRUE), c("Coca-Cola", "Age"))
    expect_equal(SumColumns(data.frame(variable.Binary, variable.Nominal),
                            weights = weights,
                            remove.missing = TRUE),
                 expected.sum)

})

load("table1D.Average.rda")
load("table1D.Percentage.rda")
load("table.1D.MultipleStatistics.rda")

test_that("Table 1D", {
    expect_equal(SumColumns(table1D.Percentage), c(`table.Age` = 100))
    expect_equal(SumColumns(table.1D.MultipleStatistics),
                 colSums(table.1D.MultipleStatistics[-4,], na.rm = TRUE))
    single.opp.inf.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE), quoted.function))
    expect_warning(SumColumns(table.1D.MultipleStatistics, warn = TRUE),
                   single.opp.inf.warning)
})

load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")
test_that("Table 2D", {
    expect_equal(SumColumns(table2D.Percentage),
                 colSums(table2D.Percentage, na.rm = TRUE))
    expect_equal(SumColumns(table2D.PercentageNaN),
                 colSums(table2D.PercentageNaN[-8, ], na.rm = TRUE))
    expect_equal(SumColumns(table2D.Percentage),
                 SumRows(t(table2D.Percentage)))
    col.summed.2d.table.multi.stats <- cbind(`Row %` = colSums(table2D.PercentageAndCount[, , 1]),
                                             `Count` = colSums(table2D.PercentageAndCount[, , 2]))
    expect_equal(SumColumns(table2D.PercentageAndCount),
                 col.summed.2d.table.multi.stats)
    transposed.table <- aperm(table2D.PercentageAndCount, c(2, 1, 3))
    attr(transposed.table, "questions") <- attr(table2D.PercentageAndCount, "questions")
    attr(transposed.table, "name") <- attr(table2D.PercentageAndCount, "name")
    expect_equal(SumColumns(transposed.table), SumRows(table2D.PercentageAndCount))
    # Extra category removed removed and warn about missing value removal
    missing.value.warning <- capture_warnings(throwWarningAboutMissingValuesIgnored())
    expect_equal(expect_warning(output.wo.missing <- SumColumns(table2D.PercentageNaN,
                                                                remove.rows = c("NET", "None of these"),
                                                                remove.missing = TRUE,
                                                                warn = TRUE),
                                missing.value.warning),
                 colSums(table2D.PercentageNaN[1:6, ], na.rm = TRUE))
    # Missing values
    expect_true(anyNA(SumColumns(table2D.PercentageNaN, remove.missing = FALSE)))
    expect_false(anyNA(output.wo.missing))
})

test_that("Higher dim Q tables", {
    load("numeric.grid.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.with.multiple.stats.qtable
    expect_equal(SumColumns(curr.table),
                 colSums(flattenQTableKeepingMultipleStatistics(curr.table)[rownames(curr.table) != "SUM", ,],
                         na.rm = TRUE))
    load("numeric.grid.nominal.qtable.rda")
    curr.table <- numeric.grid.nominal.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.row.names <- row.names(as.matrix(flattened.table))
    expect_equal(SumColumns(curr.table),
                 colSums(flattenQTableKeepingMultipleStatistics(curr.table)[flat.row.names != "SUM", ],
                         na.rm = TRUE))
    load("numeric.grid.nominal.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.nominal.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expect_equal(SumColumns(curr.table),
                 colSums(flattenQTableKeepingMultipleStatistics(curr.table)[flat.row.names != "SUM", ,],
                         na.rm = TRUE))
    load("nominal.multi.nominal.qtable.rda")
    curr.table <- nominal.multi.nominal.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expect_equal(SumColumns(curr.table),
                 colSums(flattenQTableKeepingMultipleStatistics(curr.table)[flat.row.names != "SUM", ],
                         na.rm = TRUE))
    load("nominal.multi.nominal.multi.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expect_equal(SumColumns(curr.table),
                 colSums(flattenQTableKeepingMultipleStatistics(curr.table)[flat.row.names != "SUM",],
                         na.rm = TRUE))
    load("nominal.multi.nominal.multi.with.multiple.stats.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expect_equal(SumColumns(curr.table),
                 colSums(flattenQTableKeepingMultipleStatistics(curr.table)[flat.row.names != "SUM", ,],
                         na.rm = TRUE))
})


test_that("Warnings", {
    missing.value.warning <- capture_warnings(throwWarningAboutMissingValuesIgnored())
    expect_warning(expect_equal(SumColumns(table2D.PercentageNaN,
                                           remove.rows = c("None of these", "NET"),
                                           warn = TRUE),
                                colSums(table2D.PercentageNaN[-(7:8), ], na.rm = TRUE)),
                   missing.value.warning)
    SUM.col <- matrix(rowSums(table.1D.MultipleStatistics), ncol = 1, dimnames = list(rep("", 4), "NET"))
    table.1D.MultipleStatistics.with.SUM.col <- cbind(table.1D.MultipleStatistics, SUM.col)
    table.1D.MultipleStatistics.with.SUM.col[1, 1] <- -Inf
    table.1D.MultipleStatistics.with.SUM.col <- CopyAttributes(table.1D.MultipleStatistics.with.SUM.col, table.1D.MultipleStatistics)
    single.opp.inf.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE), quoted.function))
    expect_warning(expect_equal(SumColumns(table.1D.MultipleStatistics.with.SUM.col,
                                           warn = TRUE),
                                colSums(table.1D.MultipleStatistics.with.SUM.col[-4, ], na.rm = TRUE)),
                   single.opp.inf.warning)
    ## Same situation with data.frame
    df.input <- as.data.frame(table.1D.MultipleStatistics.with.SUM.col)
    expect_warning(expect_equal(SumColumns(df.input, warn = TRUE),
                                colSums(df.input[-4, ], na.rm = TRUE)),
                   single.opp.inf.warning)
    table.1D.MultipleStatistics.with.SUM.col[1, ] <- Inf
    table.1D.MultipleStatistics.with.SUM.col[2, ] <- -Inf
    all.opp.inf.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, TRUE), quoted.function))
    expect_warning(expect_true(all(is.nan(SumColumns(table.1D.MultipleStatistics.with.SUM.col,
                                                     warn = TRUE)))),
                   all.opp.inf.warning)
    df.input <- as.data.frame(table.1D.MultipleStatistics.with.SUM.col)
    expect_warning(expect_true(all(is.nan(SumColumns(df.input, warn = TRUE)))),
                   all.opp.inf.warning)
    # Throw warning about filter and/or weights being ignored for Q Tables
    subset.on.table.warning <- capture_warnings(throwWarningThatSubsetOrWeightsNotApplicableToTable("a filter",
                                                                                                    1,
                                                                                                    quoted.function))
    expect_warning(expect_equal(SumColumns(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)), warn = TRUE),
                                c(`table.Frequency.of.drinking` = sum(table1D.Average[-4]))),
                   subset.on.table.warning)
    weights.on.table.warning <- capture_warnings(throwWarningThatSubsetOrWeightsNotApplicableToTable("weights",
                                                                                                     1,
                                                                                                     quoted.function))
    expect_warning(expect_equal(SumColumns(table1D.Average, weights = runif(10), warn = TRUE),
                                c(`table.Frequency.of.drinking` = sum(table1D.Average[-4]))),
                   weights.on.table.warning)
    both.on.table.warning <- capture_warnings(throwWarningThatSubsetOrWeightsNotApplicableToTable("a filter or weights",
                                                                                                  1,
                                                                                                  quoted.function))
    expect_warning(expect_equal(SumColumns(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)),
                                           weights = runif(10), warn = TRUE),
                                c(`table.Frequency.of.drinking` = sum(table1D.Average[-4]))),
                   both.on.table.warning)
    input.matrix <- matrix(c(Inf, -Inf, 1, 2), nrow = 2, dimnames = list(NULL, letters[1:2]))
    input.vect <- c(Inf, -Inf)
    expect_warning(SumColumns(input.matrix, warn = TRUE),
                   single.opp.inf.warning)
    expect_warning(SumColumns(input.vect, warn = TRUE),
                   all.opp.inf.warning)
    fake.qtable <- array(1:24, dim = 4:2, dimnames = list(1:4, letters[1:3], LETTERS[1:2]))
    attr(fake.qtable, "questions") <- "Foo"
    expect_warning(checkOppositeInifinitiesByColumn(sumCols(fake.qtable, remove.missing = FALSE),
                                                    fake.qtable, "foo"), NA)
    fake.qtable[, 1, 1] <- c(Inf, 1:2, -Inf)
    single.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE), "foo"))
    expect_warning(checkOppositeInifinitiesByColumn(sumCols(fake.qtable, remove.missing = FALSE),
                                                    fake.qtable, "foo"),
                   single.warning)
    fake.qtable <- array(c(Inf, -Inf), dim = c(4, 2, 2),
                         dimnames = list(NULL, letters[1:2], LETTERS[1:2]))
    attr(fake.qtable, "questions") <- "Foo"
    all.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, TRUE), "foo"))
    expect_warning(checkOppositeInifinitiesByColumn(sumCols(fake.qtable, remove.missing = FALSE),
                                                    fake.qtable, "foo"),
                   all.warning)
})
