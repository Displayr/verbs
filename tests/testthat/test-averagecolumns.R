context("AverageEachColumn")

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


testMeanFunction <- function(x)
{
    mean(flipTransformations::AsNumeric(x, binary = FALSE), na.rm = TRUE)
}

quoted.function <- sQuote("AverageEachColumn")

test_that("Variables", {
    text.error <- capture_error(throwErrorInvalidDataForNumericFunc("Text", quoted.function))[["message"]]
    expect_error(AverageEachColumn(variable.Text),
                 text.error)
    text.variables.in.data.frame <- data.frame(variable.Binary, variable.Text)
    expect_error(AverageEachColumn(text.variables.in.data.frame),
                 text.error)
    date.variables.in.data.frame <- data.frame(variable.Date, variable.Binary)
    datetime.error <- capture_error(throwErrorInvalidDataForNumericFunc("Date/Time", quoted.function))[["message"]]
    expect_error(AverageEachColumn(date.variables.in.data.frame),
                 datetime.error)
    time.variables.in.data.frame <- data.frame(variable.Date, variable.Binary)
    expect_error(AverageEachColumn(time.variables.in.data.frame),
                 datetime.error)
    expect_equal(AverageEachColumn(variable.Nominal),
                 c(Age = testMeanFunction(variable.Nominal)))
    variables.in.data.frame <- data.frame(`Coca-Cola` = variable.Binary,
                                          Age = variable.Numeric,
                                          Age = variable.Nominal,
                                          check.names = FALSE)
    expect_equal(AverageEachColumn(variables.in.data.frame),
                 c("Coca-Cola" = testMeanFunction(variable.Binary),
                   Age = testMeanFunction(variable.Numeric),
                   Age = testMeanFunction(variable.Nominal)))
    variables.in.data.frame <- data.frame(variable.Binary, variable.Nominal)
    expect_equal(AverageEachColumn(variables.in.data.frame),
                 c("Coca-Cola" = testMeanFunction(variable.Binary),
                   "Age" = testMeanFunction(variable.Nominal)))
    # Warnings for factors
    ## No extra warning for variables that are converted using value attributes
    data.frame.input <- data.frame(variable.Binary, variable.Nominal)
    captured.warnings <- capture_warnings(AverageEachColumn(data.frame.input, warn = TRUE))
    missing.value.warning <- capture_warnings(throwWarningAboutMissingValuesIgnored())
    expect_equal(captured.warnings, missing.value.warning)
    ## AsNumeric warning should be appearing when factor converted that has no value attributes
    factor.conv.warning <- capture_warnings(flipTransformations::AsNumeric(factor(1:2), binary = FALSE))
    expect_warning(AverageEachColumn(data.frame(1:5, factor(1:5)), warn = TRUE),
                   factor.conv.warning,
                   fixed = TRUE)
    # Missing values
    data.frame.input <- data.frame(`Coca-Cola` = variable.Binary,
                                   Age = variable.Numeric,
                                   Age = variable.Nominal,
                                   check.names = FALSE)
    expect_equal(AverageEachColumn(data.frame.input, remove.missing = FALSE),
                 c("Coca-Cola" = NA_integer_, Age = NA_integer_, Age = NA_integer_))
    # NaN values
    test.nan <- variable.Binary
    is.na(test.nan) <- 1:length(variable.Binary)
    data.frame.input <- data.frame(`Coca-Cola` = test.nan,
                                   Age = variable.Numeric,
                                   Age = variable.Nominal,
                                   check.names = FALSE)
    expect_equal(AverageEachColumn(data.frame.input, remove.missing = TRUE),
                 c("Coca-Cola" = NaN,
                   Age = testMeanFunction(variable.Numeric),
                   Age = testMeanFunction(variable.Nominal)))
})

test_that("Variables with weights, filters (subset), and a combination of the two", {
    # Variables and multiple variables
    subset.missing.out <- !is.na(variable.Numeric)
    nominal.to.numeric <- flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)
    expect_equal(AverageEachColumn(variable.Numeric, subset = subset.missing.out),
                 c(Age = testMeanFunction(variable.Numeric[subset.missing.out])))
    df <- data.frame(Age = variable.Numeric, Age = variable.Nominal, check.names = FALSE)
    expect_equal(AverageEachColumn(df),
                 c(Age = testMeanFunction(variable.Numeric),
                   Age = testMeanFunction(nominal.to.numeric)))
    subset.error <- capture_error(throwErrorSubsetOrWeightsWrongSize("subset", length(variable.Numeric), 10L))[["message"]]
    expect_error(AverageEachColumn(variable.Numeric[1:10], subset = subset.missing.out),
                 subset.error)
    weights <- runif(length(variable.Numeric))
    expect_equal(AverageEachColumn(variable.Numeric, weights = weights),
                 c(Age = flipStatistics::Mean(variable.Numeric, weights = weights)))
    df.input <- data.frame(Age = variable.Numeric, `Coca-Cola` = variable.Binary, check.names = FALSE)
    expect_equal(AverageEachColumn(df.input, weights = weights),
                 c(Age = flipStatistics::Mean(variable.Numeric, weights = weights),
                   `Coca-Cola` = flipStatistics::Mean(variable.Binary, weights = weights)))
    df.input <- data.frame(Age = variable.Numeric, Age = variable.Nominal, check.names = FALSE)
    expect_equal(AverageEachColumn(df.input,
                                weights = weights,
                                subset = subset.missing.out),
                 c(Age = flipStatistics::Mean(variable.Numeric, weights = weights),
                   Age = flipStatistics::Mean(flipTransformations::AsNumeric(variable.Nominal, binary = FALSE),
                                              weights = weights)))
    weights.error <- capture_error(throwErrorSubsetOrWeightsWrongSize("weights", 10L, length(variable.Numeric)))[["message"]]
    expect_error(AverageEachColumn(variable.Numeric, weights = weights[1:10]),
                 weights.error)
    # Variable sets and data.frames
    expect_equal(AverageEachColumn(data.frame(variable.Binary, variable.Nominal),
                            subset = subset.missing.out, remove.missing = FALSE),
                 c("Coca-Cola" = NA,
                   "Age" = flipStatistics::Mean(flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)[subset.missing.out])))
    subset.binary <- !is.na(variable.Binary)
    expect_equal(AverageEachColumn(data.frame(variable.Binary, variable.Nominal),
                            subset = subset.binary, weights = weights,
                            remove.missing = FALSE),
                 c("Coca-Cola" = flipStatistics::Mean(variable.Binary[subset.binary],
                                                            weights = weights[subset.binary]),
                   "Age" = NA))
    df <- data.frame(variable.Binary,
                     variable.Nominal = flipTransformations::AsNumeric(variable.Nominal, binary = FALSE))
    weighted.df <- df * weights
    expected.sum <- setNames(colSums(weighted.df, na.rm = TRUE), c("Coca-Cola", "Age"))
    expect_equal(AverageEachColumn(data.frame(variable.Binary, variable.Nominal),
                                weights = weights,
                                remove.missing = TRUE),
                 expected.sum/computeTotalWeights(df, weights = weights))
})

load("table1D.Average.rda")
load("table1D.Percentage.rda")
load("table.1D.MultipleStatistics.rda")

nonMissingElements <- function(x, remove.rows = c("NET", "SUM", "Total"))
{
    n.dim <- getDimensionLength(x)
    if (n.dim == 1)
        return(sum(!is.na(x)))
    colSums(!is.na(x))
}

test_that("Table 1D", {
    filtered.tab <- table1D.Percentage[names(table1D.Percentage) != "NET"]
    expect_equal(AverageEachColumn(table1D.Percentage),
                 c(`table.Age` = 100) / nonMissingElements(filtered.tab))
    filtered.tab <- table.1D.MultipleStatistics[-4,]
    expect_equal(AverageEachColumn(table.1D.MultipleStatistics),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
    single.opp.inf <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE), quoted.function))
    expect_warning(AverageEachColumn(table.1D.MultipleStatistics, warn = TRUE),
                   single.opp.inf)
})

load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")
test_that("Table 2D", {
    expect_equal(AverageEachColumn(table2D.Percentage),
                 colSums(table2D.Percentage, na.rm = TRUE) / nonMissingElements(table2D.Percentage))
    filtered.tab <- table2D.PercentageNaN[-8, ]
    expect_equal(AverageEachColumn(table2D.PercentageNaN),
                 colSums(filtered.tab, na.rm = TRUE)/ nonMissingElements(filtered.tab))
    filtered.tab <- table2D.Percentage[!rownames(table2D.Percentage) %in% c("NET", "SUM", "Total"), ]
    expect_equal(AverageEachColumn(table2D.Percentage),
                 colSums(filtered.tab) / nonMissingElements(filtered.tab))
    expect_equal(AverageEachColumn(table2D.PercentageAndCount),
                 colSums(table2D.PercentageAndCount) / nonMissingElements(table2D.PercentageAndCount))
    transposed.table <- aperm(table2D.PercentageAndCount, c(2, 1, 3))
    # attr(transposed.table, "questions") <- attr(table2D.PercentageAndCount, "questions")
    # attr(transposed.table, "name") <- attr(table2D.PercentageAndCount, "name")
    # expect_equal(AverageEachColumn(transposed.table), AverageRows(table2D.PercentageAndCount))
    # Extra category removed removed and warn about missing value removal
    filtered.tab <- table2D.PercentageNaN[1:6, ]
    missing.val.warning <- capture_warnings(throwWarningAboutMissingValuesIgnored())
    expect_equal(expect_warning(output.wo.missing <- AverageEachColumn(table2D.PercentageNaN,
                                                                remove.rows = c("NET", "None of these"),
                                                                remove.missing = TRUE,
                                                                warn = TRUE),
                                missing.val.warning),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
    # Missing values
    expect_true(anyNA(AverageEachColumn(table2D.PercentageNaN, remove.missing = FALSE)))
    expect_false(anyNA(output.wo.missing))
})



test_that("Higher dim Q tables", {
    .flattenAndFilter <- function(x, remove.rows = c("NET", "SUM", "Total"))
    {
        filtered.tab <- flattenQTableKeepingMultipleStatistics(curr.table)
        if (length(dim(filtered.tab)) == 3L)
            filtered.tab[rownames(curr.table) != "SUM", ,]
        else
            filtered.tab[rownames(curr.table) != "SUM", ]
    }
    load("numeric.grid.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.with.multiple.stats.qtable
    filtered.tab <- .flattenAndFilter(curr.table)
    expect_equal(AverageEachColumn(curr.table),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
    load("numeric.grid.nominal.qtable.rda")
    curr.table <- numeric.grid.nominal.qtable
    filtered.tab <- .flattenAndFilter(curr.table)
    expect_equal(AverageEachColumn(curr.table),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
    load("numeric.grid.nominal.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.nominal.with.multiple.stats.qtable
    filtered.tab <- .flattenAndFilter(curr.table)
    expect_equal(AverageEachColumn(curr.table),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
    load("nominal.multi.nominal.qtable.rda")
    curr.table <- nominal.multi.nominal.qtable
    filtered.tab <- .flattenAndFilter(curr.table)
    expect_equal(AverageEachColumn(curr.table),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
    load("nominal.multi.nominal.multi.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.qtable
    filtered.tab <- .flattenAndFilter(curr.table)
    expect_equal(AverageEachColumn(curr.table),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
    load("nominal.multi.nominal.multi.with.multiple.stats.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.with.multiple.stats.qtable
    filtered.tab <- .flattenAndFilter(curr.table)
    expect_equal(AverageEachColumn(curr.table),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
})


test_that("Warnings", {
    missing.val.warn <- capture_warnings(throwWarningAboutMissingValuesIgnored())
    single.opp.inf.warn <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE), quoted.function))
    all.opp.inf.warn <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, TRUE), quoted.function))
    expect_warning(expect_equal(AverageEachColumn(table2D.PercentageNaN,
                                               remove.rows = c("None of these", "NET"),
                                               warn = TRUE),
                                colMeans(table2D.PercentageNaN[-(7:8), ], na.rm = TRUE)),
                   missing.val.warn)
    SUM.col <- matrix(rowSums(table.1D.MultipleStatistics), ncol = 1, dimnames = list(rep("", 4), "NET"))
    table.1D.MultipleStatistics.with.SUM.col <- cbind(table.1D.MultipleStatistics, SUM.col)
    table.1D.MultipleStatistics.with.SUM.col[1, 1] <- -Inf
    table.1D.MultipleStatistics.with.SUM.col <- CopyAttributes(table.1D.MultipleStatistics.with.SUM.col, table.1D.MultipleStatistics)
    expect_warning(expect_equal(AverageEachColumn(table.1D.MultipleStatistics.with.SUM.col,
                                           warn = TRUE),
                                colMeans(table.1D.MultipleStatistics.with.SUM.col[-4, ], na.rm = TRUE)),
                   single.opp.inf.warn)
    ## Same situation with data.frame
    df.input <- as.data.frame(table.1D.MultipleStatistics.with.SUM.col)
    expect_warning(expect_equal(AverageEachColumn(df.input, warn = TRUE),
                                colMeans(df.input[-4, ], na.rm = TRUE)),
                   single.opp.inf.warn)
    table.1D.MultipleStatistics.with.SUM.col[1, ] <- Inf
    table.1D.MultipleStatistics.with.SUM.col[2, ] <- -Inf
    expect_warning(expect_true(all(is.nan(AverageEachColumn(table.1D.MultipleStatistics.with.SUM.col,
                                                     warn = TRUE)))),
                   all.opp.inf.warn)
    df.input <- as.data.frame(table.1D.MultipleStatistics.with.SUM.col)
    expect_warning(expect_true(all(is.nan(AverageEachColumn(df.input, warn = TRUE)))),
                   all.opp.inf.warn)
    # Throw warning about filter and/or weights being ignored for Q Tables
    subset.table.warn <- capture_warnings(throwWarningThatSubsetOrWeightsNotApplicableToTable("a filter", 1, quoted.function))
    expect_warning(expect_equal(AverageEachColumn(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)), warn = TRUE),
                                c(`table.Frequency.of.drinking` = mean(table1D.Average[-4]))),
                   subset.table.warn)
    weight.table.warn <- capture_warnings(throwWarningThatSubsetOrWeightsNotApplicableToTable("weights", 1, quoted.function))
    expect_warning(expect_equal(AverageEachColumn(table1D.Average, weights = runif(10), warn = TRUE),
                                c(`table.Frequency.of.drinking` = mean(table1D.Average[-4]))),
                   weight.table.warn)
    subset.weight.table.warn <- capture_warnings(throwWarningThatSubsetOrWeightsNotApplicableToTable("a filter or weights", 1, quoted.function))
    expect_warning(expect_equal(AverageEachColumn(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)),
                                               weights = runif(10), warn = TRUE),
                                c(`table.Frequency.of.drinking` = mean(table1D.Average[-4]))),
                   subset.weight.table.warn)
    input.matrix <- matrix(c(Inf, -Inf, 1, 2), nrow = 2, dimnames = list(NULL, letters[1:2]))
    input.vect <- c(Inf, -Inf)
    expect_warning(AverageEachColumn(input.matrix, warn = TRUE),
                   single.opp.inf.warn)
    expect_warning(AverageEachColumn(matrix(c(Inf, -Inf), nrow = 2, ncol = 2), warn = TRUE),
                   all.opp.inf.warn)
})

test_that("NULL or entirely missing inputs handled correctly", {
    expect_true(is.nan(AverageEachColumn(NULL)))
    expect_true(is.nan(AverageEachColumn(NA, remove.missing = TRUE)))
    expect_true(is.na(AverageEachColumn(NA, remove.missing = FALSE)))
})

