context("AverageColumns")

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

test_that("Variables", {
    expect_error(AverageColumns(variable.Text),
                 paste0("Text data has been supplied but ", sQuote('AverageColumns'), " requires numeric data."))
    text.variables.in.data.frame <- data.frame(variable.Binary, variable.Text)
    expect_error(AverageColumns(text.variables.in.data.frame),
                 paste0("Text data has been supplied but ", sQuote('AverageColumns'), " requires numeric data."))
    date.variables.in.data.frame <- data.frame(variable.Date, variable.Binary)
    expect_error(AverageColumns(date.variables.in.data.frame),
                 paste0("Date/Time data has been supplied but ", sQuote('AverageColumns'), " requires numeric data."))
    time.variables.in.data.frame <- data.frame(variable.Date, variable.Binary)
    expect_error(AverageColumns(time.variables.in.data.frame),
                 paste0("Date/Time data has been supplied but ", sQuote('AverageColumns'), " requires numeric data."))
    expect_equal(AverageColumns(variable.Nominal),
                 c(Age = testMeanFunction(variable.Nominal)))
    variables.in.data.frame <- data.frame(`Coca-Cola` = variable.Binary,
                                          Age = variable.Numeric,
                                          Age = variable.Nominal,
                                          check.names = FALSE)
    expect_equal(AverageColumns(variables.in.data.frame),
                 c("Coca-Cola" = testMeanFunction(variable.Binary),
                   Age = testMeanFunction(variable.Numeric),
                   Age = testMeanFunction(variable.Nominal)))
    variables.in.data.frame <- data.frame(variable.Binary, variable.Nominal)
    expect_equal(AverageColumns(variables.in.data.frame),
                 c("Coca-Cola" = testMeanFunction(variable.Binary),
                   "Age" = testMeanFunction(variable.Nominal)))
    # Warnings for factors
    ## No extra warning for variables that are converted using value attributes
    data.frame.input <- data.frame(variable.Binary, variable.Nominal)
    captured.warnings <- capture_warnings(AverageColumns(data.frame.input, warn = TRUE))
    expect_equal(captured.warnings, "Missing values have been ignored in calculation.")
    ## AsNumeric warning should be appearing when factor converted that has no value attributes
    expect_warning(AverageColumns(data.frame(1:5, factor(1:5)), warn = TRUE),
                   paste0("Data has been automatically converted to numeric. ",
                          "Values are assigned according to the labels of the ",
                          "categories. To use alternative numeric values, ",
                          "transform the data prior including it in this ",
                          "analysis (e.g. by changing its structure)."),
                   fixed = TRUE)
    # Missing values
    data.frame.input <- data.frame(`Coca-Cola` = variable.Binary,
                                   Age = variable.Numeric,
                                   Age = variable.Nominal,
                                   check.names = FALSE)
    expect_equal(AverageColumns(data.frame.input, remove.missing = FALSE),
                 c("Coca-Cola" = NA_integer_, Age = NA_integer_, Age = NA_integer_))
    # NaN values
    test.nan <- variable.Binary
    is.na(test.nan) <- 1:length(variable.Binary)
    data.frame.input <- data.frame(`Coca-Cola` = test.nan,
                                   Age = variable.Numeric,
                                   Age = variable.Nominal,
                                   check.names = FALSE)
    expect_equal(AverageColumns(data.frame.input, remove.missing = TRUE),
                 c("Coca-Cola" = NaN,
                   Age = testMeanFunction(variable.Numeric),
                   Age = testMeanFunction(variable.Nominal)))
})

test_that("Variables with weights, filters (subset), and a combination of the two", {
    # Variables and multiple variables
    subset.missing.out <- !is.na(variable.Numeric)
    nominal.to.numeric <- flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)
    expect_equal(AverageColumns(variable.Numeric, subset = subset.missing.out),
                 c(Age = testMeanFunction(variable.Numeric[subset.missing.out])))
    df <- data.frame(Age = variable.Numeric, Age = variable.Nominal, check.names = FALSE)
    expect_equal(AverageColumns(df),
                 c(Age = testMeanFunction(variable.Numeric),
                   Age = testMeanFunction(nominal.to.numeric)))
    expect_error(AverageColumns(variable.Numeric[1:10], subset = subset.missing.out),
                 paste0("The subset vector has length 327. However, it needs to ",
                        "have length 10 to match the number of cases in the supplied input data."))
    weights <- runif(length(variable.Numeric))
    expect_equal(AverageColumns(variable.Numeric, weights = weights),
                 c(Age = flipStatistics::Mean(variable.Numeric, weights = weights)))
    df.input <- data.frame(Age = variable.Numeric, `Coca-Cola` = variable.Binary, check.names = FALSE)
    expect_equal(AverageColumns(df.input, weights = weights),
                 c(Age = flipStatistics::Mean(variable.Numeric, weights = weights),
                   `Coca-Cola` = flipStatistics::Mean(variable.Binary, weights = weights)))
    df.input <- data.frame(Age = variable.Numeric, Age = variable.Nominal, check.names = FALSE)
    expect_equal(AverageColumns(df.input,
                                weights = weights,
                                subset = subset.missing.out),
                 c(Age = flipStatistics::Mean(variable.Numeric, weights = weights),
                   Age = flipStatistics::Mean(flipTransformations::AsNumeric(variable.Nominal, binary = FALSE),
                                              weights = weights)))
    expect_error(AverageColumns(variable.Numeric, weights = weights[1:10]),
                 paste0("The weights vector has length 10. However, it needs to ",
                        "have length 327 to match the number of cases in the supplied input data."))
    # Variable sets and data.frames
    expect_equal(AverageColumns(data.frame(variable.Binary, variable.Nominal),
                            subset = subset.missing.out, remove.missing = FALSE),
                 c("Coca-Cola" = NA,
                   "Age" = flipStatistics::Mean(flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)[subset.missing.out])))
    subset.binary <- !is.na(variable.Binary)
    expect_equal(AverageColumns(data.frame(variable.Binary, variable.Nominal),
                            subset = subset.binary, weights = weights,
                            remove.missing = FALSE),
                 c("Coca-Cola" = flipStatistics::Mean(variable.Binary[subset.binary],
                                                            weights = weights[subset.binary]),
                   "Age" = NA))
    df <- data.frame(variable.Binary,
                     variable.Nominal = flipTransformations::AsNumeric(variable.Nominal, binary = FALSE))
    weighted.df <- df * weights
    expected.sum <- setNames(colSums(weighted.df, na.rm = TRUE), c("Coca-Cola", "Age"))
    expect_equal(AverageColumns(data.frame(variable.Binary, variable.Nominal),
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
    expect_equal(AverageColumns(table1D.Percentage),
                 c(`table.Age` = 100) / nonMissingElements(filtered.tab))
    filtered.tab <- table.1D.MultipleStatistics[-4,]
    expect_equal(AverageColumns(table.1D.MultipleStatistics),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
    expect_warning(AverageColumns(table.1D.MultipleStatistics, warn = TRUE),
                   paste0(sQuote("AverageColumns"), " cannot compute some values as the ",
                          "data contains both Inf and -Inf."))
})

load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")
test_that("Table 2D", {
    expect_equal(AverageColumns(table2D.Percentage),
                 colSums(table2D.Percentage, na.rm = TRUE) / nonMissingElements(table2D.Percentage))
    filtered.tab <- table2D.PercentageNaN[-8, ]
    expect_equal(AverageColumns(table2D.PercentageNaN),
                 colSums(filtered.tab, na.rm = TRUE)/ nonMissingElements(filtered.tab))
    filtered.tab <- table2D.Percentage[!rownames(table2D.Percentage) %in% c("NET", "SUM", "Total"), ]
    expect_equal(AverageColumns(table2D.Percentage),
                 colSums(filtered.tab) / nonMissingElements(filtered.tab))
    expect_equal(AverageColumns(table2D.PercentageAndCount),
                 colSums(table2D.PercentageAndCount) / nonMissingElements(table2D.PercentageAndCount))
    transposed.table <- aperm(table2D.PercentageAndCount, c(2, 1, 3))
    # attr(transposed.table, "questions") <- attr(table2D.PercentageAndCount, "questions")
    # attr(transposed.table, "name") <- attr(table2D.PercentageAndCount, "name")
    # expect_equal(AverageColumns(transposed.table), AverageRows(table2D.PercentageAndCount))
    # Extra category removed removed and warn about missing value removal
    filtered.tab <- table2D.PercentageNaN[1:6, ]
    expect_equal(expect_warning(output.wo.missing <- AverageColumns(table2D.PercentageNaN,
                                                                remove.rows = c("NET", "None of these"),
                                                                remove.missing = TRUE,
                                                                warn = TRUE),
                                "Missing values have been ignored in calculation"),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
    # Missing values
    expect_true(anyNA(AverageColumns(table2D.PercentageNaN, remove.missing = FALSE)))
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
    expect_equal(AverageColumns(curr.table),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
    load("numeric.grid.nominal.qtable.rda")
    curr.table <- numeric.grid.nominal.qtable
    filtered.tab <- .flattenAndFilter(curr.table)
    expect_equal(AverageColumns(curr.table),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
    load("numeric.grid.nominal.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.nominal.with.multiple.stats.qtable
    filtered.tab <- .flattenAndFilter(curr.table)
    expect_equal(AverageColumns(curr.table),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
    load("nominal.multi.nominal.qtable.rda")
    curr.table <- nominal.multi.nominal.qtable
    filtered.tab <- .flattenAndFilter(curr.table)
    expect_equal(AverageColumns(curr.table),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
    load("nominal.multi.nominal.multi.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.qtable
    filtered.tab <- .flattenAndFilter(curr.table)
    expect_equal(AverageColumns(curr.table),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
    load("nominal.multi.nominal.multi.with.multiple.stats.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.with.multiple.stats.qtable
    filtered.tab <- .flattenAndFilter(curr.table)
    expect_equal(AverageColumns(curr.table),
                 colSums(filtered.tab, na.rm = TRUE) / nonMissingElements(filtered.tab))
})


test_that("Warnings", {
    expect_warning(expect_equal(AverageColumns(table2D.PercentageNaN,
                                               remove.rows = c("None of these", "NET"),
                                               warn = TRUE),
                                colMeans(table2D.PercentageNaN[-(7:8), ], na.rm = TRUE)),
                   "Missing values have been ignored in calculation.")
    SUM.col <- matrix(rowSums(table.1D.MultipleStatistics), ncol = 1, dimnames = list(rep("", 4), "NET"))
    table.1D.MultipleStatistics.with.SUM.col <- cbind(table.1D.MultipleStatistics, SUM.col)
    table.1D.MultipleStatistics.with.SUM.col[1, 1] <- -Inf
    table.1D.MultipleStatistics.with.SUM.col <- CopyAttributes(table.1D.MultipleStatistics.with.SUM.col, table.1D.MultipleStatistics)
    expect_warning(expect_equal(AverageColumns(table.1D.MultipleStatistics.with.SUM.col,
                                           warn = TRUE),
                                colMeans(table.1D.MultipleStatistics.with.SUM.col[-4, ], na.rm = TRUE)),
                   paste0(sQuote("AverageColumns"), " cannot compute some values as the data contains both Inf and -Inf."))
    ## Same situation with data.frame
    df.input <- as.data.frame(table.1D.MultipleStatistics.with.SUM.col)
    expect_warning(expect_equal(AverageColumns(df.input, warn = TRUE),
                                colMeans(df.input[-4, ], na.rm = TRUE)),
                   paste0(sQuote("AverageColumns"), " cannot compute some values as the data contains both Inf and -Inf."))
    table.1D.MultipleStatistics.with.SUM.col[1, ] <- Inf
    table.1D.MultipleStatistics.with.SUM.col[2, ] <- -Inf
    expect_warning(expect_true(all(is.nan(AverageColumns(table.1D.MultipleStatistics.with.SUM.col,
                                                     warn = TRUE)))),
                   paste0(sQuote("AverageColumns"), " cannot be computed as the data contains both Inf and -Inf."))
    df.input <- as.data.frame(table.1D.MultipleStatistics.with.SUM.col)
    expect_warning(expect_true(all(is.nan(AverageColumns(df.input, warn = TRUE)))),
                   paste0(sQuote("AverageColumns"), " cannot be computed as the data contains both Inf and -Inf."))
    # Throw warning about filter and/or weights being ignored for Q Tables
    expect_warning(expect_equal(AverageColumns(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)), warn = TRUE),
                                c(`table.Frequency.of.drinking` = mean(table1D.Average[-4]))),
                   paste0(sQuote("AverageColumns"), " is unable to apply a filter to the input Q Table ",
                          "since the original variable data is unavailable."))
    expect_warning(expect_equal(AverageColumns(table1D.Average, weights = runif(10), warn = TRUE),
                                c(`table.Frequency.of.drinking` = mean(table1D.Average[-4]))),
                   paste0(sQuote("AverageColumns"), " is unable to apply weights to the input Q Table ",
                          "since the original variable data is unavailable."))
    expect_warning(expect_equal(AverageColumns(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)),
                                               weights = runif(10), warn = TRUE),
                                c(`table.Frequency.of.drinking` = mean(table1D.Average[-4]))),
                   paste0(sQuote("AverageColumns"), " is unable to apply a filter or weights to the input Q Table ",
                          "since the original variable data is unavailable."))
    input.matrix <- matrix(c(Inf, -Inf, 1, 2), nrow = 2, dimnames = list(NULL, letters[1:2]))
    input.vect <- c(Inf, -Inf)
    expect_warning(AverageColumns(input.matrix, warn = TRUE),
                   paste0(sQuote("AverageColumns"), " cannot compute some values as the data contains both Inf and -Inf."))
    expect_warning(AverageColumns(matrix(c(Inf, -Inf), nrow = 2, ncol = 2), warn = TRUE),
                   paste0(sQuote("AverageColumns"), " cannot be computed as the data contains both Inf and -Inf."))
})

test_that("NULL or entirely missing inputs handled correctly", {
    expect_true(is.nan(AverageColumns(NULL)))
    expect_true(is.nan(AverageColumns(NA, remove.missing = TRUE)))
    expect_true(is.na(AverageColumns(NA, remove.missing = FALSE)))
})

