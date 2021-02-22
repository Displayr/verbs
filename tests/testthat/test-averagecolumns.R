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
    expect_error(AverageColumns(variable.Binary, variable.Text),
                 paste0("Text data has been supplied but ", sQuote('AverageColumns'), " requires numeric data."))
    expect_error(AverageColumns(variable.Date, variable.Binary),
                 paste0("Date/Time data has been supplied but ", sQuote('AverageColumns'), " requires numeric data."))
    expect_error(AverageColumns(variable.Time, variable.Numeric),
                 paste0("Date/Time data has been supplied but ", sQuote('AverageColumns'), " requires numeric data."))
    expect_equal(AverageColumns(variable.Nominal),
                 c(Age = testMeanFunction(variable.Nominal)))
    expect_equal(AverageColumns(variable.Binary, variable.Numeric, variable.Nominal),
                 c("Coca-Cola" = testMeanFunction(variable.Binary),
                   Age = testMeanFunction(variable.Numeric),
                   Age = testMeanFunction(variable.Nominal)))
    expect_equal(AverageColumns(data.frame(variable.Binary, variable.Nominal)),
                 c("variable.Binary" = testMeanFunction(variable.Binary),
                   "variable.Nominal" = testMeanFunction(variable.Nominal)))
    # Warnings for factors
    ## No extra warning for variables that are converted using value attributes
    captured.warnings <- capture_warnings(AverageColumns(variable.Binary, variable.Nominal, warn = TRUE))
    expect_equal(captured.warnings, "Missing values have been ignored in calculation.")
    ## AsNumeric warning should be appearing when factor converted that has no value attributes
    expect_warning(AverageColumns(1:5, factor(1:5), warn = TRUE),
                   paste0("Data has been automatically converted to numeric. ",
                          "Values are assigned according to the labels of the ",
                          "categories. To use alternative numeric values, ",
                          "transform the data prior including it in this ",
                          "analysis (e.g. by changing its structure)."),
                   fixed = TRUE)
    # Missing values
    expect_equal(AverageColumns(variable.Binary, variable.Numeric, variable.Nominal,
                            remove.missing = FALSE),
                 c("Coca-Cola" = NA_integer_, Age = NA_integer_, Age = NA_integer_))
    # NaN values
    test.nan <- variable.Binary
    is.na(test.nan) <- 1:length(variable.Binary)
    expect_equal(AverageColumns(test.nan, variable.Numeric, variable.Nominal,
                                remove.missing = TRUE),
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
    expect_equal(AverageColumns(variable.Numeric, variable.Nominal, subset = subset.missing.out),
                 c(Age = testMeanFunction(variable.Numeric),
                   Age = testMeanFunction(nominal.to.numeric)))
    expect_error(AverageColumns(variable.Numeric[1:10], subset = subset.missing.out),
                 paste0("The subset vector has length 327. However, it needs to ",
                        "have length 10 to match the number of cases in the supplied input data."))
    expect_error(AverageColumns(variable.Numeric, 1:10, subset = subset.missing.out),
                 paste0(sQuote("AverageColumns"), " requires all input elements to have the same size to be able ",
                        "to apply a filter or weight vector. ",
                        verbs:::determineAppropriateContact()))
    weights <- runif(length(variable.Numeric))
    expect_equal(AverageColumns(variable.Numeric, weights = weights),
                 c(Age = flipStatistics::Mean(variable.Numeric, weights = weights)))
    expect_equal(AverageColumns(variable.Numeric, variable.Binary, weights = weights),
                 c(Age = flipStatistics::Mean(variable.Numeric, weights = weights),
                   `Coca-Cola` = flipStatistics::Mean(variable.Binary, weights = weights)))
    expect_equal(AverageColumns(variable.Numeric, variable.Nominal,
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
                 c("variable.Binary" = NA,
                   "variable.Nominal" = flipStatistics::Mean(flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)[subset.missing.out])))
    subset.binary <- !is.na(variable.Binary)
    expect_equal(AverageColumns(data.frame(variable.Binary, variable.Nominal),
                            subset = subset.binary, weights = weights,
                            remove.missing = FALSE),
                 c("variable.Binary" = flipStatistics::Mean(variable.Binary[subset.binary],
                                                            weights = weights[subset.binary]),
                   "variable.Nominal" = NA))
    df <- data.frame(variable.Binary,
                     variable.Nominal = flipTransformations::AsNumeric(variable.Nominal, binary = FALSE))
    weighted.df <- df * weights
    expected.sum <- colSums(weighted.df, na.rm = TRUE)
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
    n.dim <- getDim(x)
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

test_that("Multiple tables and multiple basic inputs", {
    table.name <- attr(table.1D.MultipleStatistics, "name")
    expect_error(AverageColumns(table.1D.MultipleStatistics, table1D.Average),
                 paste0(sQuote("AverageColumns"), " doesn't support Tables when more than one input is provided. ",
                        "Either remove the input ", table.name, " and any other Tables from the ",
                        "input or call ", sQuote("AverageColumns"), " again with only ", table.name, " ",
                        "as the input."),
                 fixed = TRUE)
    input.matrix <- matrix(c(1, 2, 2, 5), nrow = 2, dimnames = list(letters[1:2], c("Q1", "Q2")))
    input.column.vector <- array(c(1:3, 6), dim = c(4, 1), dimnames = list(c(letters[1:3], "SUM"), "Q3"))
    expect_equal(AverageColumns(input.matrix, input.column.vector),
                 colMeans(cbind(rbind(input.matrix, NA), "Q3" = input.column.vector[-4, ]), na.rm = TRUE))
    inputs <- list(c(1:3), c(1:4))
    expect_equal(do.call(AverageColumns, inputs),
                 vapply(inputs, mean, numeric(1L)))
    inputs <- list(c(1:3), c(1:4), c(1:10))
    expect_equal(do.call(AverageColumns, inputs),
                 vapply(inputs, mean, numeric(1L)))
    inputs <- list(c(1:4), matrix(1:12, nrow = 4))
    expect_equal(do.call(AverageColumns, inputs),
                 c(mean(inputs[[1L]]), colMeans(inputs[[2L]])))
    inputs <- list(c(1:4), matrix(1:15, nrow = 5))
    expect_equal(do.call(AverageColumns, inputs),
                 c(mean(inputs[[1]]), colMeans(inputs[[2L]])))
})

test_that("Inappropriate multiple inputs", {
    expect_error(AverageColumns(c(1:4), array(1:16, dim = c(4, 2, 2))),
                 paste0(sQuote("AverageColumns"), " only supports inputs that have 1 or 2 dimensions. ",
                        "A supplied input has 3 dimensions. Contact support at ", contact.msg),
                 fixed = TRUE)
    expect_error(AverageColumns(c(1:4), list("hello")),
                 paste0(sQuote("AverageColumns"), " requires all input elements to be numeric vectors ",
                        "or reducible to individual numeric vectors such as a numeric matrix or data frame ",
                        "containing numeric elements. One of the provided input elements is a list"),
                 fixed = TRUE)
    list.input <- list("Hello")
    expect_error(AverageColumns(c(1:4), list.input),
                 paste0(sQuote("AverageColumns"), " requires all input elements to be numeric vectors ",
                        "or reducible to individual numeric vectors such as a numeric matrix or data frame ",
                        "containing numeric elements. One of the provided input elements is a list"),
                 fixed = TRUE)
    table.name <- attr(table.1D.MultipleStatistics, "name")
    expect_error(AverageColumns(1:nrow(table.1D.MultipleStatistics), table.1D.MultipleStatistics),
                 paste0(sQuote("AverageColumns"), " doesn't support Tables when more than one input is provided. ",
                        "Either remove the input ", table.name, " and any other Tables from the ",
                        "input or call ", sQuote("AverageColumns"), " again with only ", table.name, " ",
                        "as the input."),
                 fixed = TRUE)
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
    expect_warning(AverageColumns(input.matrix, input.vect, warn = TRUE),
                   paste0(sQuote("AverageColumns"), " cannot compute some values as the data contains both Inf and -Inf."))
    expect_warning(AverageColumns(input.matrix[, -2], input.vect, warn = TRUE),
                   paste0(sQuote("AverageColumns"), " cannot be computed as the data contains both Inf and -Inf."))
})
