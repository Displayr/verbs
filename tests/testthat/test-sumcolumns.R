context("SumForEachColumn")

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


test_that("Variables", {
    expect_error(SumForEachColumn(variable.Text),
                 paste0("Text data has been supplied but ", sQuote('SumForEachColumn'), " requires numeric data."))
    expect_error(SumForEachColumn(variable.Binary, variable.Text),
                 paste0("Text data has been supplied but ", sQuote('SumForEachColumn'), " requires numeric data."))
    expect_error(SumForEachColumn(variable.Date, variable.Binary),
                 paste0("Date/Time data has been supplied but ", sQuote('SumForEachColumn'), " requires numeric data."))
    expect_error(SumForEachColumn(variable.Time, variable.Numeric),
                 paste0("Date/Time data has been supplied but ", sQuote('SumForEachColumn'), " requires numeric data."))
    expect_equal(SumForEachColumn(variable.Nominal), c(Age = 12606))
    expect_equal(SumForEachColumn(variable.Binary, variable.Numeric, variable.Nominal),
                 c("Coca-Cola" = 155, Age = 12606, Age = 12606))
    expect_equal(SumForEachColumn(data.frame(variable.Binary, variable.Nominal)),
                 c("variable.Binary" = 155, "variable.Nominal" = 12606))
    # Warnings for factors
    ## No extra warning for variables that are converted using value attributes
    captured.warnings <- capture_warnings(SumForEachColumn(variable.Binary, variable.Nominal, warn = TRUE))
    expect_equal(captured.warnings, "Missing values have been ignored in calculation.")
    ## AsNumeric warning should be appearing when factor converted that has no value attributes
    expect_warning(SumForEachColumn(1:5, factor(1:5), warn = TRUE),
                   paste0("Data has been automatically converted to numeric. ",
                          "Values are assigned according to the labels of the ",
                          "categories. To use alternative numeric values, ",
                          "transform the data prior including it in this ",
                          "analysis (e.g. by changing its structure)."),
                   fixed = TRUE)
    # Missing values
    expect_equal(SumForEachColumn(variable.Binary, variable.Numeric, variable.Nominal,
                            remove.missing = FALSE),
                 c("Coca-Cola" = NA_integer_, Age = NA_integer_, Age = NA_integer_))
})

test_that("Variables with weights, filters (subset), and a combination of the two", {
    # Variables and multiple variables
    subset.missing.out <- !is.na(variable.Numeric)
    nominal.to.numeric <- flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)
    expect_equal(SumForEachColumn(variable.Numeric, subset = subset.missing.out),
                 c(Age = sum(variable.Numeric, na.rm = TRUE)))
    expect_equal(SumForEachColumn(variable.Numeric, variable.Nominal, subset = subset.missing.out),
                 c(Age = sum(variable.Numeric, na.rm = TRUE),
                   Age = sum(nominal.to.numeric, na.rm = TRUE)))
    expect_error(SumForEachColumn(variable.Numeric[1:10], subset = subset.missing.out),
                 paste0("The subset vector has length 327. However, it needs to ",
                        "have length 10 to match the number of cases in the supplied input data."))
    expect_error(SumForEachColumn(variable.Numeric, 1:10, subset = subset.missing.out),
                 paste0(sQuote("SumForEachColumn"), " requires all input elements to have the same size to be able ",
                        "to apply a filter or weight vector. ",
                        verbs:::determineAppropriateContact()))
    weights <- runif(length(variable.Numeric))
    expect_equal(SumForEachColumn(variable.Numeric, weights = weights),
                 c(Age = sum(variable.Numeric * weights, na.rm = TRUE)))
    expect_equal(SumForEachColumn(variable.Numeric, variable.Binary, weights = weights),
                 c(Age = sum(variable.Numeric * weights, na.rm = TRUE),
                   `Coca-Cola` = sum(variable.Binary * weights, na.rm = TRUE)))
    expect_equal(SumForEachColumn(variable.Numeric, variable.Nominal,
                                  weights = weights,
                                  subset = subset.missing.out),
                 c(Age = sum(variable.Numeric * weights, na.rm = TRUE),
                   Age = sum(nominal.to.numeric * weights, na.rm = TRUE)))
    expect_error(SumForEachColumn(variable.Numeric, weights = weights[1:10]),
                 paste0("The weights vector has length 10. However, it needs to ",
                        "have length 327 to match the number of cases in the supplied input data."))
    # Variable sets and data.frames
    expect_equal(SumForEachColumn(data.frame(variable.Binary, variable.Nominal),
                                  subset = subset.missing.out, remove.missing = FALSE),
                 c("variable.Binary" = NA, "variable.Nominal" = 12606))
    subset.binary <- !is.na(variable.Binary)
    expected.weighted.bin <- sum(variable.Binary * weights, na.rm = TRUE)
    expect_equal(SumForEachColumn(data.frame(variable.Binary, variable.Nominal),
                                  subset = subset.binary, weights = weights,
                                  remove.missing = FALSE),
                 c("variable.Binary" = expected.weighted.bin, "variable.Nominal" = NA))
    df <- data.frame(variable.Binary,
                     variable.Nominal = flipTransformations::AsNumeric(variable.Nominal, binary = FALSE))
    weighted.df <- df * weights
    expected.sum <- colSums(weighted.df, na.rm = TRUE)
    expect_equal(SumForEachColumn(data.frame(variable.Binary, variable.Nominal),
                                  weights = weights,
                                  remove.missing = TRUE),
                 expected.sum)

})

load("table1D.Average.rda")
load("table1D.Percentage.rda")
load("table.1D.MultipleStatistics.rda")

test_that("Table 1D", {
    expect_equal(SumForEachColumn(table1D.Percentage), c(`table.Age` = 100))
    expect_equal(SumForEachColumn(table.1D.MultipleStatistics),
                 colSums(table.1D.MultipleStatistics[-4,], na.rm = TRUE))
    expect_warning(SumForEachColumn(table.1D.MultipleStatistics, warn = TRUE),
                   paste0(sQuote("SumForEachColumn"), " cannot compute some values as the ",
                          "data contains both Inf and -Inf."))
})

load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")
test_that("Table 2D", {
    expect_equal(SumForEachColumn(table2D.Percentage),
                 colSums(table2D.Percentage, na.rm = TRUE))
    expect_equal(SumForEachColumn(table2D.PercentageNaN),
                 colSums(table2D.PercentageNaN[-8, ], na.rm = TRUE))
    expect_equal(SumForEachColumn(table2D.Percentage),
                 SumForEachRow(t(table2D.Percentage)))
    col.summed.2d.table.multi.stats <- cbind(`Row %` = colSums(table2D.PercentageAndCount[, , 1]),
                                             `Count` = colSums(table2D.PercentageAndCount[, , 2]))
    expect_equal(SumForEachColumn(table2D.PercentageAndCount),
                 col.summed.2d.table.multi.stats)
    transposed.table <- aperm(table2D.PercentageAndCount, c(2, 1, 3))
    attr(transposed.table, "questions") <- attr(table2D.PercentageAndCount, "questions")
    attr(transposed.table, "name") <- attr(table2D.PercentageAndCount, "name")
    expect_equal(SumForEachColumn(transposed.table), SumForEachRow(table2D.PercentageAndCount))
    # Extra category removed removed and warn about missing value removal
    expect_warning(output.wo.missing <- SumForEachColumn(table2D.PercentageNaN,
                                                         remove.rows = c("NET", "None of these"),
                                                         remove.missing = TRUE,
                                                         warn = TRUE),
                   "Missing values have been ignored in calculation")
    expect_equal(output.wo.missing,
                 colSums(table2D.PercentageNaN[1:6, ], na.rm = TRUE))
    # Missing values
    expect_true(anyNA(SumForEachColumn(table2D.PercentageNaN, remove.missing = FALSE)))
    expect_false(anyNA(output.wo.missing))
})

test_that("Higher dim Q tables", {
    load("numeric.grid.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.with.multiple.stats.qtable
    expect_equal(SumForEachColumn(curr.table),
                 colSums(flattenQTableKeepingMultipleStatistics(curr.table)[rownames(curr.table) != "SUM", ,],
                         na.rm = TRUE))
    load("numeric.grid.nominal.qtable.rda")
    curr.table <- numeric.grid.nominal.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.row.names <- row.names(as.matrix(flattened.table))
    expect_equal(SumForEachColumn(curr.table),
                 colSums(flattenQTableKeepingMultipleStatistics(curr.table)[flat.row.names != "SUM", ],
                         na.rm = TRUE))
    load("numeric.grid.nominal.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.nominal.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expect_equal(SumForEachColumn(curr.table),
                 colSums(flattenQTableKeepingMultipleStatistics(curr.table)[flat.row.names != "SUM", ,],
                         na.rm = TRUE))
    load("nominal.multi.nominal.qtable.rda")
    curr.table <- nominal.multi.nominal.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expect_equal(SumForEachColumn(curr.table),
                 colSums(flattenQTableKeepingMultipleStatistics(curr.table)[flat.row.names != "SUM", ],
                         na.rm = TRUE))
    load("nominal.multi.nominal.multi.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expect_equal(SumForEachColumn(curr.table),
                 colSums(flattenQTableKeepingMultipleStatistics(curr.table)[flat.row.names != "SUM",],
                         na.rm = TRUE))
    load("nominal.multi.nominal.multi.with.multiple.stats.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.row.names <- dimnames(flattened.table)[[1L]]
    expect_equal(SumForEachColumn(curr.table),
                 colSums(flattenQTableKeepingMultipleStatistics(curr.table)[flat.row.names != "SUM", ,],
                         na.rm = TRUE))
})

test_that("Multiple tables and multiple basic inputs", {
    table.name <- attr(table.1D.MultipleStatistics, "name")
    expect_error(SumForEachColumn(table.1D.MultipleStatistics, table1D.Average),
                 paste0(sQuote("SumForEachColumn"), " doesn't support Tables when more than one input is provided. ",
                        "Either remove the input ", table.name, " and any other Tables from the ",
                        "input or call ", sQuote("SumForEachColumn"), " again with only ", table.name, " ",
                        "as the input."),
                 fixed = TRUE)
    input.matrix <- matrix(c(1, 2, 2, 5), nrow = 2, dimnames = list(letters[1:2], c("Q1", "Q2")))
    input.column.vector <- array(c(1:3, 6), dim = c(4, 1), dimnames = list(c(letters[1:3], "SUM"), "Q3"))
    expect_equal(SumForEachColumn(input.matrix, input.column.vector),
                 c("Q1" = 3, "Q2" = 7, "Q3" = 6))
    expect_equal(SumForEachColumn(c(1:3), c(1:4)),
                 c(sum(1:3), sum(1:4)))
    expect_equal(SumForEachColumn(c(1:3), c(1:4), c(1:10)),
                 c(sum(1:3), sum(1:4), sum(1:10)))
    expect_equal(SumForEachColumn(c(1:4), matrix(1:12, nrow = 4)),
                 c(sum(1:4), colSums(matrix(1:12, nrow = 4))))
    expect_equal(SumForEachColumn(c(1:4), matrix(1:15, nrow = 5)),
                 c(sum(1:4), colSums(matrix(1:15, nrow = 5))))
})

test_that("Inappropriate multiple inputs", {
    expect_error(SumForEachColumn(c(1:4), array(1:16, dim = c(4, 2, 2))),
                 paste0(sQuote("SumForEachColumn"), " only supports inputs that have 1 or 2 dimensions. ",
                        "A supplied input has 3 dimensions. Contact support at ", contact.msg),
                 fixed = TRUE)
    expect_error(SumForEachColumn(c(1:4), list("hello")),
                 paste0(sQuote("SumForEachColumn"), " requires all input elements to be numeric vectors ",
                        "or reducible to individual numeric vectors such as a numeric matrix or data frame ",
                        "containing numeric elements. One of the provided input elements is a list"),
                 fixed = TRUE)
    table.name <- attr(table.1D.MultipleStatistics, "name")
    expect_error(SumForEachColumn(1:nrow(table.1D.MultipleStatistics), table.1D.MultipleStatistics),
                 paste0(sQuote("SumForEachColumn"), " doesn't support Tables when more than one input is provided. ",
                        "Either remove the input ", table.name, " and any other Tables from the ",
                        "input or call ", sQuote("SumForEachColumn"), " again with only ", table.name, " ",
                        "as the input."),
                 fixed = TRUE)
})

test_that("Warnings", {
    expect_warning(expect_equal(SumForEachColumn(table2D.PercentageNaN,
                                           remove.rows = c("None of these", "NET"),
                                           warn = TRUE),
                                colSums(table2D.PercentageNaN[-(7:8), ], na.rm = TRUE)),
                   "Missing values have been ignored in calculation.")
    SUM.col <- matrix(rowSums(table.1D.MultipleStatistics), ncol = 1, dimnames = list(rep("", 4), "NET"))
    table.1D.MultipleStatistics.with.SUM.col <- cbind(table.1D.MultipleStatistics, SUM.col)
    table.1D.MultipleStatistics.with.SUM.col[1, 1] <- -Inf
    table.1D.MultipleStatistics.with.SUM.col <- CopyAttributes(table.1D.MultipleStatistics.with.SUM.col, table.1D.MultipleStatistics)
    expect_warning(expect_equal(SumForEachColumn(table.1D.MultipleStatistics.with.SUM.col,
                                           warn = TRUE),
                                colSums(table.1D.MultipleStatistics.with.SUM.col[-4, ], na.rm = TRUE)),
                   paste0(sQuote("SumForEachColumn"), " cannot compute some values as the data contains both Inf and -Inf."))
    ## Same situation with data.frame
    df.input <- as.data.frame(table.1D.MultipleStatistics.with.SUM.col)
    expect_warning(expect_equal(SumForEachColumn(df.input, warn = TRUE),
                                colSums(df.input[-4, ], na.rm = TRUE)),
                   paste0(sQuote("SumForEachColumn"), " cannot compute some values as the data contains both Inf and -Inf."))
    table.1D.MultipleStatistics.with.SUM.col[1, ] <- Inf
    table.1D.MultipleStatistics.with.SUM.col[2, ] <- -Inf
    expect_warning(expect_true(all(is.nan(SumForEachColumn(table.1D.MultipleStatistics.with.SUM.col,
                                                     warn = TRUE)))),
                   paste0(sQuote("SumForEachColumn"), " cannot be computed as the data contains both Inf and -Inf."))
    df.input <- as.data.frame(table.1D.MultipleStatistics.with.SUM.col)
    expect_warning(expect_true(all(is.nan(SumForEachColumn(df.input, warn = TRUE)))),
                   paste0(sQuote("SumForEachColumn"), " cannot be computed as the data contains both Inf and -Inf."))
    # Throw warning about filter and/or weights being ignored for Q Tables
    expect_warning(expect_equal(SumForEachColumn(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)), warn = TRUE),
                                c(`table.Frequency.of.drinking` = sum(table1D.Average[-4]))),
                   paste0(sQuote("SumForEachColumn"), " is unable to apply a filter to the input Q Table ",
                          "since the original variable data is unavailable."))
    expect_warning(expect_equal(SumForEachColumn(table1D.Average, weights = runif(10), warn = TRUE),
                                c(`table.Frequency.of.drinking` = sum(table1D.Average[-4]))),
                   paste0(sQuote("SumForEachColumn"), " is unable to apply weights to the input Q Table ",
                          "since the original variable data is unavailable."))
    expect_warning(expect_equal(SumForEachColumn(table1D.Average, subset = rep(c(TRUE, FALSE), c(5, 5)),
                                           weights = runif(10), warn = TRUE),
                                c(`table.Frequency.of.drinking` = sum(table1D.Average[-4]))),
                   paste0(sQuote("SumForEachColumn"), " is unable to apply a filter or weights to the input Q Table ",
                          "since the original variable data is unavailable."))
    input.matrix <- matrix(c(Inf, -Inf, 1, 2), nrow = 2, dimnames = list(NULL, letters[1:2]))
    input.vect <- c(Inf, -Inf)
    expect_warning(SumForEachColumn(input.matrix, input.vect, warn = TRUE),
                   paste0(sQuote("SumForEachColumn"), " cannot compute some values as the data contains both Inf and -Inf."))
    expect_warning(SumForEachColumn(input.matrix[, -2], input.vect, warn = TRUE),
                   paste0(sQuote("SumForEachColumn"), " cannot be computed as the data contains both Inf and -Inf."))
})
