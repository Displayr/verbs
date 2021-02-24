context("AverageRows")

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
    expect_error(AverageRows(variable.Text),
                 paste0("Text data has been supplied but ", sQuote("AverageRows"), " requires numeric data."))
    expect_error(AverageRows(variable.Date),
                 paste0("Date/Time data has been supplied but ", sQuote("AverageRows"), " requires numeric data."))
    numeric.var.expected <- as.vector(variable.Numeric)
    numeric.var.expected.wo.missing <- numeric.var.expected
    numeric.var.expected.wo.missing[is.na(numeric.var.expected.wo.missing)] <- NaN
    expect_equal(AverageRows(variable.Numeric, remove.missing = FALSE), numeric.var.expected)
    expect_equal(AverageRows(variable.Numeric, remove.missing = TRUE), numeric.var.expected.wo.missing)
    nominal.var.expected <- as.vector(flipTransformations::AsNumeric(variable.Nominal, binary = FALSE))
    nominal.var.expected.wo.missing <- nominal.var.expected
    nominal.var.expected.wo.missing[is.na(nominal.var.expected.wo.missing)] <- NaN
    expect_equal(AverageRows(variable.Nominal, remove.missing = FALSE), nominal.var.expected)
    expect_equal(AverageRows(variable.Nominal, remove.missing = TRUE), nominal.var.expected.wo.missing)
    binary.var.expected <- as.vector(variable.Binary)
    binary.var.expected.wo.missing <- binary.var.expected
    binary.var.expected.wo.missing[is.na(binary.var.expected.wo.missing)] <- NaN
    expect_equal(AverageRows(variable.Binary, remove.missing = FALSE), binary.var.expected)
    expect_equal(AverageRows(variable.Binary, remove.missing = TRUE), binary.var.expected.wo.missing)
    expect_error(AverageRows(variable.Numeric, variable.Date),
                 paste0("Date/Time data has been supplied but ", sQuote("AverageRows"), " requires numeric data."))
    expect_error(AverageRows(variable.Binary, variable.Text),
                 paste0("Text data has been supplied but ", sQuote("AverageRows"), " requires numeric data."))
    nominal.to.numeric <- flipTransformations::AsNumeric(variable.Nominal,
                                                         binary = FALSE)
    vars.as.matrix <- matrix(c(variable.Binary,
                               variable.Numeric,
                               nominal.to.numeric),
                             ncol = 3,
                             dimnames = list(NULL, c("Coca-Cola", "Age", "Age to Numeric")))
    expected.2.sum.rows.missing.removed <- array(rowSums(vars.as.matrix[, -3], na.rm = TRUE),
                                                 dim = c(length(variable.Binary), 1L),
                                                 dimnames = list(NULL, "Coca-Cola + Age"))
    n.sum <- apply(!is.na(vars.as.matrix[, -3]), 1L, sum)
    expected.2.sum.rows.missing.kept <- array(rowSums(vars.as.matrix[, -3]),
                                              dim = c(length(variable.Binary), 1L),
                                              dimnames = list(NULL, "Coca-Cola + Age"))
    expect_equal(AverageRows(variable.Binary, variable.Numeric,
                             remove.missing = TRUE),
                 expected.2.sum.rows.missing.removed / n.sum)
    expect_equal(AverageRows(variable.Binary, variable.Numeric,
                         remove.missing = FALSE),
                 expected.2.sum.rows.missing.kept / n.sum)
    expected.3.sum.rows.missing.removed <- array(rowSums(vars.as.matrix, na.rm = TRUE),
                                                 dim = c(length(variable.Binary), 1L),
                                                 dimnames = list(NULL, "Coca-Cola + Age + Age to Numeric"))
    expected.3.sum.rows.missing.kept <- array(rowSums(vars.as.matrix),
                                              dim = c(length(variable.Binary), 1L),
                                              dimnames = list(NULL, "Coca-Cola + Age + Age to Numeric"))
    attr(variable.Nominal, "label") <- "Age to Numeric"
    n.sum <- apply(!is.na(vars.as.matrix), 1L, sum)
    expect_equal(AverageRows(variable.Binary, variable.Numeric, variable.Nominal,
                         remove.missing = TRUE),
                 expected.3.sum.rows.missing.removed / n.sum)
    expect_equal(AverageRows(variable.Binary, variable.Numeric, variable.Nominal,
                         remove.missing = FALSE),
                 expected.3.sum.rows.missing.kept / n.sum)
    # Warnings for factors
    ## No extra warning for variables that are converted using value attributes
    captured.warnings <- capture_warnings(AverageRows(variable.Binary,
                                                  variable.Nominal,
                                                  warn = TRUE))
    expect_length(captured.warnings, 1L)
    expect_equal(captured.warnings, "Missing values have been ignored in calculation.")
    ## AsNumeric warning should be appearing when factor converted that has no value attributes
    expect_warning(AverageRows(1:5, factor(1:5), warn = TRUE),
                   paste0("Data has been automatically converted to numeric. ",
                          "Values are assigned according to the labels of the ",
                          "categories. To use alternative numeric values, ",
                          "transform the data prior including it in this ",
                          "analysis (e.g. by changing its structure)."),
                   fixed = TRUE)
})

load("table1D.Average.rda")
load("table1D.Percentage.rda")
load("table.1D.MultipleStatistics.rda")

test_that("Table 1D", {
    expect_equivalent(AverageRows(table1D.Percentage), table1D.Percentage)
    expect_equal(AverageRows(table.1D.MultipleStatistics), rowMeans(table.1D.MultipleStatistics))
    expected.output <- rowMeans(table.1D.MultipleStatistics[, -which(colnames(table.1D.MultipleStatistics) == "z-Statistic")])
    expect_equal(AverageRows(table.1D.MultipleStatistics,
                         remove.columns = "z-Statistic"),
                 expected.output)
    captured.warnings <- capture_warnings(AverageRows(table.1D.MultipleStatistics,
                                                  remove.columns = "z-Statistic",
                                                  warn = TRUE))
    expect_setequal(captured.warnings,
                    paste0("The input data contains statistics of different types ",
                           "(i.e., Average, Effective Sample Size, t-Statistic, d.f., ",
                           "Corrected p), it may not be appropriate to compute ", sQuote("AverageRows"), "."))
})

load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")
test_that("Table 2D", {
    expected.2d.row.sums <- rowMeans(table2D.Percentage[, -10])
    expect_equal(AverageRows(table2D.Percentage), expected.2d.row.sums)
    expected.2d.row.sums <- rowMeans(table2D.PercentageNaN[, -10], na.rm = TRUE)
    expect_equal(AverageRows(table2D.PercentageNaN), expected.2d.row.sums)
    summary.stat.cols <- colnames(table2D.PercentageAndCount) == "NET"
    row.summed.2d.table.multi.stats <- table2D.PercentageAndCount[, summary.stat.cols, ]
    expect_equal(AverageRows(table2D.PercentageAndCount),
                 row.summed.2d.table.multi.stats / sum(!summary.stat.cols))
    # Warning about missing values
    expect_warning(AverageRows(table2D.PercentageNaN, warn = TRUE),
                   "Missing values have been ignored in calculation.")
    # Missing values
    expect_true(anyNA(AverageRows(table2D.PercentageNaN, remove.missing = FALSE)))
    expect_false(anyNA(AverageRows(table2D.PercentageNaN)))
    # Test subsetted 2D QTable with mulitple statistics to a single statistic
    ## i.e. the case when the dims are a 3d array with (n, p, 1)
    subsetted.qtable <- table2D.PercentageAndCount[, , 1, drop = FALSE]
    subsetted.qtable <- CopyAttributes(subsetted.qtable, table2D.PercentageAndCount)
    expect_equal(AverageRows(subsetted.qtable),
                 rowMeans(table2D.PercentageAndCount[, -10, 1], na.rm = TRUE))
    # Check opposite infinities
    table.opp.inf <- table.1D.MultipleStatistics
    table.opp.inf[1, 1] <- -Inf
    expected.out <- rowMeans(table.opp.inf)
    captured.warnings <- capture_warnings(expect_equal(AverageRows(table.opp.inf, warn = TRUE),
                                                       expected.out))
    expect_setequal(captured.warnings,
                    c(paste0("The input data contains statistics of different types ",
                             "(i.e., Average, Effective Sample Size, t-Statistic, ",
                             "d.f., z-Statistic, Corrected p), it may not be ",
                             "appropriate to compute ", sQuote('AverageRows'), "."),
                      paste0(sQuote("AverageRows"),
                             " cannot compute some values as the data contains both Inf and -Inf.")))
    table.opp.inf[, 1] <- Inf * c(-1, 1, 1, 1)
    table.opp.inf[3:4, 2] <- -Inf
    expected.out <- rowSums(table.opp.inf)
    captured.warnings <- capture_warnings(expect_equal(AverageRows(table.opp.inf, warn = TRUE),
                                                       expected.out))
    expect_setequal(captured.warnings,
                    c(paste0("The input data contains statistics of different types ",
                             "(i.e., Average, Effective Sample Size, t-Statistic, ",
                             "d.f., z-Statistic, Corrected p), it may not be ",
                             "appropriate to compute ", sQuote('AverageRows'), "."),
                      paste0(sQuote("AverageRows"), " cannot be computed as the data contains both Inf and -Inf.")))

})

test_that("Higher dim Q tables", {
    load("numeric.grid.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.with.multiple.stats.qtable
    expect_equal(AverageRows(curr.table),
                 apply(curr.table[, -which(dimnames(curr.table)[[2L]] == "SUM"), ], c(1, 3), mean, na.rm = TRUE))
    load("numeric.grid.nominal.qtable.rda")
    curr.table <- numeric.grid.nominal.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    flat.col.names <- dimnames(flattened.table)[[2L]]
    expect_equal(AverageRows(curr.table),
                 rowMeans(flattened.table[, flat.col.names != "SUM"],
                         na.rm = TRUE))
    load("numeric.grid.nominal.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.nominal.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    expect_equal(AverageRows(curr.table),
                 apply(flattened.table, c(1, 3), mean,
                       na.rm = TRUE))
    load("nominal.multi.nominal.qtable.rda")
    curr.table <- nominal.multi.nominal.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    expect_equal(AverageRows(curr.table),
                 rowMeans(flattened.table, na.rm = TRUE))
    load("nominal.multi.nominal.with.multiple.stats.qtable.rda")
    curr.table <- nominal.multi.nominal.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    expect_equal(AverageRows(curr.table),
                 apply(flattened.table, c(1, 3), mean, na.rm = TRUE))
    load("nominal.multi.nominal.multi.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    expect_equal(AverageRows(curr.table),
                 rowMeans(flattened.table, na.rm = TRUE))
    load("nominal.multi.nominal.multi.with.multiple.stats.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.with.multiple.stats.qtable
    flattened.table <- flattenQTableKeepingMultipleStatistics(curr.table)
    expect_equal(AverageRows(curr.table),
                 apply(flattened.table, c(1, 3), mean, na.rm = TRUE))
})

test_that("Q Tables: Check warning of different statistics thrown or suppressed", {
    # Matching statistics (No warnings)
    # warning already suppressed by default
    expected.out <- rowMeans(table.1D.MultipleStatistics)
    # Don't warn when default warn = FALSE
    expect_equal(AverageRows(table.1D.MultipleStatistics), expected.out)
    warn.msg <- paste0("The input data contains statistics of different types ",
                       "(i.e., Average, Effective Sample Size, t-Statistic, d.f., ",
                       "z-Statistic, Corrected p), it may not be appropriate to ",
                       "compute ", sQuote('AverageRows'), ".")
    captured.warnings <- capture_warnings(expect_equal(AverageRows(table.1D.MultipleStatistics, warn = TRUE),
                                                       expected.out))
    expect_setequal(captured.warnings, warn.msg)
    # No warning even if warn = TRUE when only a single statistic
    expect_equivalent(AverageRows(table1D.Average),
                      table1D.Average)
    expect_equivalent(AverageRows(table1D.Average, warn = TRUE),
                      table1D.Average)
    expected.out <- rowMeans(table2D.Percentage)
    expect_equal(AverageRows(table2D.Percentage, remove.columns = NULL),
                 expected.out)
})

test_that("A single R Output (e.g. a vanilla matrix or vector) selected", {
    ## tries to calls sum() and returns scalar
    matrix.1 <- matrix(1:24, nrow = 6)
    expect_equal(AverageRows(matrix.1), rowMeans(matrix.1))
    vector.1 <-1:24
    expect_equal(AverageRows(vector.1), vector.1)
    # Don't support higher arrays
    array.1 <- array(1:504, dim = 7:9)
    expect_error(AverageRows(array.1),
                 paste0(sQuote("AverageRows"), " only supports inputs that have 1 or 2 dimensions. ",
                        "A supplied input has 3 dimensions. ",
                        "Contact support at ", contact.msg))
})

test_that("Inappropriate multiple inputs", {
    expect_error(AverageRows(c(1:3), c(1:4)),
                 paste0(sQuote("AverageRows"), " requires all input elements to have the same ",
                        "number of rows. In this case there are input elements with 3 and ",
                        "4 rows. Please ensure that all inputs have the same number of rows ",
                        "before attempting to call ", sQuote("AverageRows"), " again."),
                 fixed = TRUE)
    expect_error(AverageRows(c(1:3), c(1:4), c(1:10)),
                 paste0(sQuote("AverageRows"), " requires all input elements to have the same ",
                        "number of rows. In this case there are input elements with 3, ",
                        "4 and 10 rows. Please ensure that all inputs have the same number of rows ",
                        "before attempting to call ", sQuote("AverageRows"), " again."),
                 fixed = TRUE)
    expect_error(AverageRows(c(1:4), array(1:16, dim = c(4, 2, 2))),
                 paste0(sQuote("AverageRows"), " only supports inputs that have 1 or 2 dimensions. ",
                        "A supplied input has 3 dimensions. Contact support at ", contact.msg),
                 fixed = TRUE)
    expect_error(AverageRows(c(1:4), list("hello")),
                 paste0(sQuote("AverageRows"), " requires all input elements to be numeric vectors ",
                        "or reducible to individual numeric vectors such as a numeric matrix or data frame ",
                        "containing numeric elements. One of the provided input elements is a list"),
                 fixed = TRUE)
    list.input <- list("Hello")
    expect_error(AverageRows(c(1:4), list.input),
                 paste0(sQuote("AverageRows"), " requires all input elements to be numeric vectors ",
                        "or reducible to individual numeric vectors such as a numeric matrix or data frame ",
                        "containing numeric elements. One of the provided input elements is a list"),
                 fixed = TRUE)
    table.name <- attr(table.1D.MultipleStatistics, "name")
    expect_error(AverageRows(1:nrow(table.1D.MultipleStatistics), table.1D.MultipleStatistics),
                 paste0(sQuote("AverageRows"), " doesn't support Tables when more than one input is provided. ",
                        "Either remove the input ", table.name, " and any other Tables from the ",
                        "input or call ", sQuote("AverageRows"), " again with only ", table.name, " ",
                        "as the input."),
                 fixed = TRUE)
})

test_that("Multiple inputs", {
    expected.output <- as.array(rowMeans(cbind(1:4, matrix(1:12, nrow = 4))))
    expected.with.names <- expected.output
    names(expected.with.names) <- LETTERS[1:4]
    expect_equal(AverageRows(c(1:4), matrix(1:12, nrow = 4)), expected.output)
    expect_equal(AverageRows(c(A = 1, B = 2, C = 3, D = 4),
                         matrix(1:12, nrow = 4, dimnames = list(LETTERS[1:4], NULL))),
                 expected.with.names)
    vect <- c(A = 1, B = 2, C = 3, D = 4)
    mat  <- matrix(1:12, nrow = 4, dimnames = list(LETTERS[1:4], NULL))
    expect_equal(AverageRows(vect, mat), expected.with.names)
    dimnames(mat)[[1L]] <- letters[1:4]
    expect_equal(AverageRows(vect, mat), expected.with.names)
    vect <- matrix(vect, ncol = 1, dimnames = list(names(vect), NULL))
    expect_equal(AverageRows(vect, mat), expected.with.names)
})
