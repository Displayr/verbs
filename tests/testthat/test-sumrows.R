context("SumRows")

data(variable.Text)
data(variable.Binary)
data(variable.Nominal)
data(variable.Numeric)
data(variable.Time)
data(variable.Date)
test_that("Variables", {
    expect_error(SumRows(variable.Text),
                 "Text data has been supplied but 'SumRows' requires numeric data.")
    expect_error(SumRows(variable.Numeric, variable.Date),
                 "Date/Time data has been supplied but 'SumRows' requires numeric data.")
    expect_error(SumRows(variable.Binary, variable.Text),
                 "Text data has been supplied but 'SumRows' requires numeric data.")
    vars.as.matrix <- matrix(c(variable.Binary,
                               variable.Numeric,
                               flipTransformations::AsNumeric(variable.Nominal,
                                                              binary = FALSE)),
                             ncol = 3)
    expected.sum.rows.missing.removed <- rowSums(vars.as.matrix, na.rm = TRUE)
    expected.sum.rows.missing.kept <- rowSums(vars.as.matrix)
    expect_equivalent(SumRows(variable.Binary, variable.Numeric, variable.Nominal,
                              remove.missing = TRUE)[TRUE],
                      expected.sum.rows.missing.removed)
    expect_equal(SumRows(variable.Binary, variable.Numeric, variable.Nominal,
                         remove.missing = FALSE)[TRUE],
                 expected.sum.rows.missing.kept)

    # Warnings for factors
    ## No extra warning for variables that are converted using value attributes
    captured.warnings <- capture_warnings(SumRows(variable.Binary,
                                                  variable.Nominal,
                                                  warn = TRUE))
    expect_length(captured.warnings, 1L)
    expect_equal(captured.warnings, "Missing values have been ignored in calculation.")
    ## AsNumeric warning should be appearing when factor converted that has no value attributes
    expect_warning(SumRows(1:5, factor(1:5), warn = TRUE),
                   paste0("Data has been automatically converted to numeric. ",
                          "Values are assigned according to the labels of the ",
                          "categories. To use alternative numeric values, ",
                          "transform the data prior including it in this ",
                          "analysis (e.g. by changing its structure)."),
                   fixed = TRUE)
})


data(table1D.Average)
data(table1D.Percentage)
data(table.1D.MultipleStatistics)

test_that("Table 1D", {
    table.without.most.attr <- as.vector(table1D.Percentage)
    names(table.without.most.attr) <- names(table1D.Percentage)
    expect_equivalent(SumRows(table1D.Percentage, remove.rows = NULL),
                      table.without.most.attr)
    expect_equivalent(SumRows(table1D.Percentage, remove.rows = "NET"),
                      table.without.most.attr[names(table.without.most.attr) != "NET"])
    expect_equivalent(SumRows(table.1D.MultipleStatistics, remove.rows = NULL),
                              c(`Colas (e.g., Coca-Cola, Pepsi Max)?` = Inf, `Sparkling mineral water` = -Inf,
                                Coffee = 670.523888977345, SUM = Inf))
    expect_equivalent(SumRows(table.1D.MultipleStatistics),
                      c(`Colas (e.g., Coca-Cola, Pepsi Max)?` = Inf, `Sparkling mineral water` = -Inf,
                        Coffee = 670.523888977345))
    expect_equal(SumRows(table.1D.MultipleStatistics, remove.rows = NULL,
                         remove.columns = "z-Statistic"),
                 rowSums(table.1D.MultipleStatistics[, colnames(table.1D.MultipleStatistics) != "z-Statistic"]))
    expect_equal(SumRows(table.1D.MultipleStatistics, remove.rows = "SUM",
                         remove.columns = "z-Statistic"),
                 rowSums(table.1D.MultipleStatistics[-4, colnames(table.1D.MultipleStatistics) != "z-Statistic"]))
    captured.warnings <- capture_warnings(SumRows(table.1D.MultipleStatistics,
                                                  remove.rows = "SUM",
                                                  remove.columns = "z-Statistic",
                                                  warn = TRUE))
    expect_setequal(captured.warnings,
                    c(paste0("The input data contains statistics of different types ",
                             "(i.e., Average, Effective Sample Size, t-Statistic, d.f., ",
                             "Corrected p), it may not be appropriate to compute 'SumRows'."),
                      "These categories have been removed from the rows: SUM.",
                      "These categories have been removed from the columns: z-Statistic."))
})

data(table2D.Percentage)
data(table2D.PercentageAndCount)
data(table2D.PercentageNaN)
test_that("Table 2D", {
    expect_equal(SumRows(table2D.Percentage),
                 c(`Coca-Cola` = 100, `Diet Coke` = 100,
                   `Coke Zero` = 100, Pepsi = 100,
                   `Diet Pepsi` = 100, `Pepsi Max` = 100))
    expect_equal(SumRows(table2D.PercentageNaN),
                 rowSums(table2D.PercentageNaN[-8, -10], na.rm = TRUE))
    summary.stat.cols <- colnames(table2D.PercentageAndCount) == "NET"
    row.summed.2d.table.multi.stats <- table2D.PercentageAndCount[, summary.stat.cols, ]
    expect_equal(SumRows(table2D.PercentageAndCount),
                 row.summed.2d.table.multi.stats)
    # Warning for dodgy calculation
    expect_warning(SumRows(table2D.PercentageAndCount, warn = TRUE),
                   paste0("The input data contains statistics of different types ",
                          "(i.e., Row %, Count), it may not be appropriate to compute 'SumRows'."),
                   fixed = TRUE)
    # Extra category removed removed
    expect_equal(SumRows(table2D.PercentageNaN, remove.rows = c("NET", "None of these")),
                 rowSums(table2D.PercentageNaN[-7:-8, -10], na.rm = TRUE))
    # Warning about missing values
    expect_warning(SumRows(table2D.PercentageNaN, warn = TRUE),
                   "Missing values have been ignored in calculation.")
    # Missing values
    expect_true(anyNA(SumRows(table2D.PercentageNaN, remove.missing = FALSE)))
    expect_false(anyNA(SumRows(table2D.PercentageNaN)))
})


test_that("Two Q Tables selected: no stats match", {
    # Show a warning
    ## Two 1D tables with no matching statistics.
    table.1d.multiple.no.average <- CopyAttributes(table.1D.MultipleStatistics[, -c(1, 5), drop = FALSE],
                                                   table.1D.MultipleStatistics)
    captured.warnings <- capture_warnings(SumRows(table.1d.multiple.no.average, table1D.Average,
                                                  warn = TRUE))
    expect_setequal(captured.warnings,
                    c("These categories have been removed from the rows: SUM.",
                      paste0("The input data contains statistics of different types ",
                             "(i.e., Effective Sample Size, t-Statistic, d.f., ",
                             "Corrected p, Average), it may not be appropriate to ",
                             "compute 'SumRows'.")))
    ## Two 2D tables with no matching statistics.
    table.2d.with.only.count <- CopyAttributes(table2D.PercentageAndCount[, , -1, drop = FALSE],
                                               table2D.PercentageAndCount)
    captured.warnings <- capture_warnings(row.output <- SumRows(table.2d.with.only.count,
                                                                table2D.Percentage,
                                                  warn = TRUE))
    expect_setequal(captured.warnings,
                    c("These categories have been removed from the columns: NET.",
                      paste0("The input data contains statistics of different types ",
                             "(i.e., Count, Row %), it may not be appropriate to ",
                             "compute 'SumRows'.")))
    net.col <- which(colnames(table.2d.with.only.count) == "NET")
    expected.output <- rowSums(table.2d.with.only.count[, -net.col, ]) +
                       rowSums(table2D.Percentage[, -net.col])
    names(expected.output) <- rownames(table2D.Percentage)
    expect_equal(row.output, expected.output)
})

test_that("Two Q Tables selected: missing values (exclude by default)", {
    # Check missing values omitted by default
    net.col <- which(colnames(table2D.PercentageNaN) == "NET")
    net.row <- which(rownames(table2D.PercentageNaN) == "NET")
    table.with.na <- table2D.PercentageNaN
    rownames(table.with.na)[1:6] <- rownames(table2D.Percentage)
    expected.output <- rowSums(table2D.PercentageNaN[1:6, -net.col], na.rm = TRUE) +
                       rowSums(table2D.Percentage[, -net.col], na.rm = TRUE)

    expect_equal(output.wo.missing <- SumRows(table.with.na, table2D.Percentage,
                                              remove.rows = c("NET", "None of these")),
                 rowSums(table2D.Percentage[1:6, -net.col], na.rm = TRUE) +
                     rowSums(table2D.PercentageNaN[1:6, -net.col], na.rm = TRUE))
    expect_equal(output.w.missing <- SumRows(table.with.na, table2D.Percentage,
                                             remove.rows = c("NET", "None of these"),
                                             remove.missing = FALSE),
                 rowSums(table2D.Percentage[1:6, -net.col], na.rm = FALSE) +
                     rowSums(table2D.PercentageNaN[1:6, -net.col], na.rm = FALSE))
    expect_true(anyNA(output.w.missing))
    expect_false(anyNA(output.wo.missing))
})

# test_that("Two Q Tables with some unmatched names: error if requested", { # check rownames
#               # check colnames
#
#           })
#
# test_that("Two Q Tables with some unmatched names: warn and continue")
#
# test_that("Two Q Tables with some unmatched names: fuzzy match error")
#
# test_that("Two Q Tables with some unmatched names: fuzzy match warning")
#
# test_that("Two Q Tables with some unmatched names: ignore names requested")
# test_that("Two Q Tables with rows to exclude provided")
#
# test_that("Two Q Tables with columns to exclude provided")
