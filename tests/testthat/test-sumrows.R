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
    expect_error(SumRows(variable.Date),
                 "Date/Time data has been supplied but 'SumRows' requires numeric data.")
    expect_equal(SumRows(variable.Numeric), as.vector(variable.Numeric))
    expect_equal(SumRows(variable.Nominal), as.vector(variable.Numeric))
    expect_equal(SumRows(variable.Binary), as.vector(variable.Binary))
    expect_error(SumRows(variable.Numeric, variable.Date),
                 "Date/Time data has been supplied but 'SumRows' requires numeric data.")
    expect_error(SumRows(variable.Binary, variable.Text),
                 "Text data has been supplied but 'SumRows' requires numeric data.")
    vars.as.matrix <- matrix(c(variable.Binary,
                               variable.Numeric,
                               flipTransformations::AsNumeric(variable.Nominal,
                                                              binary = FALSE)),
                             ncol = 3)
    expected.2.sum.rows.missing.removed <- rowSums(vars.as.matrix[, -3], na.rm = TRUE)
    expected.2.sum.rows.missing.kept <- rowSums(vars.as.matrix[, -3])
    expect_equal(SumRows(variable.Binary, variable.Numeric,
                         remove.missing = TRUE),
                 expected.2.sum.rows.missing.removed)
    expect_equal(SumRows(variable.Binary, variable.Numeric,
                         remove.missing = FALSE),
                 expected.2.sum.rows.missing.kept)
    expected.3.sum.rows.missing.removed <- rowSums(vars.as.matrix, na.rm = TRUE)
    expected.3.sum.rows.missing.kept <- rowSums(vars.as.matrix)
    expect_equal(SumRows(variable.Binary, variable.Numeric, variable.Nominal,
                         remove.missing = TRUE),
                 expected.3.sum.rows.missing.removed)
    expect_equal(SumRows(variable.Binary, variable.Numeric, variable.Nominal,
                         remove.missing = FALSE),
                 expected.3.sum.rows.missing.kept)
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
    expect_equal(SumRows(table1D.Percentage, remove.rows = NULL),
                      table.without.most.attr)
    expect_equal(SumRows(table1D.Percentage, remove.rows = "NET"),
                      table.without.most.attr[names(table.without.most.attr) != "NET"])
    expect_equal(SumRows(table.1D.MultipleStatistics, remove.rows = NULL),
                 c(`Colas (e.g., Coca-Cola, Pepsi Max)?` = Inf, `Sparkling mineral water` = -Inf,
                   Coffee = 670.523888977345, SUM = Inf))
    expect_equal(SumRows(table.1D.MultipleStatistics),
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
    # Test subsetted 2D QTable with mulitple statistics to a single statistic
    ## i.e. the case when the dims are a 3d array with (n, p, 1)
    subsetted.qtable <- table2D.PercentageAndCount[, , 1, drop = FALSE]
    subsetted.qtable <- CopyAttributes(subsetted.qtable, table2D.PercentageAndCount)
    expect_equal(SumRows(subsetted.qtable),
                 rowSums(table2D.PercentageAndCount[, -10, 1], na.rm = TRUE))
    # Check opposite infinities
    table.opp.inf <- table.1D.MultipleStatistics
    table.opp.inf[1, 1] <- -Inf
    captured.warnings <- capture_warnings(expect_equal(SumRows(table.opp.inf, warn = TRUE),
                                                       rowSums(table.opp.inf[-4, ])))
    expect_setequal(captured.warnings,
                    c("These categories have been removed from the rows: SUM.",
                      paste0("The input data contains statistics of different types ",
                             "(i.e., Average, Effective Sample Size, t-Statistic, ",
                             "d.f., z-Statistic, Corrected p), it may not be ",
                             "appropriate to compute 'SumRows'."),
                      "'SumRows' cannot compute some values as the data contains both Inf and -Inf."))
    table.opp.inf[, 1] <- Inf * c(-1, 1, 1, 1)
    table.opp.inf[3, 5] <- -Inf
    captured.warnings <- capture_warnings(expect_equal(SumRows(table.opp.inf, warn = TRUE),
                                                       rowSums(table.opp.inf[-4, ])))
    expect_setequal(captured.warnings,
                    c("These categories have been removed from the rows: SUM.",
                      paste0("The input data contains statistics of different types ",
                             "(i.e., Average, Effective Sample Size, t-Statistic, ",
                             "d.f., z-Statistic, Corrected p), it may not be ",
                             "appropriate to compute 'SumRows'."),
                      "'SumRows' cannot be computed as the data contains both Inf and -Inf."))

})

test_that("Exact matching variables with element names - ignoring unmatched", {
    var1 <- table2D.Percentage[, 1]
    inds <- sample(seq(NROW(var1)))
    while(identical(inds, seq_along(NROW(var1))))
        inds <- sample(inds)
    var2 <- table2D.Percentage[inds, 4]
    var2.correct.order <- var2[names(var1)]
    expect_equal(names(var1), names(var2.correct.order))
    expect_equal(SumRows(var1, var2),
                 rowSums(cbind(var1, var2.correct.order)))
    partial.named.var2 <- var2
    names(partial.named.var2)[1] <- NA
    expect_error(SumRows(var1, partial.named.var2),
                 paste0("'SumRows' requires either a fully named vector or a vector ",
                        "with no names to calculate output. Some elements of the ",
                        "input vector have names while other elements are not named. ",
                        "Please name all elements if you wish to compute 'SumRows' ",
                        "by matching elements. Contact support at opensource@displayr.com ",
                        "or raise an issue at https://github.com/Displayr/verbs if ",
                        "you wish this to be changed."))
    unnamed.var2 <- unname(var2)
    expect_equal(SumRows(var1, unnamed.var2),
                 var1)
    expect_warning(expect_equal(SumRows(var1, unnamed.var2, warn = TRUE),
                                var1),
                   paste0(""))
    expect_equal(expect_warning(SumRows(var1, unnamed.var2, warn = TRUE),
                                "One of the input elements doesn't have any names and cannot be matched."),
                 var1)
})

