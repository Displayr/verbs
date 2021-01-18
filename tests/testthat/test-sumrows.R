context("SumRows")

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
    expect_error(SumRows(variable.Text),
                 paste0("Text data has been supplied but ", sQuote("SumRows"), " requires numeric data."))
    expect_error(SumRows(variable.Date),
                 paste0("Date/Time data has been supplied but ", sQuote("SumRows"), " requires numeric data."))
    numeric.var.expected <- as.vector(variable.Numeric)
    numeric.var.expected.wo.missing <- numeric.var.expected
    numeric.var.expected.wo.missing[is.na(numeric.var.expected.wo.missing)] <- 0
    expect_equal(SumRows(variable.Numeric, remove.missing = FALSE), numeric.var.expected)
    expect_equal(SumRows(variable.Numeric, remove.missing = TRUE), numeric.var.expected.wo.missing)
    nominal.var.expected <- as.vector(flipTransformations::AsNumeric(variable.Nominal, binary = FALSE))
    nominal.var.expected.wo.missing <- nominal.var.expected
    nominal.var.expected.wo.missing[is.na(nominal.var.expected.wo.missing)] <- 0
    expect_equal(SumRows(variable.Nominal, remove.missing = FALSE), nominal.var.expected)
    expect_equal(SumRows(variable.Nominal, remove.missing = TRUE), nominal.var.expected.wo.missing)
    binary.var.expected <- as.vector(variable.Binary)
    binary.var.expected.wo.missing <- binary.var.expected
    binary.var.expected.wo.missing[is.na(binary.var.expected.wo.missing)] <- 0
    expect_equal(SumRows(variable.Binary, remove.missing = FALSE), binary.var.expected)
    expect_equal(SumRows(variable.Binary, remove.missing = TRUE), binary.var.expected.wo.missing)
    expect_error(SumRows(variable.Numeric, variable.Date),
                 paste0("Date/Time data has been supplied but ", sQuote("SumRows"), " requires numeric data."))
    expect_error(SumRows(variable.Binary, variable.Text),
                 paste0("Text data has been supplied but ", sQuote("SumRows"), " requires numeric data."))
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
    expected.2.sum.rows.missing.kept <- array(rowSums(vars.as.matrix[, -3]),
                                              dim = c(length(variable.Binary), 1L),
                                              dimnames = list(NULL, "Coca-Cola + Age"))
    expect_equal(SumRows(variable.Binary, variable.Numeric,
                         remove.missing = TRUE),
                 expected.2.sum.rows.missing.removed)
    expect_equal(SumRows(variable.Binary, variable.Numeric,
                         remove.missing = FALSE),
                 expected.2.sum.rows.missing.kept)
    expected.3.sum.rows.missing.removed <- array(rowSums(vars.as.matrix, na.rm = TRUE),
                                                 dim = c(length(variable.Binary), 1L),
                                                 dimnames = list(NULL, "Coca-Cola + Age + Age to Numeric"))
    expected.3.sum.rows.missing.kept <- array(rowSums(vars.as.matrix),
                                              dim = c(length(variable.Binary), 1L),
                                              dimnames = list(NULL, "Coca-Cola + Age + Age to Numeric"))
    attr(variable.Nominal, "label") <- "Age to Numeric"
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

load("table1D.Average.rda")
load("table1D.Percentage.rda")
load("table.1D.MultipleStatistics.rda")

test_that("Table 1D", {
    expect_equivalent(SumRows(table1D.Percentage), table1D.Percentage)
    expected.1d.table.row.sums <- array(rowSums(table.1D.MultipleStatistics),
                                        dim = c(nrow(table.1D.MultipleStatistics), 1L),
                                        dimnames = list(row.names(table.1D.MultipleStatistics),
                                                        paste0(colnames(table.1D.MultipleStatistics),
                                                               collapse = " + ")))
    expect_equal(SumRows(table.1D.MultipleStatistics), expected.1d.table.row.sums)
    input.dat <- rowSums(table.1D.MultipleStatistics[, -which(colnames(table.1D.MultipleStatistics) == "z-Statistic")])
    expected.colnames <- sub(" + z-Statistic", "", colnames(expected.1d.table.row.sums), fixed = TRUE)
    expected.1d.table.row.sums <- array(input.dat,
                                        dim = c(nrow(table.1D.MultipleStatistics), 1L),
                                        dimnames = list(row.names(table.1D.MultipleStatistics),
                                                        expected.colnames))
    expect_equal(SumRows(table.1D.MultipleStatistics,
                         remove.columns = "z-Statistic"),
                 expected.1d.table.row.sums)
    captured.warnings <- capture_warnings(SumRows(table.1D.MultipleStatistics,
                                                  remove.columns = "z-Statistic",
                                                  warn = TRUE))
    expect_setequal(captured.warnings,
                    paste0("The input data contains statistics of different types ",
                           "(i.e., Average, Effective Sample Size, t-Statistic, d.f., ",
                           "Corrected p), it may not be appropriate to compute ", sQuote("SumRows"), "."))
})

load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")
test_that("Table 2D", {
    expected.2d.row.sums <- rowSums(table2D.Percentage[, -10])
    expected.colnames <- paste0(colnames(table2D.Percentage[, -10]),
                                collapse = " + ")
    expected.2d.row.sums <- array(expected.2d.row.sums,
                                  dim = c(nrow(table2D.Percentage), 1L),
                                  dimnames = list(row.names(table2D.Percentage),
                                                  expected.colnames))
    expect_equal(SumRows(table2D.Percentage), expected.2d.row.sums)
    expected.2d.row.sums <- rowSums(table2D.PercentageNaN[, -10], na.rm = TRUE)
    expected.colnames <- paste0(colnames(table2D.PercentageNaN[, -10]),
                                collapse = " + ")
    expected.2d.row.sums <- array(expected.2d.row.sums,
                                  dim = c(nrow(table2D.PercentageNaN), 1L),
                                  dimnames = list(row.names(table2D.PercentageNaN),
                                                  expected.colnames))
    expect_equal(SumRows(table2D.PercentageNaN), expected.2d.row.sums)
    summary.stat.cols <- colnames(table2D.PercentageAndCount) == "NET"
    row.summed.2d.table.multi.stats <- table2D.PercentageAndCount[, summary.stat.cols, ]
    expect_equal(SumRows(table2D.PercentageAndCount),
                 row.summed.2d.table.multi.stats)
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
    expected.out <- array(rowSums(table.opp.inf),
                          dim = c(nrow(table.opp.inf), 1L),
                          dimnames = list(row.names(table.1D.MultipleStatistics),
                                          paste0(colnames(table.1D.MultipleStatistics),
                                                 collapse = " + ")))
    captured.warnings <- capture_warnings(expect_equal(SumRows(table.opp.inf, warn = TRUE),
                                                       expected.out))
    expect_setequal(captured.warnings,
                    c(paste0("The input data contains statistics of different types ",
                             "(i.e., Average, Effective Sample Size, t-Statistic, ",
                             "d.f., z-Statistic, Corrected p), it may not be ",
                             "appropriate to compute ", sQuote('SumRows'), "."),
                      paste0(sQuote("SumRows"),
                             " cannot compute some values as the data contains both Inf and -Inf.")))
    table.opp.inf[, 1] <- Inf * c(-1, 1, 1, 1)
    table.opp.inf[3:4, 2] <- -Inf
    expected.out <- array(rowSums(table.opp.inf),
                          dim = c(nrow(table.opp.inf), 1L),
                          dimnames = list(row.names(table.1D.MultipleStatistics),
                                          paste0(colnames(table.1D.MultipleStatistics),
                                                 collapse = " + ")))
    captured.warnings <- capture_warnings(expect_equal(SumRows(table.opp.inf, warn = TRUE),
                                                       expected.out))
    expect_setequal(captured.warnings,
                    c(paste0("The input data contains statistics of different types ",
                             "(i.e., Average, Effective Sample Size, t-Statistic, ",
                             "d.f., z-Statistic, Corrected p), it may not be ",
                             "appropriate to compute ", sQuote('SumRows'), "."),
                      paste0(sQuote("SumRows"), " cannot be computed as the data contains both Inf and -Inf.")))

})

test_that("Higher dim Q tables", {
    load("numeric.grid.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.with.multiple.stats.qtable
    expect_equal(SumRows(curr.table),
                 apply(curr.table[, -which(dimnames(curr.table)[[2L]] == "SUM"), ], c(1, 3), sum, na.rm = TRUE))
    load("numeric.grid.nominal.qtable.rda")
    curr.table <- numeric.grid.nominal.qtable
    expect_equal(SumRows(curr.table),
                 apply(curr.table[, , -dim(curr.table)[[3L]]],
                       1:2, sum, na.rm = TRUE))
    load("numeric.grid.nominal.with.multiple.stats.qtable.rda")
    curr.table <- numeric.grid.nominal.with.multiple.stats.qtable
    expect_equal(SumRows(curr.table),
                 apply(curr.table[, , -dim(curr.table)[3], ], c(1:2, 4L), sum, na.rm = TRUE))
    load("nominal.multi.nominal.qtable.rda")
    curr.table <- nominal.multi.nominal.qtable
    expect_equal(SumRows(curr.table),
                 apply(curr.table[, , -dim(curr.table)[[3L]]],
                       1:2, sum, na.rm = TRUE))
    load("nominal.multi.nominal.with.multiple.stats.qtable.rda")
    curr.table <- nominal.multi.nominal.with.multiple.stats.qtable
    expect_equal(SumRows(curr.table),
                 apply(curr.table[, , -dim(curr.table)[3], ],
                       c(1:2, 4L), sum, na.rm = TRUE))
    load("nominal.multi.nominal.multi.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.qtable
    expect_equal(SumRows(curr.table),
                 apply(curr.table[, , , -dim(curr.table)[4]],
                       1:3, sum, na.rm = TRUE))
    load("nominal.multi.nominal.multi.with.multiple.stats.qtable.rda")
    curr.table <- nominal.multi.nominal.multi.with.multiple.stats.qtable
    expect_equal(SumRows(curr.table),
                 apply(curr.table[, , , -dim(curr.table)[4], ],
                       c(1:3, 5), sum, na.rm = TRUE))
})

test_that("Q Tables: Check warning of different statistics thrown or suppressed", {
    # Matching statistics (No warnings)
    # warning already suppressed by default
    expected.out <- array(rowSums(table.1D.MultipleStatistics),
                          dim = c(nrow(table.1D.MultipleStatistics), 1L),
                          dimnames = list(rownames(table.1D.MultipleStatistics),
                                          paste0(colnames(table.1D.MultipleStatistics),
                                                 collapse = " + ")))
    # Don't warn when default warn = FALSE
    expect_equal(SumRows(table.1D.MultipleStatistics), expected.out)
    warn.msg <- paste0("The input data contains statistics of different types ",
                       "(i.e., Average, Effective Sample Size, t-Statistic, d.f., ",
                       "z-Statistic, Corrected p), it may not be appropriate to ",
                       "compute ", sQuote('SumRows'), ".")
    captured.warnings <- capture_warnings(expect_equal(SumRows(table.1D.MultipleStatistics, warn = TRUE),
                                                       expected.out))
    expect_setequal(captured.warnings, warn.msg)
    # No warning even if warn = TRUE when only a single statistic
    expect_equivalent(SumRows(table1D.Average),
                      table1D.Average)
    expect_equivalent(SumRows(table1D.Average, warn = TRUE),
                      table1D.Average)
    expected.out <- array(rowSums(table2D.Percentage),
                          dim = c(nrow(table2D.Percentage), 1L),
                          dimnames = list(rownames(table2D.Percentage),
                                          paste0(colnames(table2D.Percentage),
                                                 collapse = " + ")))
    expect_equal(SumRows(table2D.Percentage, remove.columns = NULL),
                 expected.out)
})

test_that("A single R Output (e.g. a vanilla matrix or vector) selected", {
    ## tries to calls sum() and returns scalar
    matrix.1 <- matrix(1:24, nrow = 6)
    expect_equal(SumRows(matrix.1), rowSums(matrix.1))
    vector.1 <-1:24
    expect_equal(SumRows(vector.1), vector.1)
    # Don't support higher arrays
    array.1 <- array(1:504, dim = 7:9)
    expect_error(SumRows(array.1),
                 paste0(sQuote("SumRows"), " only supports inputs that have 1 or 2 dimensions. ",
                        "A supplied input has 3 dimensions. ",
                        "Contact support at opensource@displayr.com or raise an issue ",
                        "at https://github.com/Displayr/verbs if you wish this to be changed."))
})

test_that("Coercing matrices and dataframes to vectors", {
    v <- table1D.Average
    p <- 5
    n <- length(v)
    m <- array(rep(table1D.Average, p), dim = c(n, p), dimnames = list(names(table1D.Average)))
    df <- as.data.frame(m)
    input <- list(v, m, v, df, v)
    single.out <- array(table1D.Average, dim = n, dimnames = list(names(table1D.Average)))
    split.matrix <- split(m, col(m))
    split.matrix[[1L]] <- setRowNames(split.matrix[[1L]], names.to.use = names(table1D.Average))
    names(split.matrix) <- NULL
    split.df <- as.list(df)
    split.df[[1L]] <- setRowNames(split.df[[1L]], names.to.use = names(table1D.Average))
    output <- c(list(v), split.matrix, list(v), split.df, list(v))
    expect_equal(splitIntoOneDimensionalVariables(input), output)
})

test_that("Multiple inputs", {
    input1 <- matrix(1:12, nrow = 6, dimnames = list(letters[1:6], c("Q1", "Q2")))
    input2 <- structure(1:6, label = "foo")
    input3 <- structure(7:12, name = "bar")
    expected.out <- rowSums(do.call(cbind, args = list(input1, input2, input3)))
    expected.out <- array(expected.out, dim = c(6L, 1L),
                          dimnames = list(letters[1:6], paste0(c("Q1", "Q2", "foo", "bar"),
                                                               collapse = " + ")))
    expect_equal(SumRows(input1, input2, input3), expected.out)
    expect_equal(SumRows(as.data.frame(input1), input2, input3), expected.out)
})

test_that("Inappropriate multiple inputs", {
    expect_error(SumRows(c(1:3), c(1:4)),
                 paste0(sQuote("SumRows"), " requires all input elements to have the same ",
                        "number of rows. In this case there are input elements with 3 and ",
                        "4 rows. Please ensure that all inputs have the same number of rows ",
                        "before attempting to call ", sQuote("SumRows"), " again."),
                 fixed = TRUE)
    expect_error(SumRows(c(1:3), c(1:4), c(1:10)),
                 paste0(sQuote("SumRows"), " requires all input elements to have the same ",
                        "number of rows. In this case there are input elements with 3, ",
                        "4 and 10 rows. Please ensure that all inputs have the same number of rows ",
                        "before attempting to call ", sQuote("SumRows"), " again."),
                 fixed = TRUE)
    expect_error(SumRows(c(1:4), matrix(1:12, nrow = 4)), NA)
    expect_error(SumRows(c(1:4), array(1:16, dim = c(4, 2, 2))),
                 paste0(sQuote("SumRows"), " only supports inputs that have 1 or 2 dimensions. ",
                        "A supplied input has 3 dimensions. Contact support at ", contact.msg),
                 fixed = TRUE)
    expect_error(SumRows(c(1:4), list("hello")),
                 paste0(sQuote("SumRows"), " requires all input elements to be numeric vectors or a matrix"),
                 fixed = TRUE)
})

test_that("Get appropriate names from inputs", {
    x <- matrix(1:6, nrow = 3)
    attr.to.use <- c("symbol", "question", "name", "label", "dimnames")
    attr.to.set <- list("A", "B", "C", "D", c("foo", "bar"))
    attr(x, "symbol") <- "A"
    expect_equal(getColumnNames(x), "A")
    attr(x, "question") <- "Q"
    expect_equal(getColumnNames(x), "Q")
    attr(x, "name") <- "B"
    expect_equal(getColumnNames(x), "B")
    attr(x, "label") <- "D"
    expect_equal(getColumnNames(x), "D")
    dimnames(x) <- list(NULL, c("foo", "bar"))
    expect_equal(getColumnNames(x), c("foo", "bar"))
})
