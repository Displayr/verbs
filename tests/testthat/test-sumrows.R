context("SumRows")

data(variable.Text)
data(variable.Binary)
data(variable.Nominal)
data(variable.Numeric)
data(variable.Time)
data(variable.Date)
test_that("Variables", {
    expect_error(SumRows(variable.Text),
                 paste0("Text data has been supplied but ", sQuote("SumRows"), " requires numeric data."))
    expect_error(SumRows(variable.Date),
                 paste0("Date/Time data has been supplied but ", sQuote("SumRows"), " requires numeric data."))
    expect_equal(SumRows(variable.Numeric), as.vector(variable.Numeric))
    expect_equal(SumRows(variable.Nominal), as.vector(variable.Numeric))
    expect_equal(SumRows(variable.Binary), as.vector(variable.Binary))
    expect_error(SumRows(variable.Numeric, variable.Date),
                 paste0("Date/Time data has been supplied but ", sQuote("SumRows"), " requires numeric data."))
    expect_error(SumRows(variable.Binary, variable.Text),
                 paste0("Text data has been supplied but ", sQuote("SumRows"), " requires numeric data."))
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

test_that("Variables with weights, filters (subset), and a combination of the two", {
    subset.missing.out <- !is.na(variable.Numeric)
    expect_equal(SumRows(variable.Numeric, subset = subset.missing.out),
                 variable.Numeric[!is.na(variable.Numeric)])
    expect_equal(SumRows(variable.Numeric, variable.Nominal, subset = subset.missing.out),
                     rowSums(cbind(variable.Numeric[subset.missing.out],
                                   flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)[subset.missing.out]),
                     na.rm = TRUE))
    expect_error(SumRows(variable.Numeric[1:10], subset = subset.missing.out),
                 paste0("The subset vector has length 327. However, it needs to ",
                        "have length 10 to match the number of cases in the supplied input data."))
    expect_error(SumRows(variable.Numeric, 1:10, subset = subset.missing.out),
                 paste0(sQuote("SumRows"), " requires all input elements to have the same size to be able ",
                        "to apply a filter or weight vector. ",
                        verbs:::determineAppropriateContact()),
                 fixed = TRUE)
    weights <- runif(length(variable.Numeric))
    expect_equal(SumRows(variable.Numeric, weights = weights),
                 as.vector(variable.Numeric * weights))
    expect_equal(SumRows(variable.Numeric, as.numeric(variable.Nominal),
                         weights = weights,
                         subset = subset.missing.out),
                 rowSums(cbind(variable.Numeric * weights,
                               as.numeric(variable.Nominal) * weights)[subset.missing.out, ]))
    expect_error(SumRows(variable.Numeric, weights = weights[1:10]),
                 paste0("The weights vector has length 10. However, it needs to ",
                        "have length 327 to match the number of cases in the supplied input data."))
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
                             "Corrected p), it may not be appropriate to compute ", sQuote("SumRows"), "."),
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
    expect_warning(expect_equal(SumRows(table2D.PercentageAndCount, warn = TRUE),
                                sumWithin3Darray(table2D.PercentageAndCount[, -10, ], rowSums, remove.missing = TRUE)),
                   paste0("The input data contains statistics of different types ",
                          "(i.e., Row %, Count), it may not be appropriate to compute ", sQuote("SumRows"), "."),
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
                      paste0(sQuote("SumRows"),
                             " cannot compute some values as the data contains both Inf and -Inf.")))
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
                      paste0(sQuote("SumRows"), " cannot be computed as the data contains both Inf and -Inf.")))

})

test_that("Q Tables: Check warning of different statistics thrown or suppressed", {
    # Matching statistics (No warnings)
    # warning already suppressed by default
    expected.out <- rowSums(table.1D.MultipleStatistics[-4, ])
    # Don't warn when default warn = FALSE
    expect_equal(SumRows(table.1D.MultipleStatistics), expected.out)
    warn.msg <- paste0("The input data contains statistics of different types ",
                       "(i.e., Average, Effective Sample Size, t-Statistic, d.f., ",
                       "z-Statistic, Corrected p), it may not be appropriate to ",
                       "compute 'SumRows'.")
    captured.warnings <- capture_warnings(expect_equal(SumRows(table.1D.MultipleStatistics, warn = TRUE),
                                                       expected.out))
    expect_setequal(captured.warnings,
                    c(warn.msg, "These categories have been removed from the rows: SUM."))
    # No warning even if warn = TRUE when only a single statistic
    expect_equivalent(SumRows(table1D.Average, remove.rows = NULL),
                      table1D.Average)
    expect_equivalent(SumRows(table1D.Average, remove.rows = NULL, warn = TRUE),
                      table1D.Average)
    expect_equal(SumRows(table2D.Percentage, remove.columns = NULL),
                 rowSums(table2D.Percentage))
    expect_equal(SumRows(table2D.Percentage, remove.columns = NULL, warn = TRUE),
                 rowSums(table2D.Percentage))
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

# Helper function to shuffle second element, useful for the matching tests
shuffleElement <- function(x)
{
    n <- length(x)
    ind <- sample(n)
    while(any(ind == 1:n))
        ind <- sample(n)
    x[ind]
}

test_that("Exact matching variables with element names - ignoring unmatched", {
    var1 <- SumRows(table2D.Percentage)
    var.shuffled <- replicate(3, shuffleElement(var1 * runif(6)), simplify = FALSE)
    # The first input element decides the order
    correct.order <- lapply(var.shuffled, function(x) x[names(var.shuffled[[1L]])])
    correct.binded <- do.call(cbind, correct.order)
    expected.out <- rowSums(correct.binded)
    expect_equal(SumRows(var.shuffled[[1L]], var.shuffled[[2L]], var.shuffled[[3L]]),
                 expected.out)
    # Correct removal of element
    elem <- "Pepsi Max"
    expect_equal(SumRows(var.shuffled[[1L]], var.shuffled[[2L]], var.shuffled[[3L]],
                         remove.rows = "Pepsi Max"),
                 expected.out[names(expected.out) != elem])
    # Add extra element
    not.same.size <- var.shuffled
    all.coke <- sum(not.same.size[[2L]])
    not.same.size[[2L]] <- c(not.same.size[[2L]], "All Coke" = all.coke)
    expect_equal(SumRows(not.same.size[[1L]], not.same.size[[2L]], not.same.size[[3L]]),
                 c(expected.out, "All Coke" = all.coke))
    # Don't allow partially named vectors
    partial.named.var <- var1
    names(partial.named.var)[1] <- NA
    expect_error(SumRows(var1, partial.named.var),
                 paste0(sQuote("SumRows"), " requires either a fully named vector or a vector ",
                        "with no names to calculate output. Some elements of the ",
                        "input vector have names while other elements are not named. ",
                        "Please name all elements if you wish to compute 'SumRows' ",
                        "by matching elements. Contact support at opensource@displayr.com ",
                        "or raise an issue at https://github.com/Displayr/verbs if ",
                        "you wish this to be changed."))
    ## Warn if necessary if a fully unnamed vector is used during the matching process
    unnamed.var1 <- unname(var1)
    expect_equal(SumRows(var1, unnamed.var1),
                 var1)
    expect_warning(expect_equal(SumRows(var1, unnamed.var1, warn = TRUE),
                                var1),
                   paste0("One of the input elements doesn't have any names and cannot be matched. ",
                          "Consider changing the name matching options or ensure all the names ",
                          "match before recomputing."))
})

test_that("Fuzzy matching variables", {
    var1 <- SumRows(table2D.Percentage)
    names(var1) <- gsub("\\s|-", "", names(var1))
    shuffled.vars <- replicate(3, shuffleElement(var1 * runif(6)), simplify = FALSE)
    correct.order <- lapply(shuffled.vars, function(x) x[names(shuffled.vars[[1L]])])
    correct.binded <- do.call(cbind, correct.order)
    ## randomly swap one of the cases
    swapCase <- function(x)
    {
        is.lower <- letters %in% x
        if (any(is.lower))
            return(LETTERS[which(is.lower)])
        letters[which(LETTERS %in% x)]
    }
    shuffled.vars[2:3] <- lapply(shuffled.vars[2:3], function(x) {
        nam <- names(x)
        n.chars <- nchar(nam)
        index.char.to.swap <- vapply(n.chars, function(x) sample(2:(x - 1), size = 1), integer(1))
        char.to.swap <- substr(nam, index.char.to.swap, index.char.to.swap)
        swapped.case <- vapply(char.to.swap, swapCase, character(1))
        new.names <- paste0(substr(nam, 1, index.char.to.swap - 1),
                            swapped.case,
                            substr(nam, index.char.to.swap + 1, n.chars))
        names(x) <- new.names
        x
    })
    expect_equal(SumRows(shuffled.vars[[1L]], shuffled.vars[[2L]],
                         match.elements = "Fuzzy - ignore if unmatched"),
                 rowSums(correct.binded[, 1:2]))
    expect_equal(SumRows(shuffled.vars[[1L]], shuffled.vars[[3L]],
                         match.elements = "Fuzzy - ignore if unmatched"),
                 rowSums(correct.binded[, c(1, 3)]))
    extra <- shuffled.vars[1:2]
    answer <- c("Answer" = 42)
    extra[[2L]] <- append(extra[[2L]], values = answer)
    expect_equal(SumRows(extra[[1L]], extra[[2L]],
                         match.elements = "Fuzzy - ignore if unmatched"),
                 append(rowSums(correct.binded[, c(1, 2)]), answer))
    # Error if unmatched
    expect_error(SumRows(extra[[1L]], extra[[2L]],
                         match.elements = "Fuzzy - error if unmatched"),
                 paste0(sQuote("SumRows"), " cannot be computed since matching elements by name ",
                        "is required. However, after possible removing rows, the input elements ",
                        "have different lengths (6 and 7 respectively). Consider relaxing the ",
                        "name matching options or modify the inputs to have the same number of ",
                        "elements before proceeding with a name matched computation again."),
                 fixed = TRUE)
})
