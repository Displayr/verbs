context("Utilities")
data(variable.Text)
data(variable.Binary)
data(variable.Nominal)
data(variable.Numeric)
data(variable.Time)
data(variable.Date)
data(table1D.Average)
data(table1D.Percentage)
data(table.1D.MultipleStatistics)
data(table2D.Percentage)
data(table2D.PercentageAndCount)
data(table2D.PercentageNaN)
test_that("Dimension checking functions", {
    # QTables
    ## Inspect the table structure
    expect_equal(getDim(table1D.Average), 1L)
    expect_equal(getDim(table1D.Percentage), 1L)
    expect_equal(getDim(table.1D.MultipleStatistics), 2L)
    expect_equal(getDim(table2D.Percentage), 2L)
    expect_equal(getDim(table2D.PercentageAndCount), 3L)
    expect_equal(getDim(table2D.PercentageNaN), 2L)
    # Tables without attributes ok
    stucture1d.no.att <- table1D.Average[TRUE]
    stucture2d.no.att <- table.1D.MultipleStatistics[TRUE, TRUE]
    stucture3d.no.att <- table2D.PercentageAndCount[TRUE, TRUE, TRUE]
    expect_equal(getDim(stucture1d.no.att), 1)
    expect_equal(getDim(1:3), 1)
    expect_equal(getDim(stucture2d.no.att), 2)
    expect_equal(getDim(matrix(1:4, nrow = 2)), 2)
    expect_equal(getDim(matrix(1:4, nrow = 4)), 2)
    expect_equal(getDim(stucture3d.no.att), 3)
})

test_that("Check elements for opposite Infinities", {
    # Check warning thrown when appropriate
    expect_true(checkForOppositeInfinites(c(Inf, -Inf, 1)))
    expect_false(checkForOppositeInfinites(1:3))
    expect_false(checkForOppositeInfinites(c(Inf, 1:3)))
    expect_false(checkForOppositeInfinites(c(1:3, -Inf)))
    # Not confused by NA values
    expect_false(checkForOppositeInfinites(c(NA, -Inf, 1)))
})

test_that("Check vector appropriate", {
    var1 <- runif(5)
    names(var1) <- LETTERS[1:5]
    var2 <- var1
    names(var2) <- letters[1:5]
    expect_equal(vector.names <- lapply(list(var1, var2), rowNames),
                 list(LETTERS[1:5], letters[1:5]))
    expect_error(checkPartiallyNamedVector(vector.names, "'Test'"), NA)
    expect_error(checkPartiallyNamedVector(rev(vector.names), "'Test'"), NA)
    names(var2)[1] <- NA
    expect_equal(vector.names <- lapply(list(var1, var2), rowNames),
                 list(LETTERS[1:5], c(NA, letters[2:5])))
    expect_error(checkPartiallyNamedVector(vector.names, "'Test'"),
                 paste0("'Test' requires either a fully named vector or a vector ",
                        "with no names to calculate output. Some elements of the ",
                        "input vector have names while other elements are not named. ",
                        "Please name all elements if you wish to compute 'Test' by ",
                        "matching elements. ",
                        determineAppropriateContact()))
    expect_error(checkPartiallyNamedVector(rev(vector.names), "'Test'"),
                 paste0("'Test' requires either a fully named vector or a vector ",
                        "with no names to calculate output. Some elements of the ",
                        "input vector have names while other elements are not named. ",
                        "Please name all elements if you wish to compute 'Test' by ",
                        "matching elements. ",
                        determineAppropriateContact()))
})

test_that("Row and column checking functions",{
    # Confirm row name lookup is adequate
    expect_equal(verbs:::rowNames(table1D.Average), names(table1D.Average))
    expect_equal(verbs:::rowNames(table1D.Percentage), names(table1D.Percentage))
    expect_equal(verbs:::rowNames(table.1D.MultipleStatistics), row.names(table.1D.MultipleStatistics))
    expect_equal(verbs:::rowNames(table2D.Percentage), row.names(table2D.Percentage))
    expect_equal(verbs:::rowNames(table2D.PercentageAndCount), row.names(table2D.PercentageAndCount))
    expect_equal(verbs:::rowNames(table2D.PercentageNaN), row.names(table2D.PercentageNaN))
    # Confirm column name lookup is adequate
    expect_equal(verbs:::colNames(table1D.Average), NULL)
    expect_equal(verbs:::colNames(table1D.Percentage), NULL)
    expect_equal(verbs:::colNames(table.1D.MultipleStatistics), colnames(table.1D.MultipleStatistics))
    expect_equal(verbs:::colNames(table2D.Percentage), colnames(table2D.Percentage))
    expect_equal(verbs:::colNames(table2D.PercentageAndCount), colnames(table2D.PercentageAndCount))
    expect_equal(verbs:::colNames(table2D.PercentageNaN), colnames(table2D.PercentageNaN))
    basic.vector <- 1:10
    names(basic.vector) <- LETTERS[1:10]
    expect_equal(rowNames(basic.vector), names(basic.vector))
    expect_equal(colNames(basic.vector), NULL)
    basic.matrix <- matrix(1:12, nrow = 3, ncol = 4, dimnames = list(LETTERS[1:3], letters[1:4]))
    expect_equal(rowNames(basic.matrix), rownames(basic.matrix))
    expect_equal(colNames(basic.matrix), colnames(basic.matrix))
    # Add tests about subsetting tables
    ## Check helper function removeElementsFromArray first, uses logical to subset
    expect_equivalent(removeElementsFromArray(table1D.Average, keep.rows = c(TRUE, TRUE, FALSE, FALSE)),
                      table1D.Average[1:2])
    expect_equivalent(removeElementsFromArray(table1D.Percentage,
                                                      keep.rows = c(rep(FALSE, 3), rep(TRUE, 4), rep(FALSE, 3))),
                      table1D.Percentage[4:7])
    expect_equivalent(removeElementsFromArray(table.1D.MultipleStatistics,
                                                      keep.rows = c(FALSE, TRUE, TRUE, FALSE)),
                      table.1D.MultipleStatistics[2:3, ])
    ## Exoect error if an array with 3 dimensions is input but its not a 2D QTable with multiple statistics
    expect_error(removeElementsFromArray(array(1:24, dim = 2:4), function.name = "'Test'"),
                 paste0("'Test' only supports inputs that have 1 or 2 dimensions. A supplied input has 3 ",
                        "dimensions. ", determineAppropriateContact()),
                 fixed = TRUE)
    ## Check next function, removeRowsAndCols that determines the logical indices
    default.removal <- c("NET", "SUM", "Total")
    expect_equivalent(removeRowsAndCols(table1D.Average,
                                                remove.rows = default.removal,
                                                remove.columns = default.removal,
                                                warn = FALSE),
                      table1D.Average[1:3])
    expect_equivalent(removeRowsAndCols(table1D.Percentage,
                                                remove.rows = default.removal,
                                                remove.columns = default.removal,
                                                warn = FALSE),
                      table1D.Percentage[1:9])
    expect_equivalent(removeRowsAndCols(table.1D.MultipleStatistics,
                                                remove.rows = default.removal,
                                                remove.columns = default.removal,
                                                warn = FALSE),
                      table.1D.MultipleStatistics[1:3, ])
    # Check remove columns as well.
    ## Check internal helper function with the logical indices
    expect_equivalent(removeElementsFromArray(table.1D.MultipleStatistics,
                                              keep.rows = c(FALSE, TRUE, TRUE, FALSE),
                                              keep.columns = c(TRUE, rep(FALSE, 5))),
                      table.1D.MultipleStatistics[2:3, 1])
    expect_equivalent(removeElementsFromArray(table2D.Percentage,
                                              keep.rows = c(rep(FALSE, 2), rep(TRUE, 2), rep(FALSE, 2)),
                                              keep.columns = c(rep(FALSE, 3), rep(TRUE, 4), rep(FALSE, 3))),
                      table2D.Percentage[3:4, 4:7])
    expect_equivalent(removeElementsFromArray(table2D.PercentageAndCount,
                                              keep.rows = c(rep(FALSE, 2), rep(TRUE, 2), rep(FALSE, 2)),
                                              keep.columns = c(rep(FALSE, 3), rep(TRUE, 4), rep(FALSE, 3))),
                      table2D.PercentageAndCount[3:4, 4:7, ])
    ## Check outer function that removes appropriate slices
    expect_equivalent(table.subsetted <- removeRowsAndCols(table.1D.MultipleStatistics,
                                                           remove.rows = default.removal,
                                                           remove.columns = "z-Statistic",
                                                           warn = TRUE),
                      table.1D.MultipleStatistics[1:3, -5])
    expect_equal(attr(table.subsetted, "Removed Indices"), list(rows = "SUM", columns = "z-Statistic"))
    captured.warnings <- capture_warnings(warnAboutRemovedElements(list(table.subsetted)))
    expect_setequal(captured.warnings,
                    c("These categories have been removed from the rows: SUM.",
                      "These categories have been removed from the columns: z-Statistic."))
    # Check entriesToKeep function
    ## If no valid comparisons are made, fall back to the required logical vector
    expect_equal(entriesToKeep(LETTERS[1:3], "B"), c(TRUE, FALSE, TRUE))
    expect_equal(entriesToKeep(NULL, "B", dim.length = 4), rep(TRUE, 4))
})

test_that("QTable: Inspecting Statistics and throwing warnings", {
    # Check statistics name lookup in tables working as expected
    expect_equal(lookupStatistics(table1D.Average), "Average")
    expect_equal(lookupStatistics(table1D.Percentage), "%")
    expect_equal(lookupStatistics(table.1D.MultipleStatistics), colnames(table.1D.MultipleStatistics))
    expect_equal(lookupStatistics(table2D.Percentage), "Row %")
    expect_equal(lookupStatistics(table2D.PercentageAndCount), dimnames(table2D.PercentageAndCount)[[3]])
    expect_equal(lookupStatistics(table2D.PercentageNaN), "%")
    expect_equal(possibleStatistics(table1D.Average), "Average")
    expect_equal(possibleStatistics(table1D.Percentage), "%")
    expect_equal(possibleStatistics(table.1D.MultipleStatistics), colnames(table.1D.MultipleStatistics))
    expect_equal(possibleStatistics(table2D.Percentage), "Row %")
    expect_equal(possibleStatistics(table2D.PercentageAndCount), dimnames(table2D.PercentageAndCount)[[3]])
    expect_equal(possibleStatistics(table2D.PercentageNaN), "%")
    ## Check possible filters out ones that aren't a statistic
    struct1d <- table1D.Average[TRUE]
    struct2d <- table.1D.MultipleStatistics[TRUE, TRUE]
    colnames(struct2d) <- LETTERS[1:NCOL(struct2d)]
    attr(struct1d, "statistic") <- "This is a red herring"
    expect_equal(possibleStatistics(struct1d), "This is a red herring")
    expect_null(lookupStatistics(struct1d))
    expect_equal(possibleStatistics(struct2d), colnames(struct2d))
    expect_null(lookupStatistics(struct2d))
    # Warning function works ok
    input.stats <- c("Average", "Standard Error")
    expect_warning(throwWarningAboutDifferentStatistics(input.stats,
                                                                function.name = "'Hello'"),
                   paste0("The input data contains statistics of different types (i.e., ",
                          paste0(input.stats, collapse = ", "),
                          "), it may not be ",
                          "appropriate to compute 'Hello'."),
                   fixed = TRUE)
    # Check thrown warnings, single input
    expect_warning(checkForMultipleStatistics(table1D.Average, function.name = "'Sum'"), NA)
    expect_warning(checkForMultipleStatistics(table1D.Percentage, function.name = "'Sum'"), NA)
    expect_warning(checkForMultipleStatistics(table.1D.MultipleStatistics, function.name = "'Sum'"),
                   paste0("The input data contains statistics of different types (i.e., Average, Effective Sample Size, ",
                          "t-Statistic, d.f., z-Statistic, Corrected p), it may not be appropriate to compute 'Sum'."),
                   fixed = TRUE)
    expect_warning(checkForMultipleStatistics(table2D.Percentage, function.name = "'Sum'"), NA)
    expect_warning(checkForMultipleStatistics(table2D.PercentageAndCount, function.name = "'Sum'"),
                   paste0("The input data contains statistics of different types (i.e., Row %, Count), it may not be ",
                          "appropriate to compute 'Sum'."),
                   fixed = TRUE)
    expect_warning(checkForMultipleStatistics(table2D.PercentageNaN, function.name = "'Sum'"), NA)
    # Check function name correct
    expect_warning(checkForMultipleStatistics(table.1D.MultipleStatistics, function.name = "'Hello'"),
                   paste0("The input data contains statistics of different types (i.e., Average, ",
                          "Effective Sample Size, t-Statistic, d.f., z-Statistic, Corrected p), it may not be ",
                          "appropriate to compute 'Hello'."),
                   fixed = TRUE)
})


test_that("Contact details correct", {
    # Expect open source contact correct
    expect_equal(determineAppropriateContact(),
                 paste0("Contact support at opensource@displayr.com or raise an issue at ",
                        "https://github.com/Displayr/verbs if you wish this to be changed."),
                 fixed = TRUE)
    # Expect customer support contact correct
    expect_equal(with_mock(IsRServer = function() TRUE,
                           determineAppropriateContact(),
                           .env = "flipU"),
                 "Contact support at support@displayr.com if you wish this to be changed.",
                 fixed = TRUE)
    expect_error(throwErrorContactSupportForRequest("isn't supported. ", "'some func'"),
                 paste0("'some func' isn't supported. ", determineAppropriateContact()))
})

test_that("Check missing data handling", {
    expect_warning(checkMissingData(remove.missing = FALSE), NA)
    expect_warning(checkMissingData(list(1:3), remove.missing = TRUE), NA)
    expect_warning(checkMissingData(list(c(1, NA, 3)), remove.missing = TRUE),
                   "Missing values have been ignored in calculation.")
})

test_that("Subset and Weights handled correctly", {
    # checkWeights
    ## Valid weights pass the check
    n <- 100L
    x <- runif(n)
    weight.test <- runif(n)
    expect_equal(checkWeights(weight.test, n), weight.test)
    ## Check helper function multiples appropriately
    expect_equal(weightInput(x, weight.test),
                 x * weight.test)
    ## Check weights with negative elements are set to zero
    invalid.weights <- weight.test
    rand.negatives <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
    invalid.weights[rand.negatives] <- -runif(sum(rand.negatives))
    expect_warning(output <- checkWeights(invalid.weights, n, warn = TRUE),
                   "Elements with negative weights were set to have weight of zero")
    expect_equal(output[rand.negatives], rep(0, sum(rand.negatives)))
    ## Check weights with missing values are set to zero
    invalid.weights <- weight.test
    rand.negatives <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
    invalid.weights[rand.negatives] <- NA
    expect_warning(output <- checkWeights(invalid.weights, n, warn = TRUE),
                   "Weights with missing elements were set to have a weight of zero")
    expect_equal(output[rand.negatives], rep(0, sum(rand.negatives)))
    ## Check weight vector that is the wrong length will throw an error
    invalid.weight <- weight.test[seq_len(n/2)]
    expect_error(checkWeights(invalid.weight, n),
                 paste0("The weights vector has length ", length(invalid.weight), ". However, it needs to have ",
                 "length ", n, " to match the number of cases in the supplied input data."))
    # checkSubset
    subset.test <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
    expect_error(checkSubset(subset.test, n), NA)
    ## Check missing elements in subset vector converted to FALSE
    broken.subset <- subset.test
    broken.subset[sample(c(TRUE, FALSE), size = n, replace = TRUE)] <- NA
    expect_warning(subset.out <- checkSubset(broken.subset, n, warn = TRUE),
                   paste0("The subset argument contains missing values. ",
                          "Data correspondong to these were filtered out."))
    expected.subset <- broken.subset
    expected.subset[is.na(expected.subset)] <- FALSE
    expect_equal(subset.out, expected.subset)
    ## Check susbet vector that is the wrong length will throw an error
    invalid.subset <- subset.test[seq_len(n/2)]
    expect_error(checkSubset(invalid.subset, n),
                 paste0("The subset vector has length ", length(invalid.weight), ". However, it needs to have ",
                        "length ", n, " to match the number of cases in the supplied input data."))
    subset.input <- seq_along(subset.test)
    attr(subset.input, "foo") <- "bar"
    expect_equal(subsetInput(subset.input, subset.test),
                 structure(which(subset.test), foo = "bar"))
    # Expect errors with invalid subset and/or weight inputs
    ## Expect invalid subset and weight vectors to throw an error
    expect_error(subsetAndWeightInputs(1L, subset = "Hello"),
                 "The subset argument should be a logical vector")
    expect_error(subsetAndWeightInputs(1L, subset = NULL, weights = "Hello"),
                 "The weights argument should be a numeric vector")
    rand.in <- lapply(5:10, rnorm)
    expect_equal(subsetAndWeightInputs(rand.in,
                                       subset = rep(TRUE, 5),
                                       weights = NULL,
                                       function.name = "'Test'"),
                 rand.in)
    expect_error(subsetAndWeightInputs(rand.in,
                                       subset = c(rep(TRUE, 4), FALSE),
                                       weights = NULL,
                                       function.name = "'Test'"),
                 paste0("'Test' requires all input elements to have the same size to be able to ",
                        "apply a filter or weight vector. ", determineAppropriateContact()),
                 fixed = TRUE)
    simple.df <- function()
    {
        out <- data.frame(lapply(1:3, function(x) rnorm(10)))
        names(out) <- LETTERS[1:3]
        attr(out , "foo") <- "bar"
        out
    }
    many.df <- replicate(5, simple.df(), simplify = FALSE)
    subset.test <- rep(c(TRUE, FALSE, TRUE, FALSE, TRUE), 2)
    subsetted.dfs <- lapply(many.df, function(x) {
        out <- x[subset.test, ]
        out
    })
    expect_equal(subsetAndWeightInputs(many.df, subset = subset.test),
                 subsetted.dfs)
    weights.test <- runif(10)
    subsetted.and.weighted.dfs <- lapply(many.df, function(x) {
        out <- x[subset.test, ] * weights.test[subset.test]
        out
    })
    expect_equal(subsetAndWeightInputs(many.df,
                                       subset = subset.test,
                                       weights = weights.test),
                 subsetted.and.weighted.dfs)
    ## Tests to see Q Tables ignored and warned when used with subset and weights
    expect_equal(subsetAndWeightInputs(list(table1D.Average, table1D.Percentage),
                                       subset = rep(c(TRUE, FALSE), c(5, 5))),
                 list(table1D.Average, table1D.Percentage))
    warn.msg <- "'Test' is unable to apply a filter to the input Q Tables since the original variable data is unavailable."
    expect_warning(subsetAndWeightInputs(list(table1D.Average, table1D.Percentage),
                                         subset = rep(c(TRUE, FALSE), c(5, 5)),
                                         warn = TRUE,
                                         function.name = "'Test'"),
                   warn.msg)
    warn.msg <- sub("a filter", "weights", warn.msg)
    expect_warning(subsetAndWeightInputs(list(table1D.Average, table1D.Percentage),
                                         weights = runif(5),
                                         warn = TRUE,
                                         function.name = "'Test'"),
                   warn.msg)
    warn.msg <- sub("weights", "a filter or weights", warn.msg)
    expect_warning(subsetAndWeightInputs(list(table1D.Average, table1D.Percentage),
                                         subset = c(TRUE, FALSE),
                                         weights = runif(5),
                                         warn = TRUE,
                                         function.name = "'Test'"),
                   warn.msg)
    expect_equal(weightInput(table1D.Average, weights = runif(5)), table1D.Average)
    expect_equal(subsetInput(table1D.Average, subset = runif(5)), table1D.Average)
})

test_that("Data types checked", {
    data(variable.Date)
    data(variable.Text)
    data(variable.Numeric)
    data(variable.Nominal)
    data(table1D.Average)
    expect_error(checkIfCharacter(variable.Text, function.name = "'test'"),
                 "Text data has been supplied but 'test' requires numeric data.")
    expect_error(checkIfDateTime(variable.Date, function.name = "'test'"),
                 "Date/Time data has been supplied but 'test' requires numeric data.")
    # Expect error for Date and Text variables
    expect_error(checkInputTypes(variable.Date, "'Test'"),
                 "Date/Time data has been supplied but 'Test' requires numeric data.")
    expect_error(checkInputTypes(variable.Text, "'Test'"),
                 "Text data has been supplied but 'Test' requires numeric data.")
    expect_error(checkInputTypes(variable.Binary, "'Test'"), NA)
    expect_error(checkInputTypes(list(table1D.Average, variable.Numeric), "'Test'"),
                 paste0("'Test' requires input elements to be of the same type. ",
                        "However, both QTables and Variables have been used as inputs. ",
                        "It is not possible to use 'Test' with multiple inputs of ",
                        "different types. Contact support at opensource@displayr.com ",
                        "or raise an issue at https://github.com/Displayr/verbs if ",
                        "you wish this to be changed."))
    ## Check conversion from categorical to numeric occurs
    expect_equal(convertToNumeric(list(variable.Nominal)),
                 list(flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)))
    # Expect error if a QTable and Variables are both used as inputs
    expect_error(checkInputsDontContainTablesAndVariables(list(variable.Numeric, table1D.Average),
                                                          function.name = "'Test'"),
                 paste0("'Test' requires input elements to be of the same type. However, ",
                        "both QTables and Variables have been used as inputs. ",
                        "It is not possible to use 'Test' ",
                        "with multiple inputs of different types. ",
                        determineAppropriateContact()))
    ## Check if multiple variable are entered, error if not
    expect_error(checkIfSuitableVectorType(variable.Binary, function.name = "'test'"), NA)
    expect_error(checkIfSuitableVectorType(variable.Text, function.name = "'test'"),
                 "Text data has been supplied but 'test' requires numeric data.")
    expect_error(checkIfSuitableVectorType(variable.Date, function.name = "'test'"),
                 "Date/Time data has been supplied but 'test' requires numeric data.")
    error.msg <- paste0("'test' does not support multiple inputs unless they are all ",
                        "individual variables or vectors. One of the inputs here has ",
                        "class : list. Contact support at opensource@displayr.com or ",
                        "raise an issue at https://github.com/Displayr/verbs if you ",
                        "wish this to be changed.")
    expect_error(checkIfSuitableVectorType(list(""), function.name = "'test'"), error.msg)
    error.msg <- sub("list", "matrix, array", error.msg)
    expect_error(checkIfSuitableVectorType(matrix(0), function.name = "'test'"), error.msg)
    error.msg <- sub("has class : matrix, array", "is a data frame", error.msg)
    expect_error(checkIfSuitableVectorType(data.frame(1:5), function.name = "'test'"), error.msg)
    error.msg <- sub("data frame", "Q Table", error.msg)
    expect_error(checkIfSuitableVectorType(table1D.Average, function.name = "'test'"), error.msg)
    fake.variable.set <- data.frame(x = 1:5, y = 6:10)
    attr(fake.variable.set, "question") <- "Some Q"
    attr(fake.variable.set, "questiontype") <- "Number - Multi"
    error.msg <- sub("Q Table", "Variable Set", error.msg)
    expect_error(checkIfSuitableVectorType(fake.variable.set, function.name = "'test'"), error.msg)
})

test_that("ExtractChartData", {
    var1 <- variable.Numeric
    var2 <- runif(length(var1))
    correlation.output <- flipStatistics::CorrelationMatrix(data.frame(var1, var2))
    expect_equivalent(extractChartDataIfNecessary(correlation.output),
                      cor(data.frame(var1, var2), use = "complete.obs"))
})

# Helper function to shuffle second element, useful for the matching tests
.shuffleSecond <- function(x)
{
    n.second <- length(x[[2]])
    ind <- sample(n.second)
    while(any(ind == 1:n.second))
        ind <- sample(n.second)
    x[[2]] <- x[[2]][ind]
    x
}

test_that("Exact matching functions", {
    # exact match of names - error if unmatched
    ## Unnamed inputs, all is ok
    unnamed.inputs <- replicate(2, runif(5), simplify = FALSE)
    expected.unnamed <- do.call(cbind, unnamed.inputs)
    expect_equal(exactMatchRowNames(unnamed.inputs,
                                    ignore.unmatched = FALSE,
                                    warn = TRUE,
                                    function.name = sQuote("test")),
                 expected.unnamed)
    ## Unnamed inputs of different size, should error
    diff.size.unnamed.inputs <- unnamed.inputs
    diff.size.unnamed.inputs[[1]] <- append(diff.size.unnamed.inputs[[1]], runif(1))
    expected.diff.size.unnamed <- diff.size.unnamed.inputs
    expected.diff.size.unnamed[[2]] <- append(expected.diff.size.unnamed[[2]], 0)
    expected.diff.size.unnamed <- do.call(cbind, expected.diff.size.unnamed)
    expect_error(exactMatchRowNames(diff.size.unnamed.inputs,
                                    ignore.unmatched = FALSE,
                                    warn = TRUE,
                                    function.name = sQuote("test")),
                 paste0(sQuote("test"), " cannot be computed since matching elements by name ",
                        "is required. However, after possible removing rows, the input elements have ",
                        "different lengths (6 and 5 respectively). Consider relaxing the name matching ",
                        "options or modify the inputs to have the same number of elements before ",
                        "proceeding with a name matched computation again."),
                 fixed = TRUE)
    ## ok for inputs with same size and names
    inputs.same.size.and.names <- replicate(2, {
        x <- runif(5)
        names(x) <- letters[1:5]
        x
    }, simplify = FALSE)
    output.same.size.and.names <- do.call(cbind, inputs.same.size.and.names)
    expect_equal(exactMatchRowNames(inputs.same.size.and.names,
                                    ignore.unmatched = FALSE,
                                    warn = TRUE,
                                    function.name = sQuote("test")),
                 output.same.size.and.names)
    expect_equal(matchRows(inputs.same.size.and.names,
                           match.elements = 'Yes - error if unmatched',
                           warn = TRUE,
                           function.name = sQuote("test")),
                 output.same.size.and.names)
    ## Same size, jumbled names
    permuted <- .shuffleSecond(inputs.same.size.and.names)
    expect_equal(exactMatchRowNames(permuted,
                                    ignore.unmatched = FALSE,
                                    warn = TRUE,
                                    function.name = sQuote("test")),
                 output.same.size.and.names)
    ## Different names
    inputs.different.names <- inputs.same.size.and.names
    names(inputs.different.names[[1L]])[1] <- "Z"
    exp.err <- paste0(sQuote("test"), " requires inputs to have matching row names. ",
                      "However, some inputs have names they don't match, i.e. a ",
                      "named element doesn't occur in all input elements, e.g. ",
                      "the elements named : 'Z', 'a'. Consider changing the name matching ",
                      "options or ensure all the names match before recomputing.")
    expect_error(exactMatchRowNames(inputs.different.names,
                                    ignore.unmatched = FALSE,
                                    warn = TRUE,
                                    function.name = sQuote("test")),
                 exp.err)
    ## One unnamed, one named input
    inputs.all.named.and.unnamed <-list(inputs.same.size.and.names[[1]],
                                        unnamed.inputs[[1]])
    expected.combo <- do.call(cbind, list(inputs.same.size.and.names[[1]], rep(0, 5)))
    expect_warning(expect_equal(exactMatchRowNames(inputs.all.named.and.unnamed,
                                                   ignore.unmatched = TRUE,
                                                   warn = TRUE,
                                                   function.name = sQuote("test")),
                                expected.combo),
                   paste0("One of the input elements doesn't have any names and ",
                          "cannot be matched. Consider changing the name matching ",
                          "options or ensure all the names match before recomputing."))
    # exact match of names - ignore if unmatched
    ## unnamed inputs ok
    output.unnamed <- diff.size.unnamed.inputs
    output.unnamed[[2]] <- append(output.unnamed[[2]], 0)
    output.unnamed <- do.call(cbind, output.unnamed)
    expect_equal(exactMatchRowNames(diff.size.unnamed.inputs,
                                    ignore.unmatched = TRUE,
                                    warn = TRUE,
                                    function.name = sQuote("test")),
                 output.unnamed)
    expect_equal(exactMatchRowNames(rev(diff.size.unnamed.inputs),
                                    ignore.unmatched = TRUE,
                                    warn = TRUE,
                                    function.name = sQuote("test")),
                 output.unnamed[, 2:1])
    ## named inputs, same dim and names ok
    expect_equal(exactMatchRowNames(inputs.same.size.and.names,
                                    ignore.unmatched = TRUE,
                                    warn = TRUE,
                                    function.name = sQuote("test")),
                 output.same.size.and.names)
    expect_equal(matchRows(inputs.same.size.and.names,
                           match.elements = 'Yes - ignore if unmatched',
                           warn = TRUE,
                           function.name = sQuote("test")),
                 output.same.size.and.names)
    ## Same size, jumbled names
    permuted.inputs.same.size.and.names <- .shuffleSecond(inputs.same.size.and.names)
    expect_equal(exactMatchRowNames(permuted.inputs.same.size.and.names,
                                    ignore.unmatched = TRUE,
                                    warn = TRUE,
                                    function.name = sQuote("test")),
                 output.same.size.and.names)
    inputs.different.size <- inputs.same.size.and.names
    inputs.different.size[[2]] <- append(inputs.different.size[[2]],
                                         c(f = runif(1)))
    output.different.size <- inputs.different.size
    output.different.size[[1]] <- append(output.different.size[[1]], c('f' = 0))

    output.different.size <- do.call(cbind, output.different.size)
    exp.err <- paste0(sQuote("test"), " requires inputs to have matching row names. ",
                      "However, some inputs have names they don't match, i.e. a ",
                      "named element doesn't occur in all input elements, e.g. ",
                      "the element named : 'f'. Consider changing the name matching ",
                      "options or ensure all the names match before recomputing.")
    expect_equal(exactMatchRowNames(inputs.different.size,
                                    ignore.unmatched = TRUE,
                                    warn = TRUE,
                                    function.name = sQuote("test")),
                 output.different.size)
    permuted.inputs <- .shuffleSecond(inputs.different.size)
    expect_equal(exactMatchRowNames(inputs.different.size,
                                    ignore.unmatched = TRUE,
                                    warn = TRUE,
                                    function.name = sQuote("test")),
                 output.different.size)
    ## Different names
    inputs.different.names <- inputs.different.size
    inputs.different.names[[1]] <- append(inputs.different.size[[1]],
                                          c("Z" = runif(1)), after = 1)
    output.different.names <- inputs.different.names
    output.different.names[[2]] <- append(output.different.names[[2]], c(`Z` = 0), after = 1)
    output.different.names[[1]] <- append(output.different.names[[1]], c(`f` = 0))
    output.different.names <- do.call(cbind, output.different.names)
    exp.err <- paste0(sQuote("test"), " requires inputs to have matching row names. ",
                      "However, some inputs have names they don't match, i.e. a ",
                      "named element doesn't occur in all input elements, e.g. ",
                      "the elements named : 'Z', 'f'. Consider changing the name matching ",
                      "options or ensure all the names match before recomputing.")
    expect_equal(exactMatchRowNames(inputs.different.names,
                                    ignore.unmatched = TRUE,
                                    warn = TRUE,
                                    function.name = sQuote("test")),
                 output.different.names)
    # No matching, only checking dimensions
    ## no named input of same dimension ok
    expect_equal(matchRows(unnamed.inputs,
                           match.elements = "No",
                           warn = TRUE,
                           function.name = sQuote("test")),
                 expected.unnamed)
    ## Diff sized unnamed input not ok.
    expect_error(matchRows(diff.size.unnamed.inputs,
                           match.elements = "No",
                           warn = TRUE,
                           function.name = sQuote("test")),
                 paste0("Two inputs have a different number of rows and cannot be joined to compute ", sQuote("test")))
    ## Inputs correct size but different names and warnings requested
    expect_warning(expect_equal(matchRows(inputs.different.names,
                                          match.elements = "No",
                                          warn = TRUE,
                                          function.name = sQuote("test")),
                                do.call(cbind, inputs.different.names)),
                   paste0("The argument for matching names was set to 'No' in ", sQuote("test"), ". ",
                          "However, the inputs don't have identical row names and the ",
                          "calculation in ", sQuote("test"), " might not be appropriate."))
})

test_that("Fuzzy matching", {
    # Check punctuation and white space removed
    text.in <- c("Don't", "I can't and won't", "This is awesome!", "#hashtag", "http://")
    names.out <- c("dont", "icantandwont", "thisisawesome", "hashtag", "http")
    text.out <- text.in
    names(text.out) <- names.out
    expect_equal(simplifyTextForFuzzyMatching(text.in), text.out)
    ## Check unnamed vectors are ok with fuzzy matching specified
    unnamed <- replicate(2, runif(2), simplify = FALSE)
    expect_equal(fuzzyMatchRowNames(unnamed, ignore.unmatched = TRUE), do.call(cbind, unnamed))
    expect_equal(fuzzyMatchRowNames(unnamed, ignore.unmatched = FALSE), do.call(cbind, unnamed))
    # Check exact matches work when attemping to fuzzy match
    exact.in <- replicate(2, {
        x <- runif(5)
        names(x) <- letters[1:5]
        x
    }, simplify = FALSE)
    expected.out <- do.call(cbind, exact.in)
    expect_equal(fuzzyMatchRowNames(exact.in, ignore.unmatched = TRUE), expected.out)
    expect_equal(fuzzyMatchRowNames(exact.in, ignore.unmatched = FALSE), expected.out)
    # Shuffle the 2nd list and check answer is still correct
    shuffled.exact.in <- .shuffleSecond(exact.in)
    expect_equal(fuzzyMatchRowNames(shuffled.exact.in, ignore.unmatched = TRUE), expected.out)
    expect_equal(fuzzyMatchRowNames(shuffled.exact.in, ignore.unmatched = FALSE), expected.out)
    ## Make the second use all upper case names requiring fuzzy matching
    names.near.exact <- exact.in
    names(names.near.exact[[2]]) <- toupper(names(names.near.exact[[2]]))
    expect_equal(fuzzyMatchRowNames(names.near.exact, ignore.unmatched = TRUE), expected.out)
    expect_equal(fuzzyMatchRowNames(names.near.exact, ignore.unmatched = FALSE), expected.out)
    # Test case that requires fuzzy matching
    test.in <- replicate(2, {
        x <- runif(7)
        names(x) <- c("Hello", "Don't know", "None of these", "Other", "Burger",
                      "Sushi", "Pizza")
        x
    }, simplify = FALSE)
    full.expected.out <- do.call(cbind, list(test.in[[1L]],
                                             test.in[[2L]]))
    variants.only.out <- full.expected.out[1:4, ]
    permuted.exact.in <- .shuffleSecond(exact.in)
    expect_equal(fuzzyMatchRowNames(permuted.exact.in, ignore.unmatched = TRUE), expected.out)
    expect_equal(fuzzyMatchRowNames(permuted.exact.in, ignore.unmatched = FALSE), expected.out)
    test.only.variants <- test.in
    test.only.variants <- lapply(test.only.variants, function(x) x[1:4])
    # Create variants
    names(test.only.variants[[2]])[names(test.only.variants[[2]]) == "None of these"] <- "none"
    names(test.only.variants[[2]])[names(test.only.variants[[2]]) == "Don't know"] <- "dont know"
    names(test.only.variants[[2]])[names(test.only.variants[[2]]) == "Other"] <- "  other     "
    expect_equal(fuzzyMatchRowNames(test.only.variants, ignore.unmatched = TRUE), variants.only.out)
    expect_equal(fuzzyMatchRowNames(test.only.variants, ignore.unmatched = FALSE), variants.only.out)
    shuffled.only.variants <- .shuffleSecond(test.only.variants)
    expect_equal(fuzzyMatchRowNames(shuffled.only.variants, ignore.unmatched = TRUE), variants.only.out)
    expect_equal(fuzzyMatchRowNames(shuffled.only.variants, ignore.unmatched = FALSE), variants.only.out)
    test.non.matching.case <- test.in
    names(test.non.matching.case[[2]]) <- tolower(names(test.non.matching.case[[2]]))
    names(test.non.matching.case[[2]])[names(test.non.matching.case[[2]]) == "hello"] <- "Hello" # 1 exact match
    names(test.non.matching.case[[2]])[names(test.non.matching.case[[2]]) == "burger"] <- "Berger" # 1 character off
    names(test.non.matching.case[[2]])[names(test.non.matching.case[[2]]) == "none of these"] <- "none" # variant of none of these
    # Start with test that only has the variants
    names.to.test <- c("hello", "don't know", "none of these", "other", "none")
    test.with.variants <- lapply(test.in, function(x) x[tolower(names(x)) %in% names.to.test])
    expected.out.variants <- do.call(cbind, list(test.with.variants[[1L]],
                                                 test.with.variants[[2L]]))
    test.in.with.variants.and.case <- lapply(test.in, function(x) x[-5])
    expected.out <- do.call(cbind, test.in.with.variants.and.case)
    expect_equal(fuzzyMatchRowNames(test.in.with.variants.and.case, ignore.unmatched = TRUE), expected.out)
    expect_equal(fuzzyMatchRowNames(test.in.with.variants.and.case, ignore.unmatched = FALSE), expected.out)
    shuffled.test.in.with.variants.and.case <- .shuffleSecond(test.in.with.variants.and.case)
    expect_equal(fuzzyMatchRowNames(shuffled.test.in.with.variants.and.case, ignore.unmatched = TRUE), expected.out)
    expect_equal(fuzzyMatchRowNames(shuffled.test.in.with.variants.and.case, ignore.unmatched = FALSE), expected.out)
    test.misc <- replicate(2, {
        x <- runif(7)
        names(x) <- c("Hello", "Don't know", "None of these", "Other", "Burger",
                      "Sushi", "Pizza")
        x
    }, simplify = FALSE)
    names(test.misc[[2]])[2] <- "not sure"
    names(test.misc[[2]])[3] <- "unsure"
    expected.misc <- do.call(cbind, test.misc)
    expected.misc[2:3, 2] <- 0
    expected.misc <- rbind(expected.misc, cbind(c(0, 0), test.misc[[2]][c("not sure", "unsure")]))
    captured.warnings <- capture_warnings(expect_equal(fuzzyMatchRowNames(test.misc,
                                                                          ignore.unmatched = TRUE,
                                                                          warn = TRUE),
                                                       expected.misc))

    warn.msg <- paste0("Multiple fuzzy matches found with rows named 'Don't know', ",
                       "'not sure', 'unsure'. Considering merging these categories ",
                       "if they are similar measures.")
    output.msg <- paste0("After a fuzzy matching search there are still names that ",
                         "couldn't be matched without ambiguity. These had the names ",
                         "'Don't know', 'None of these', 'not sure', 'unsure'. ",
                         "Consider merging these categories if appropriate or relaxing ",
                         "the matching options to ignore them beforing proceeeding further.")
    expect_error(expect_warning(fuzzyMatchRowNames(test.misc, ignore.unmatched = FALSE, warn = FALSE),
                                warn.msg),
                 output.msg)
    expect_error(fuzzyMatchRowNames(test.misc, ignore.unmatched = FALSE, warn = FALSE),
                 output.msg)
    ## Near match tests
    test.dist <- replicate(2, runif(7), simplify = FALSE)
    names(test.dist[[1]]) <- c("Displayr", "qu", "burger", "Ham", "Stew", "kitten", "Honda")
    # Spelling errors
    names(test.dist[[2]]) <- c("displayer", "Q", "Berger", "yam", "stew", "sitten", "Hyundai")
    expected.out <- do.call(cbind, test.dist)
    expected.out[7, 2] <- 0
    expected.out <- rbind(expected.out, c(0, unname(test.dist[[2]][7])))
    rownames(expected.out)[8] <- "Hyundai"
    expect_equal(fuzzyMatchRowNames(test.dist, ignore.unmatched = TRUE), expected.out)
    ## Ambiguous fuzzy matches, throw a warning
    ambiguous <- test.dist
    names(ambiguous[[2L]])[2] <- "displayar"
    expected.ambiguous <- ambiguous
    expected.ambiguous[[1L]] <- append(expected.ambiguous[[1L]], c("displayer" = 0, "displayar" = 0, "Hyundai" = 0))
    expected.ambiguous[[2L]] <- c(0, 0, expected.ambiguous[[2L]][3:6], 0, expected.ambiguous[[2L]][c(1:2, 7)])
    expected.ambiguous <- do.call(cbind, expected.ambiguous)
    expect_equal(fuzzyMatchRowNames(ambiguous, ignore.unmatched = TRUE),
                 expected.ambiguous)
    expect_warning(expect_equal(fuzzyMatchRowNames(ambiguous, ignore.unmatched = TRUE, warn = TRUE),
                                expected.ambiguous),
                   paste0("After a fuzzy matching search there are still names that couldn't ",
                          "be matched without ambiguity. These had the names 'Displayr', ",
                          "'qu', 'Honda', 'displayer', 'displayar', 'Hyundai'. ",
                          "Consider merging these categories if appropriate or relaxing ",
                          "the matching options to ignore them beforing proceeeding further."))
    ## Match the punctuation
    punct.match <- test.dist
    punct.match <- lapply(punct.match, function(x) x[1:6]) # Remove Honda and Hyundai
    punct.match[[1L]] <- append(punct.match[[1L]], c("FILET-O-FISH" = runif(1),
                                                     "Toys'R'Us" = runif(1),
                                                     "Young @ Heart" = runif(1)))
    punct.match[[2L]] <- append(punct.match[[2L]], c("Filet'o'fish" = runif(1),
                                                     "ToysRUs" = runif(1),
                                                     "Young@Heart" = runif(1)))
    expected.punct <- do.call(cbind, punct.match)
    expect_equal(fuzzyMatchRowNames(punct.match, ignore.unmatched = TRUE),
                 expected.punct)
    shuffled.punct <- punct.match
    shuffled.punct <- .shuffleSecond(shuffled.punct)
    expect_equal(fuzzyMatchRowNames(shuffled.punct, ignore.unmatched = TRUE),
                 expected.punct)
    expect_equal(matchRows(shuffled.punct, match.elements = "Fuzzy - ignore if unmatched",
                           warn = TRUE),
                 expected.punct)
    expect_equal(matchRows(shuffled.punct, match.elements = "Fuzzy - error if unmatched",
                           warn = TRUE),
                 expected.punct)
})
