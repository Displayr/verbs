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

test_that("QTable: Check elements for opposite Infinities", {
    expect_true(verbs:::checkElementsForOppositeInfinites(c(-Inf, Inf, 1)))
    expect_false(verbs:::checkElementsForOppositeInfinites(c(Inf, -1)))
    expect_false(verbs:::checkElementsForOppositeInfinites(c(-Inf, -1)))
    # Check warning thrown when appropriate
    expect_warning(checkForOppositeInfinites(list(c(Inf, -Inf, 1)), function.name = "Sum"),
                   "'Sum' cannot be computed as the data contains both Inf and -Inf.")
    # Expect only a single warning when many inputs contain Inf and -Inf
    multiple.offenders <- list(x = c(-Inf, Inf), y = c(-Inf, Inf), z = -3:3)
    captured.warnings <- capture_warnings(checkForOppositeInfinites(multiple.offenders,
                                                                            function.name = "Sum"))
    expect_length(captured.warnings, 1L)
    expect_equal(captured.warnings, "'Sum' cannot be computed as the data contains both Inf and -Inf.")
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
    expect_error(removeElementsFromArray(array(1:24, dim = 2:4), function.name = 'Test'),
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
    ## Check outer function that creates the logical indices
    expect_equivalent(table.subsetted <- removeRowsAndCols(table.1D.MultipleStatistics,
                                                                   remove.rows = default.removal,
                                                                   remove.columns = "z-Statistic",
                                                                   warn = TRUE),
                      table.1D.MultipleStatistics[1:3, -5])
    ### Expect attribute to be added to the output showing which rows/columns are removed.
    expect_equal(attr(table.subsetted, "Removed Indices"), list(rows = "SUM", columns ="z-Statistic"))
    expect_equivalent(table.subsetted <- removeRowsAndCols(table.1D.MultipleStatistics,
                                                                   remove.rows = default.removal,
                                                                   remove.columns = NULL,
                                                                   warn = TRUE),
                      table.1D.MultipleStatistics[1:3, ])
    ### Expect attribute to be added to the output showing which rows/columns are removed.
    expect_equal(attr(table.subsetted, "Removed Indices"), list(rows = "SUM", columns = character(0)))
    ## Check the behaviour for multiple inputs
    many.inputs <- list(table2D.Percentage, table2D.PercentageAndCount)
    expect_error(many.subsetted.tables <- lapply(many.inputs, removeRowsAndCols,
                                                 remove.rows = c("Coca-Cola", default.removal),
                                                 remove.columns = c("Never", default.removal),
                                                 warn = TRUE),
                 NA)
    expect_error(many.subsetted.tables.no.attr <- lapply(many.inputs, removeRowsAndCols,
                                                         remove.rows = c("Coca-Cola", default.removal),
                                                         remove.columns = c("Never", default.removal),
                                                         warn = FALSE),
                 NA)
    ### Check attributes, doesn't exist when warnings not requested
    expect_equal(lapply(many.subsetted.tables.no.attr, function(x) attr(x, "Removed Indices")),
                 list(NULL, NULL))
    # Exists when warning requested.
    expect_equal(lapply(many.subsetted.tables, function(x) attr(x, "Removed Indices")),
                 replicate(2, list(rows = "Coca-Cola", columns = c("Never", "NET")), simplify = FALSE))
    # Correct warnings thrown
    index.of.interest <- "some index"
    index.names <- LETTERS[1:3]
    expect_warning(throwWarningAboutRemovedIndices(index.of.interest, index.names),
                   "These categories have been removed from the some index: A, B, C.")
    # Check warnings are adequately thrown
    test.only.row <- structure(1, `Removed Indices` = list(rows = "A"))
    expect_warning(warnAboutRemovedElements(list(test.only.row)),
                   "These categories have been removed from the rows: A.")
    # Duplicates removed and only 1 warning
    test.only.row <- list(structure(1, `Removed Indices` = list(rows = "A")),
                          structure(1, `Removed Indices` = list(rows = "A")))
    index.warnings <- capture_warnings(warnAboutRemovedElements(test.only.row))
    expect_equal(index.warnings, "These categories have been removed from the rows: A.")
    # Test columns and also unique indices put into one warning
    test.only.col <- list(structure(1, `Removed Indices` = list(columns = "B")),
                          structure(1, `Removed Indices` = list(columns = c("B", "C"))))
    index.warnings <- capture_warnings(warnAboutRemovedElements(test.only.col))
    expect_equal(index.warnings, "These categories have been removed from the columns: B, C.")
    # Both rows and cols
    test.rows.and.cols <- list(structure(1, `Removed Indices` = list(rows = "A", columns = "B")),
                               structure(1, `Removed Indices` = list(columns = c("B", "C"))))
    index.warnings <- capture_warnings(warnAboutRemovedElements(test.rows.and.cols))
    expect_equal(index.warnings,
                 c("These categories have been removed from the rows: A.",
                   "These categories have been removed from the columns: B, C."))
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
    ## Check possible filters out ones that arent a statistic
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
                                                                function.name = 'Hello'),
                   paste0("The input data contains statistics of different types (i.e., ",
                          paste0(input.stats, collapse = ", "),
                          "), it may not be ",
                          "appropriate to compute 'Hello'."),
                   fixed = TRUE)
    # Check thrown warnings, single input
    expect_warning(checkForMultipleStatistics(list(table1D.Average), function.name = 'Sum'), NA)
    expect_warning(checkForMultipleStatistics(list(table1D.Percentage), function.name = 'Sum'), NA)
    expect_warning(checkForMultipleStatistics(list(table.1D.MultipleStatistics), function.name = 'Sum'),
                   paste0("The input data contains statistics of different types (i.e., Average, Effective Sample Size, ",
                          "t-Statistic, d.f., z-Statistic, Corrected p), it may not be appropriate to compute 'Sum'."),
                   fixed = TRUE)
    expect_warning(checkForMultipleStatistics(list(table2D.Percentage), function.name = 'Sum'), NA)
    expect_warning(checkForMultipleStatistics(list(table2D.PercentageAndCount), function.name = 'Sum'),
                   paste0("The input data contains statistics of different types (i.e., Row %, Count), it may not be ",
                          "appropriate to compute 'Sum'."),
                   fixed = TRUE)
    expect_warning(checkForMultipleStatistics(list(table2D.PercentageNaN), function.name = 'Sum'), NA)
    # Check thrown warnings, multiple input
    expect_warning(checkForMultipleStatistics(list(table1D.Average, table1D.Average),
                                                      function.name = 'Sum'),
                   NA)
    expect_warning(checkForMultipleStatistics(list(table1D.Average, table1D.Percentage),
                                                      function.name = 'Sum'),
                   paste0("The input data contains statistics of different types (i.e., Average, %), it may not be ",
                          "appropriate to compute 'Sum'."),
                   fixed = TRUE)
    # Check function name correct
    expect_warning(checkForMultipleStatistics(list(table1D.Average, table1D.Percentage),
                                                      function.name = 'Hello'),
                   paste0("The input data contains statistics of different types (i.e., Average, %), it may not be ",
                          "appropriate to compute 'Hello'."),
                   fixed = TRUE)
    # Expect single warning for each dimension
    expect_error(captured.warnings <- capture_warnings(Sum(table1D.Average, table1D.Average, warn = TRUE)), NA)
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
    expect_error(throwErrorContactSupportForRequest("isn't supported. ", "some func"),
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
    weight.test <- runif(n)
    expect_equal(checkWeights(weight.test, n), weight.test)
    ## Check weights with negative elements are set to zero
    invalid.weights <- weight.test
    rand.negatives <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
    invalid.weights[rand.negatives] <- -runif(sum(rand.negatives))
    expect_warning(output <- checkWeights(invalid.weights, n),
                   "Elements with negative weights were set to have weight of zero")
    expect_equal(output[rand.negatives], rep(0, sum(rand.negatives)))
    ## Check weights with missing values are set to zero
    invalid.weights <- weight.test
    rand.negatives <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
    invalid.weights[rand.negatives] <- NA
    expect_warning(output <- checkWeights(invalid.weights, n),
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
    expect_warning(subset.out <- checkSubset(broken.subset, n),
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
    expect_equal(subsetInputs(subset.input, subset.test),
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
                                       function.name = "Test"),
                 rand.in)
    expect_error(subsetAndWeightInputs(rand.in,
                                       subset = c(rep(TRUE, 4), FALSE),
                                       weights = NULL,
                                       function.name = "Test"),
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
})

test_that("Data types checked", {
    data(variable.Date)
    data(variable.Text)
    data(variable.Numeric)
    data(variable.Nominal)
    data(table1D.Average)
    # Expect error for Date and Text variables
    expect_error(checkInputTypes(variable.Date, "Test"),
                 "Date/Time data has been supplied but 'Test' requires numeric data.")
    expect_error(checkInputTypes(variable.Text, "Test"),
                 "Text data has been supplied but 'Test' requires numeric data.")
    # Expect error if a QTable and Variables are both used as inputs
    expect_error(checkInputsDontContainTablesAndVariables(list(variable.Numeric, table1D.Average),
                                                          function.name = "Test"),
                 paste0("'Test' requires input elements to be of the same type. However, ",
                        "both QTables and Variables have been used as inputs. ",
                        "It is not possible to use 'Test' ",
                        "with multiple inputs of different types. ",
                        determineAppropriateContact()))
    ## Check conversion from categorical to numeric occurs
    expect_equal(convertToNumeric(list(variable.Nominal)),
                 list(flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)))
})
