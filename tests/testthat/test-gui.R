context("GUI Controls")

test_that("Parsing functions", {
    # non-qtables with attribute dont pass check
    x <- "foo"
    expect_false(checkSpans(x, 1L))
    expect_false(checkSpans(x, 2L))
    x <- array(1:12, dim = 3:4)
    expect_false(checkSpans(x, 1L))
    expect_false(checkSpans(x, 2L))
    attr(x, "has.row.spans") <- attr(x, "has.col.spans") <- TRUE
    expect_false(checkSpans(x, 1L))
    expect_false(checkSpans(x, 2L))
    attr(x, "questions") <- c("Foo", "SUMMARY")
    expect_true(checkSpans(x, 1L))
    expect_true(checkSpans(x, 2L))
    attr(x, "has.col.spans") <- FALSE
    expect_false(checkSpans(x, 2L))
    # Check parseCategoriesToRemove
    exclude.categories <- "  planes, trains and automobiles "
    pta <- trimws(exclude.categories)
    movie.list <- c("spaceballs", "planes, trains and automobiles", "star wars")
    expect_equal(parseCategoriesToRemove(exclude.categories, movie.list), pta)
    larger.movie.list <- c(movie.list, "jurassic park", "saving private ryan",
                           "tenet", "kung fu hustle")
    expected.matches <- sample(larger.movie.list, size = 3L)
    test.string <- paste0(expected.matches, collapse = "; ")
    expect_equal(parseCategoriesToRemove(test.string, larger.movie.list),
                 expected.matches)
    # , works if there are no ; in the text)
    larger.movie.list <- gsub(",", "", larger.movie.list, fixed = TRUE)
    expected.matches <- sample(larger.movie.list, size = 3L)
    test.string <- paste0(expected.matches, collapse = ", ")
    expect_equal(parseCategoriesToRemove(test.string, larger.movie.list),
                 expected.matches)
})

test_that("Warnings about user selections", {
    parsed.string <- c("NET", "SUM")
    test.dimnames <- c("foo", "bar", "baz")
    expect_warning(warnIfUserSelectionHasNoMatch(parsed.string, test.dimnames, has.spans = FALSE, dimension = 1L),
                   NA)
    parsed.string <- "Foo"
    expected <- paste0("The following row label requested to be excluded was not found in the input data: ",
                       "'Foo'. Please supply labels such as 'foo', 'bar', 'baz'.")
    expect_warning(warnIfUserSelectionHasNoMatch(parsed.string, test.dimnames, has.spans = FALSE, dimension = "row"),
                   expected, fixed = TRUE)
    parsed.string <- c("Foo", "Bar")
    expected <- paste0("The following column labels specified to be excluded were not found in the input data: ",
                       "'Foo', 'Bar'. Please supply labels such as 'foo', 'bar', 'baz'.")
    expect_warning(warnIfUserSelectionHasNoMatch(parsed.string, test.dimnames, has.spans = FALSE, dimension = "column"),
                   expected, fixed = TRUE)
})

test_that("Parse categories to remove", {
    input.str <- "foo"
    dummy.in <- replicate(3, setNames(1:3, letters[1:3]), simplify = FALSE)
    expect_error(ParseCategoriesToRemove(input.str, dummy.in), "List of length two required")
    input.str <- c("foo", "bar")
    expect_error(ParseCategoriesToRemove(input.str, dummy.in), "List of length two required")
    input.str <- list(1, 2)
    expect_error(ParseCategoriesToRemove(input.str, dummy.in), "Strings expected in each list element")
    input.str <- list(paste0(letters[1:3], collapse = ", "), "")
    output.str <- list(letters[1:3], NULL)
    expect_equal(ParseCategoriesToRemove(input.str, dummy.in), output.str)
    input.str <- list(paste0(letters[3:4], collapse = ", "), "")
    expect_warning(output <- ParseCategoriesToRemove(input.str, dummy.in),
                   paste0("The following row label requested to be excluded was not found in the input data: ",
                          "'d'. Please supply labels such as 'a', 'b', 'c'."))
})

test_that("Parsing factor levels", {
    # Complete matches OK (no warn)
    generateFactor <- function(labels, n)
        factor(sample(seq_along(labels), size = n, replace = TRUE), labels = labels)
    n <- 100L
    level.list <- list(fact1 = letters[1:3], fact2 = LETTERS[5:8])
    all.levels <- unlist(level.list)
    complete.df <- as.data.frame(lapply(level.list, generateFactor, n = n))
    complete.df <- cbind(complete.df, numeric = runif(n), text = sample(letters, size = n, replace = TRUE))
    complete.df <- complete.df[, sample.int(ncol(complete.df))]
    all.levels <- unname(unlist(Filter(length, lapply(complete.df, levels))))
    delims <- c(",", ";", ", ", "; ")
    # add some empty text as well to check it gets filtered out
    inputs <- lapply(delims, function(x) paste0(sample(c(all.levels, "", "   ")), collapse = x))
    inputs.factor <- lapply(delims, function(x) paste0(sample(c(level.list[[1L]], "", "  ")), collapse = x))
    expected.dat <- mapply(function(txt, delim) Filter(nzchar, trimws(strsplit(txt, delim)[[1L]])),
                           inputs, delims, SIMPLIFY = FALSE)
    expected.factor <- mapply(function(txt, delim) Filter(nzchar, trimws(strsplit(txt, delim)[[1L]])),
                              inputs.factor, delims, SIMPLIFY = FALSE)
    expected.numeric <- NULL
    for (i in seq_along(delims))
    {
        expect_equal(ParseCategoricalLabels(inputs[[i]], complete.df), expected.dat[[i]])
        expect_equal(ParseCategoricalLabels(inputs.factor[[i]], complete.df[["fact1"]]), expected.factor[[i]])
        expect_equal(ParseCategoricalLabels(inputs.factor[[i]], complete.df[["numeric"]]), NULL)
    }
    # Correct partial matches OK (no warn)
    partial.inputs <- lapply(delims, function(x) paste0(sample(all.levels)[1:(length(all.levels)/2)], collapse = x))
    expected <- mapply(function(txt, delim) Filter(nzchar, trimws(strsplit(txt, delim)[[1L]])),
                       partial.inputs, delims, SIMPLIFY = FALSE)
    for (i in seq_along(delims))
        expect_equal(ParseCategoricalLabels(partial.inputs[[i]], complete.df), expected[[i]])
    # Extra levels requested found and warned
    extra.levels <- c("Z", "W", "X")
    input <- c(all.levels, extra.levels)
    inputs <- lapply(delims, function(x) paste0(input, collapse = x))
    expected.output <- all.levels
    for (i in seq_along(inputs))
        expect_warning(expect_equal(ParseCategoricalLabels(inputs[[i]], complete.df), expected.output),
                       paste0("The labels entered in the CATEGORIES TO COUNT section were not found in the input data: ",
                              sQuote(paste0(extra.levels, collapse = paste0(trimws(delims[i]), " "))), "."),
                       fixed = TRUE)
    # If user request not matched, a suggestion is given to that shows the unmatched
    match.ind <- sort(sample.int(length(all.levels))[1:(length(all.levels)/2)])
    matched <- all.levels[match.ind]
    unmatched <- all.levels[-match.ind]
    input <- c(matched, extra.levels)
    inputs <- lapply(delims, function(x) paste0(input, collapse = x))
    expected.output <- matched
    for (i in seq_along(inputs))
        expect_warning(expect_equal(ParseCategoricalLabels(inputs[[i]], complete.df), expected.output),
                       paste0("The labels entered in the CATEGORIES TO COUNT section were not found in the input data: ",
                              sQuote(paste0(extra.levels, collapse = paste0(trimws(delims[i]), " "))), ". ",
                              "Possible labels include ",
                              sQuote(paste0(c(unmatched[1:3], "..."), collapse = paste0(trimws(delims[i]), " "))), "."),
                       fixed = TRUE)
    # Extra information given if the user has delimiter inside the factor labels
    semi.labelled <- c("Oranges; Grapes", "Apples; Bananas")
    comma.labelled <- gsub(";", ",", semi.labelled)
    semi.labelled.df <- data.frame(X = factor(sample(1:2, size = 100L, replace = TRUE), labels = semi.labelled),
                                    Y = runif(100L))
    comma.labelled.df <- data.frame(X = factor(sample(1:2, size = 100L, replace = TRUE), labels =  comma.labelled),
                                     Y = runif(100L))
    #  If no ambiguity then parsing ok
    expect_equal(ParseCategoricalLabels(paste0(semi.labelled, collapse = ", "), semi.labelled.df),
                 semi.labelled)
    expect_equal(ParseCategoricalLabels(paste0(comma.labelled, collapse = "; "), comma.labelled.df),
                 comma.labelled)
    # If ambiguous then give a warning
    expect_warning(expect_equal(ParseCategoricalLabels(paste(semi.labelled, collapse = ";"), semi.labelled.df),
                                character(0)),
                   paste0("It is not possible to unambiguously determine which CATEGORIES TO COUNT while ",
                          "the labels contain the symbol ", sQuote(";")))
    both.labelled <- c("Oranges, apples; grapes", "bananas and pears")
    double.df <- data.frame(X = factor(sample(1:2, size = 100L, replace = TRUE), labels = both.labelled))
    expect_warning(ParseCategoricalLabels(paste(both.labelled, collapse = ", "), double.df),
                   paste0("It is not possible to unambiguously determine which CATEGORIES TO COUNT while ",
                          "the labels contain the symbols ", sQuote(";"), " and ", sQuote(",")))

})

test_that("Levels correctly determined", {
    x <- factor(letters[1:3])
    expect_equal(deduceAllLevels(x), levels(x))
    invalid <- list(1:3, letters[1:3], matrix(1:3), array(1:3))
    for (x in invalid)
        expect_true(is.null(deduceAllLevels(x)))
    x <- list(factor(letters[1:3]),
              data.frame(x = runif(10), y = factor(LETTERS[1:10])),
              1:10,
              factor(letters[1:10]))
    y <- expect_warning(lapply(x, flipTransformations::AsNumeric, binary = FALSE),
                        "Data has been automatically converted to numeric")
    expect_equal(deduceAllLevels(x), c(letters[1:3], LETTERS[1:10], letters[4:10]))
    expect_true(is.null(deduceAllLevels(y)))
})

# No issues with NULL attributes

test_that("DS-3679 Missing row and column span attributes handled", {
    load("table1D.Average.rda")
    test.table.without.span.attributes <- table1D.Average
    attr(test.table.without.span.attributes, "span") <- NULL
    expect_equal(CountEachColumn(table1D.Average),
                 CountEachColumn(test.table.without.span.attributes))
})
