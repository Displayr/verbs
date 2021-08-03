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
