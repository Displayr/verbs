context("First and Last")

library(flipTime)

test_that("First and last without time period", {
    # vector with attribute
    x <- 1:10
    names(x) <- LETTERS[1:10]
    attr(x, "test") <- "test value"
    expect_equal(First(x, 3),
                 structure(1:3, .Names = c("A", "B", "C"), test = "test value"))

    # vector, matches head and tail
    x <- 1:10
    names(x) <- LETTERS[1:10]
    expect_equal(First(x), head(x))
    expect_equal(Last(x, 2), tail(x, 2))

    # matrix, matches head and tail
    m <- matrix(1:20, 4)
    expect_equal(First(m), head(m))
    expect_equal(Last(m, 2), tail(m, 2))

    # matrix with multiple keep, matches head and tail
    m <- matrix(1:20, 4)
    expect_equal(First(m, c(2, 3)), head(m, c(2, 3)))
    expect_equal(Last(m, c(NA, 1)), tail(m, c(NA, 1)))

    # array, matches head and tail
    arr <- array(1:24, dim = c(2,3,4))
    expect_equal(First(arr), head(arr))
    expect_equal(Last(arr, 2), tail(arr, 2))

    # list, matches head and tail
    lst <- list(1, 1:2, 1:3, 1:4)
    names(lst) <- letters[1:4]
    expect_equal(First(lst, 3), head(lst, 3))
    expect_equal(Last(lst, 2), tail(lst, 2))

    # table, matches head and tail
    tbl <- table(letters)
    expect_equal(First(tbl, 3), head(tbl, 3))
    expect_equal(Last(tbl, 2), tail(tbl, 2))

    # function, matches head and tail
    expect_equal(First(colMeans, 3), head(colMeans, 3))
    expect_equal(Last(colMeans, 2), tail(colMeans, 2))

    # keep = 0
    expect_equal(First(1:10, 0), integer(0))
    expect_equal(Last(1:10, 0), integer(0))
    expect_equal(First(matrix(1:20, 4), 0),
                 structure(integer(0), .Dim = c(0L, 5L)))
    expect_equal(First(matrix(1:20, 4), c(0, 0)),
                 structure(integer(0), .Dim = c(0L, 0L)))
    expect_equal(Last(matrix(1:20, 4), 0),
                 structure(integer(0), .Dim = c(0L, 5L),
                           .Dimnames = list(NULL, NULL)))
    expect_equal(Last(matrix(1:20, 4), c(0, 0)),
                 structure(integer(0), .Dim = c(0L, 0L),
                           .Dimnames = list(NULL, NULL)))
})

test_that("Invalid keep", {
    # Invalid keep: matrix
    expect_error(First(1:10, matrix(1:20, 4)),
                 paste0("The input 'keep' needs to be an integer scalar or vector ",
                 "containing the number of entries to keep corresponding to ",
                 "the dimensions of the input data."))

    # Invalid keep: character
    expect_error(First(1:10, "3"),
                 paste0("The input 'keep' needs to be an integer scalar or vector ",
                        "containing the number of entries to keep corresponding to ",
                        "the dimensions of the input data."))

    # Invalid keep: negative value
    expect_error(First(1:10, -1),
                 paste0("The input 'keep' needs to be an integer scalar or vector ",
                        "containing the number of entries to keep corresponding to ",
                        "the dimensions of the input data."))

    # Invalid keep length
    expect_error(First(1:10, 1:2),
                 paste0("The input 'keep' is a vector with more than one ",
                        "value. It needs to be scalar when 'x' is ",
                        "non-dimensioned, e.g., when it is a vector."))
    expect_error(First(matrix(1:20, 4), 1:3),
                 paste0("The input 'keep' is a vector with length greater ",
                        "than the number of dimensions of 'x'. Its length ",
                        "needs to be less than or equal to the number of ",
                        "dimensions of 'x'."))
})

test_that("Invalid by", {
    expect_error(First(1:10, 2, by = "bad value"),
                 paste0("The input 'by' needs to be one of 'element', 'year', ",
                        "'quarter', 'month', 'week', 'day', 'hour', 'minute', ",
                        "'second'."))

    expect_error(First(1:10, 2, by = c("bad value 1", "bad value 2")),
                 paste0("The input 'by' needs to be one of 'element', 'year', ",
                        "'quarter', 'month', 'week', 'day', 'hour', 'minute', ",
                        "'second'."))

    expect_error(First(matrix(1:20, 4), c(2, NA), by = c(NA, "element")),
                 paste0("The input 'by' needs to be one of 'element', 'year', ",
                        "'quarter', 'month', 'week', 'day', 'hour', 'minute', ",
                        "'second' or a vector of these strings (and NA) ",
                        "corresponding to the elements in 'keep'"), fixed = TRUE)
})

test_that("Invalid calendar", {
    expect_error(First(1:10, 2, calendar = 0),
                 "The input 'calendar' needs to be either TRUE or FALSE")
})

test_that("Invalid date names", {
    expect_error(First(1:10, 2, by = "year"),
                 paste0("The duration 'year' cannot be applied as the input ",
                        "data is not labeled with dates."))

    expect_error(First(matrix(1:20, 4), 2, by = "year"),
                 paste0("The duration 'year' cannot be applied as the rows ",
                        "in the input data are not labeled with dates."))

    x <- 1:10
    names(x) <- as.character(AsDateTime("2020-11-03") + 1:10)
    names(x)[5] <- "2020-111-08" # badly formatted date
    expect_error(First(x, 2, by = "year"),
                 paste0("The duration 'year' cannot be applied as the input ",
                        "data is labeled with invalid date(s)."), fixed = TRUE)

    m <- matrix(1:20, 4)
    colnames(m) <- as.character(AsDateTime("2020-11-03") + 1:5)
    colnames(m)[2] <- "2020-111-04" # badly formatted date
    expect_error(First(m, c(NA, 1), by = c(NA, "year")),
                 paste0("The duration 'year' cannot be applied as the ",
                        "columns in the input data are not labeled with ",
                        "dates."))
})

test_that("First and last with time period", {
    x <- 1:900
    names(x) <- AsDateTime("2000-11-07 13:34:56") + (0:899) * 24 * 60 * 60

    x2 <- 1:9000
    names(x2) <- AsDateTime("2000-11-07 13:34:56") + 0:8999 * 60

    x3 <- 1:900
    names(x3) <- AsDateTime("2000-11-07 13:34:56") + 0:899

    x4 <- 1:90
    names(x4) <- paste0("2000-11-07 13:34:5", (3:92) / 10)

    # calendar year
    result <- First(x, 2, "year")
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2001-12-31 13:34:56` = 420L))
    result <- Last(x, 2, "year")
    expect_equal(result[1], c(`2002-01-01 13:34:56` = 421L))
    expect_equal(result[length(result)], x[length(x)])

    # non-calendar year
    result <- First(x, 2, "year", calendar = FALSE)
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2002-11-06 13:34:56` = 730L))
    result <- Last(x, 2, "year", calendar = FALSE)
    expect_equal(result[1], c(`2001-04-26 13:34:56` = 171L))
    expect_equal(result[length(result)], x[length(x)])

    # calendar quarter
    result <- First(x, 3, "quarter")
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2001-06-30 13:34:56` = 236L))
    result <- Last(x, 3, "quarter")
    expect_equal(result[1], c(`2002-10-01 13:34:56` = 694L))
    expect_equal(result[length(result)], x[length(x)])

    # non-calendar quarter
    result <- First(x, 3, "quarter", calendar = FALSE)
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2001-08-06 13:34:56` = 273L))
    result <- Last(x, 3, "quarter", calendar = FALSE)
    expect_equal(result[1], c(`2002-07-26 13:34:56` = 627L))
    expect_equal(result[length(result)], x[length(x)])

    # calendar month
    result <- First(x, 5, "month")
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2001-03-31 13:34:56` = 145L))
    result <- Last(x, 5, "month")
    expect_equal(result[1], c(`2002-12-01 13:34:56` = 755L))
    expect_equal(result[length(result)], x[length(x)])

    # non-calendar month
    result <- First(x, 5, "month", calendar = FALSE)
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2001-04-06 13:34:56` = 151L))
    result <- Last(x, 5, "month", calendar = FALSE)
    expect_equal(result[1], c(`2002-11-26 13:34:56` = 750L))
    expect_equal(result[length(result)], x[length(x)])

    # calendar week
    result <- First(x, 6, "week")
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2000-12-16 13:34:56` = 40L))
    result <- Last(x, 6, "week")
    expect_equal(result[1], c(`2003-03-16 13:34:56` = 860L))
    expect_equal(result[length(result)], x[length(x)])

    # non-calendar week
    result <- First(x, 6, "week", calendar = FALSE)
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2000-12-18 13:34:56` = 42L))
    result <- Last(x, 6, "week", calendar = FALSE)
    expect_equal(result[1], c(`2003-03-15 13:34:56` = 859L))
    expect_equal(result[length(result)], x[length(x)])

    # calendar day
    result <- First(x2, 2, "day")
    expect_equal(result[1], x2[1])
    expect_equal(result[length(result)], c(`2000-11-08 23:59:56` = 2066L))
    result <- Last(x2, 2, "day")
    expect_equal(result[1], c(`2000-11-12 00:00:56` = 6387L))
    expect_equal(result[length(result)], x2[length(x2)])

    # non-calendar day
    result <- First(x2, 2, "day", calendar = FALSE)
    expect_equal(result[1], x2[1])
    expect_equal(result[length(result)], c(`2000-11-09 13:33:56` = 2880L))
    result <- Last(x2, 2, "day", calendar = FALSE)
    expect_equal(result[1], c(`2000-11-11 19:34:56` = 6121L))
    expect_equal(result[length(result)], x2[length(x2)])

    # calendar hour
    result <- First(x2, 5, "hour")
    expect_equal(result[1], x2[1])
    expect_equal(result[length(result)], c(`2000-11-07 17:59:56` = 266L))
    result <- Last(x2, 5, "hour")
    expect_equal(result[1], c(`2000-11-13 15:00:56` = 8727L))
    expect_equal(result[length(result)], x2[length(x2)])

    # non-calendar hour
    result <- First(x2, 5, "hour", calendar = FALSE)
    expect_equal(result[1], x2[1])
    expect_equal(result[length(result)], c(`2000-11-07 18:33:56` = 300L))
    result <- Last(x2, 5, "hour", calendar = FALSE)
    expect_equal(result[1], c(`2000-11-13 14:34:56` = 8701L))
    expect_equal(result[length(result)], x2[length(x2)])

    # calendar minute
    result <- First(x3, 5, "minute")
    expect_equal(result[1], x3[1])
    expect_equal(result[length(result)], c(`2000-11-07 13:38:59` = 244L))
    result <- Last(x3, 5, "minute")
    expect_equal(result[1], c(`2000-11-07 13:45:00` = 605L))
    expect_equal(result[length(result)], x3[length(x3)])

    # non-calendar minute
    result <- First(x3, 5, "minute", calendar = FALSE)
    expect_equal(result[1], x3[1])
    expect_equal(result[length(result)], c(`2000-11-07 13:39:55` = 300L))
    result <- Last(x3, 5, "minute", calendar = FALSE)
    expect_equal(result[1], c(`2000-11-07 13:44:56` = 601L))
    expect_equal(result[length(result)], x3[length(x3)])

    # calendar second
    result <- First(x4, 7, "second")
    expect_equal(result[1], x4[1])
    expect_equal(result[length(result)], c(`2000-11-07 13:34:56.9` = 67L))
    result <- Last(x4, 7, "second")
    expect_equal(result[1], c(`2000-11-07 13:34:53` = 28L))
    expect_equal(result[length(result)], x4[length(x4)])

    # non-calendar second
    result <- First(x4, 7, "second", calendar = FALSE)
    expect_equal(result[1], x4[1])
    expect_equal(result[length(result)], c(`2000-11-07 13:34:57.2` = 70L))
    result <- Last(x4, 7, "second", calendar = FALSE)
    expect_equal(result[1], c(`2000-11-07 13:34:52.3` = 21))
    expect_equal(result[length(result)], x4[length(x4)])
})

test_that("First and last with multi-dimensional time period", {
    m <- matrix(1:20, 4)
    colnames(m) <- as.character(AsDateTime("2020-11-03") + (1:5) * 24 * 60 * 60)
    result <- First(m, 2)
    expect_equal(result, head(m, 2))
    result <- Last(m, 2)
    expect_equal(result, tail(m, 2))

    result <- First(m, c(NA, 2), "day")
    expect_equal(result, m[, 1:2])
    result <- Last(m, c(NA, 2), "day")
    expect_equal(result, m[, 4:5])

    result <- First(m, c(3, 2), c("element", "day"))
    expect_equal(result, head(m[, 1:2], 3))
    result <- Last(m, c(3, 2), c("element", "day"))
    expect_equal(result, tail(m[, 4:5], 3))
})
