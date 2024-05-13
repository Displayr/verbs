context("AsArray")

test_that("Column array", {
    x <- structure(
        c(1:3),
        foo = "bar"
    )
    expect_equal(AsArray(x), c(1:3))
})

test_that("1D Array", {
    x <- structure(
        array(1:3, dim = c(3)),
        dimnames = list(letters[1:3]),
        foo = "bar"
    )
    exp <- structure(
        array(1:3, dim = c(3)),
        dimnames = list(letters[1:3])
    )
    expect_equal(AsArray(x), exp)
})

test_that("Tables", {
    x <- structure(
        matrix(1:12, nrow = 3),
        dimnames = list(letters[1:3], LETTERS[1:4]),
        foo = "bar"
    )
    exp <- structure(
        matrix(1:12, nrow = 3),
        dimnames = list(letters[1:3], LETTERS[1:4])
    )
    expect_equal(AsArray(x), exp)
})

test_that("3D Array", {
    x <- structure(
        array(1:24, dim = c(2, 3, 4)),
        dimnames = list(letters[1:2], LETTERS[1:3], letters[1:4]),
        foo = "bar"
    )
    exp <- structure(
        array(1:24, dim = c(2, 3, 4)),
        dimnames = list(letters[1:2], LETTERS[1:3], letters[1:4])
    )
    expect_equal(AsArray(x), exp)
})

test_that("4D Array", {
    x <- structure(
        array(1:16, dim = c(2, 2, 2, 2)),
        dimnames = list(letters[1:2], LETTERS[1:2], letters[1:2], LETTERS[1:2]),
        foo = "bar"
    )
    exp <- structure(
        array(1:16, dim = c(2, 2, 2, 2)),
        dimnames = list(letters[1:2], LETTERS[1:2], letters[1:2], LETTERS[1:2])
    )
    expect_equal(AsArray(x), exp)
})

test_that("5D Array", {
    x <- structure(
        array(1:32, dim = c(2, 2, 2, 2, 2)),
        dimnames = list(letters[1:2], LETTERS[1:2], letters[1:2], LETTERS[1:2], letters[1:2]),
        foo = "bar"
    )
    exp <- structure(
        array(1:32, dim = c(2, 2, 2, 2, 2)),
        dimnames = list(letters[1:2], LETTERS[1:2], letters[1:2], LETTERS[1:2], letters[1:2])
    )
    expect_equal(AsArray(x), exp)
})

test_that("Dataframe", {
    x <- structure(
        data.frame(a = 1:3, b = 4:6),
        foo = "bar"
    )
    expect_equal(AsArray(x), list(1:3, 4:6))
})

test_that("Vector", {
    x <- structure(
        1:3,
        foo = "bar"
    )
    expect_equal(AsArray(x), 1:3)
})

test_that("Factor", {
    x <- structure(
        factor(letters[1:3]),
        foo = "bar"
    )
    expect_equal(AsArray(x), 1:3)
})

test_that("Matrix", {
    x <- structure(
        matrix(1:6, nrow = 2),
        foo = "bar"
    )
    expect_equal(AsArray(x), matrix(1:6, nrow = 2))
})

test_that("Null", {
    expect_null(AsArray(NULL))
})