context("AsArray")

testthat("Column array", {
    x <- structure(
        c(1:3), 
        foo = "bar"
    )
    expect_equal(AsArray(x), c(1:3))
})

testthat("1D Array", {
    x <- structure(
        array(1:3, dim = c(1)), 
        dimnames = list("a"), 
        foo = "bar"
    )
    exp <- structure(
        array(1:3, dim = c(1)), 
        dimnames = list("a")
    )
    expect_equal(AsArray(x), exp)
})

testthat("Tables", {
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

testthat("3D Array", {
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

testthat("4D Array", {
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

testthat("5D Array", {
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

testthat("Dataframe", {
    x <- structure(
        data.frame(a = 1:3, b = 4:6), 
        foo = "bar"
    )
    expect_equal(AsArray(x), data.frame(a = 1:3, b = 4:6))
})

testthat("Vector", {
    x <- structure(
        1:3,
        foo = "bar"
    )
    expect_equal(AsArray(x), 1:3)
})

testthat("Factor", {
    x <- structure(
        factor(letters[1:3]),
        foo = "bar"
    )
    expect_equal(AsArray(x), factor(letters[1:3]))
})

testthat("Null", {
    expect_null(AsArray(NULL))
})