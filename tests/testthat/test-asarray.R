context("AsArray")

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

testthat("Null", {
    expect_equal(AsArray(null), null)
})