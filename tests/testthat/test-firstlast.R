context("First and Last")

testthat("First and last without time period", {
    # vector
    x <- 1:10
    names(x) <- LETTERS[1:10]
    attr(x, "test") <- "test value"
    expect_equal(First(x, 3), )
    expect_equal(Last(letters, 2), c("y", "z"))

    #
})

testthat("First and last with time period", {

})
