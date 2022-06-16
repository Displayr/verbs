context("Subscripting QTables")

arrayAsTable <- function(dims, dimnames = NULL) {
    if (missing(dims))
        stop("dims argument required")
    output <- array(sample(1:100, size = prod(dims), replace = TRUE), dim = dims, dimnames = dimnames)
    class(output) <- c("QTable", class(output))
    output
}

set.seed(12321)
table.env <- new.env()
# Very simple arrays
x.1 <- arrayAsTable(1)
x.1.named <- arrayAsTable(1, list("Foo"))
# Typical 1D Array
x.5 <- arrayAsTable(5)
x.5.named <- arrayAsTable(5, list(LETTERS[1:5]))
# Typical 2D array
x.2.3 <- arrayAsTable(2:3)
x.2.3.named <- arrayAsTable(2:3, list(letters[1:2], LETTERS[1:3]))
x.2.3.4 <- arrayAsTable(2:4)
x.2.3.4.named <- arrayAsTable(2:4, list(letters[1:2], LETTERS[1:3], rev(letters)[1:4]))
x.2.3.4.5 <- arrayAsTable(2:4)
x.2.3.4.5.named <- arrayAsTable(2:5, list(letters[1:2], LETTERS[1:3], rev(letters)[1:4], rev(LETTERS)[1:5]))
x.2.3.4.5.6 <- arrayAsTable(2:6)
x.2.3.4.5.6.named <- arrayAsTable(2:6,
                                  list(letters[1:2], LETTERS[1:3], rev(letters)[1:4], rev(LETTERS)[1:5], NULL))

tables.wont.drop <- list(x.1, x.1.named, x.5, x.5.named, x.2.3, x.2.3.named, x.2.3.4, x.2.3.4.named,
                         x.2.3.4.5, x.2.3.4.5.named, x.2.3.4.5.6, x.2.3.4.5.6.named)

# Redundant arrays (can drop)
x.2.1 <- arrayAsTable(2:1)
x.2.1 <- arrayAsTable(2:1, dimnames = list(LETTERS[1:2], "a"))

test_that("Empty indices passed ok", {
    for (input in tables.wont.drop) {
        expect_equal(input[], input)
        for (drop.arg in c(TRUE, FALSE))
            expect_equal(input[drop = drop.arg], input)
    }
})

test_that("Informative message when user provides incorrect arguments", {
    expect_error(x.2.3.4[1, 2],
                 capture_error(throwErrorTableIndexInvalid(quote(x.2.3.4), 3, 2))[["message"]],
                 fixed = TRUE)

})

test_that("drop recognised and used appropriately", {
    expect_error(x.1[Drop = FALSE], paste0("Only the ", sQuote("drop")))
    for (arg in c(TRUE, FALSE))
        expect_equal(x.1[drop = arg], x.1)
})
