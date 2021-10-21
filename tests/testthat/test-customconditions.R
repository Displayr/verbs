context("Custom warning handling")

test_that("Custom warnings have the appropriate structure", {
    # Custom class is a condition
    warning.class <- "foo"
    warning.message  <- "I'm a foo class"
    extra.attr <- list(bar = "baz")
    custom.warning.structure <- createCustomWarningStructure(warning.class,
                                                             message = warning.message,
                                                             some.attr = extra.attr)
    expect_setequal(class(custom.warning.structure), c("foo", "condition"))
    expect_equal(custom.warning.structure[["message"]], warning.message)
    expect_equal(attr(custom.warning.structure, "some.attr"), extra.attr)
    expect_type(custom.warning.structure, "list")
    expect_condition(customWarning(warning.class,
                                   message = warning.message,
                                   some.attr = extra.attr),
                     regex = warning.message,
                     class = warning.class)
})
