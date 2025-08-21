context("expandCharacterSelections")

test_that("expandCharacterSelections expands duplicate row names", {
    m <- matrix(1:12, nrow = 6)
    rownames(m) <- c("A", "B", "B", "C", "A", "D")
    # After checkSelections.character duplicates in the *selection vector* would be removed.
    # We simulate user providing c("A","B") which should expand to all indices with those names.
    sel <- c("A", "B")
    # Pass through checkSelections to mimic real workflow
    sel.valid <- checkSelections(sel, m, 1)
    expanded <- expandCharacterSelections(m, sel.valid, dim = 1)
    expect_true(is.integer(expanded))
    expect_identical(sort(expanded), sort(which(rownames(m) %in% sel)))
    # Direct assertion on order of expansion: indices for "A" (1,5) then "B" (2,3)
    expect_identical(expanded, c(1L, 5L, 2L, 3L))
    expect_length(expanded, 4L)
})

test_that("expandCharacterSelections returns original when names unique", {
    m <- matrix(1:9, nrow = 3)
    rownames(m) <- c("A", "B", "C")
    sel <- c("A", "B")
    sel.valid <- checkSelections(sel, m, 1)
    expanded <- expandCharacterSelections(m, sel.valid, dim = 1)
    # With unique names expansion should just map names -> numeric using which, producing two indices
    expect_identical(expanded, c(1L, 2L))
})

test_that("expandCharacterSelections expands duplicate column names", {
    m <- matrix(1:12, nrow = 3)
    colnames(m) <- c("X", "Y", "Y", "Z")
    sel <- c("Y", "Z")
    sel.valid <- checkSelections(sel, m, 2)
    expanded <- expandCharacterSelections(m, sel.valid, dim = 2)
    expect_true(all(expanded %in% seq_len(ncol(m))))
    expect_identical(sort(expanded), sort(which(colnames(m) %in% sel)))
    # Direct assertion on order: both Y's (2,3) then Z (4)
    expect_identical(expanded, c(2L, 3L, 4L))
})

test_that("expandCharacterSelections leaves numeric selections unchanged", {
    m <- matrix(1:9, nrow = 3)
    rownames(m) <- c("A", "A", "B")
    sel <- c(1, 3)
    sel.valid <- checkSelections(sel, m, 1)
    expanded <- expandCharacterSelections(m, sel.valid, dim = 1)
    expect_identical(expanded, sel.valid)
    expect_true(is.numeric(expanded))
})
