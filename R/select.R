nTables <- function()
    length(ls(pattern = "^formTables[0-9]+$", envir = .GlobalEnv)) - 1

# Subsets a table based on values of combo.boxes
selectFromTableUsingComboBoxes <- function(table, table.orig, combo.boxes, dim = 1)
{
    stopifnot(dim == 1 || dim == 2)

    dim.names <- dimnames(table)[[dim]]
    idx <- which(combo.boxes)
    if (dim == 1L){
        table <- selectFromRows(table, table.orig, idx)
    }else if(dim == 2L)
        table <- selectFromColumns(table, table.orig, idx)
    return(table)
}

selectFromRows <- function (table, table.orig, idx)
{
    n.dims <- length(dim(table))

    if (is.character(idx))
        idx <- checkSuppliedIndices(table, idx, 1)

    if (n.dims == 1){
        table <- table[idx, drop = FALSE]
    }else if (n.dims == 2){
        table <- table[idx, , drop = FALSE]
    }else if (n.dims == 3){
        table <- table[idx, , , drop = FALSE]
    }

    if (hasRowSpan(table.orig))
        table <- updateTableRowSpanAttribute(table, table.orig, idx)
    table <- flipU::CopyAttributes(table, table.orig)
    return(table)
}

selectFromColumns <- function(table, table.orig, idx)
{
    n.dims <- length(dim(table))

    if (is.character(idx))
        idx <- checkSuppliedIndices(table, idx, 2)
    if (length(dim(table)) == 2){
        table <- table[, idx, drop = FALSE]
    }else if (length(dim(table)) == 3)
        table <- table[, idx, , drop = FALSE]

    if (hasColSpan(table.orig))
         table <- updateTableColSpanAttribute(table, table.orig, idx)
    table <- flipU::CopyAttributes(table, table.orig)
    return(table)
}

checkSuppliedIndices <- function(table, indices, dim)
{
    if (is.data.frame(table))
    {
        if (dim == 1)
            names <- names(table)
        else
            names <- rownames(table)
    }else
        names <- dimnames(table)[[dim]]

    bad.idx <- which(!indices %in% names)
    if (length(bad.idx))
    {
        dim.str <- ifelse(dim == 1, "row", "column")
        if (length(bad.idx) == length(names))
            stop("No selections found in the ", dim.str,
                 " labels. If using a page Control, check ",
                 " the item list for the Control and edit if necessary.")

         bad.names <- paste(indices[bad.idx], collapse = ", ")
         warning("The following labels/selections were not found in the ",
                 dim.str, " labels of the table and will be ignored: ", bad.names,
                 ". Please check the item list of the Control being used ",
                 "  for making selections and edit if necessary.")
        return(indices[-bad.idx])
    }
    return(indicies)
}
