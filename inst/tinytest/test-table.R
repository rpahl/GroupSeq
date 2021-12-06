
exit_file("Internal functions only tested interactively")

t <- tktoplevel()
frame <- tkframe(t)
data <- as.data.frame(matrix(1:9, nrow = 3, ncol = 3))

tab <- create_table(frame, dims = dim(data), name = "test")
tkpack(tab)
tkpack(frame)

for (i in seq_len(nrow(data))) {
    for (j in seq_len(ncol(data))) {
        val <- data[i, j]
        set_cell_value(tab, i, j, val)
        entry <- get_cell_value(tab, i, j)
        expect_equal(as.character(val), entry)
    }
}

# Verify table-wise disable works as expected
tkconfigure(tab, state = "disabled", bg = "green")
for (i in seq_len(nrow(data))) {
    for (j in seq_len(ncol(data))) {
        val <- data[i, j]
        set_cell_value(tab, i, j, val + 10)
        entry <- get_cell_value(tab, i, j)
        expect_equal(as.character(val), entry)
    }
}

# Verify table-wise re-enable works as expected
tkconfigure(tab, state = "normal", bg = "green")
for (i in seq_len(nrow(data))) {
    for (j in seq_len(ncol(data))) {
        val <- data[i, j]
        set_cell_value(tab, i, j, val + 10)
        entry <- get_cell_value(tab, i, j)
        expect_equal(as.character(val + 10), entry)
    }
}


data2 <- as.data.frame(matrix(15:20, nrow = 2, ncol = 3))
tkconfigure(tab, rows = nrow(data2) + 1, cols = ncol(data2))

for (i in seq_len(nrow(data2))) {
    for (j in seq_len(ncol(data2))) {
        val <- data2[i, j]
        set_cell_value(tab, i, j, val)
        entry <- get_cell_value(tab, i, j)
        expect_equal(as.character(val), entry)
    }
}

tkdestroy(t)

