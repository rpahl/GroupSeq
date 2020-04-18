
t <- tktoplevel()
frame <- tkframe(t)
data <- as.data.frame(matrix(1:9, nrow = 3, ncol = 3))

tab <- create_table(frame, dims = dim(data))
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

# Verify disable/enable works as expected
tkconfigure(tab, state = "disabled")
for (i in seq_len(nrow(data))) {
    for (j in seq_len(ncol(data))) {
        val <- data[i, j]
        set_cell_value(tab, i, j, val + 10)
        entry <- get_cell_value(tab, i, j)
        expect_equal(as.character(val), entry)
    }
}

tkconfigure(tab, state = "normal")
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

tkconfigure(tab, state = "normal")
for (i in seq_len(nrow(data))) {
    for (j in seq_len(ncol(data))) {
        val <- data[i, j]
        set_cell_value(tab, i, j, val)
        entry <- get_cell_value(tab, i, j)
        expect_equal(as.character(val), entry)
    }
}

tkdestroy(t)

