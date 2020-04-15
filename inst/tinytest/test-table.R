
t <- tktoplevel()
frame <- tkframe(t)
data <- as.data.frame(matrix(1:9, nrow = 3, ncol = 3))

tab <- create_table(frame)
fill_table(tab, data)
tkpack(tab)
tkpack(frame)
tkconfigure(tab, state = "disabled")

for (i in seq_len(nrow(data))) {
    for (j in seq_len(ncol(data))) {
        entry <- get_cell_value(tab, i, j)
        expect_equal(as.character(data[i, j]), entry)
    }
}

tkconfigure(tab, state = "normal")
data <- as.data.frame(matrix(15:20, nrow = 2, ncol = 3))
for (i in seq_len(nrow(data))) {
    for (j in seq_len(ncol(data))) {
        set_cell_value(tab, i, j, data[i, j])
        entry <- get_cell_value(tab, i, j)
        expect_equal(as.character(data[i, j]), entry)
    }
}

tkdestroy(t)
