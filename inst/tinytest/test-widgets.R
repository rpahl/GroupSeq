exit_file("Internal gui functions - only tested interactively")

# create_combobox  TODO:


# create_numeric_entry TODO:


# create_radiobutton
root <- tcltk::tktoplevel()
dummy <- container::dict()
dummy$add("foo", tclVar("1"))
rb1 <- create_radiobutton(root, "foo", value = 1, register = dummy)
rb2 <- create_radiobutton(root, "foo", value = 2, register = dummy)
tkgrid(rb1, rb2)

expect_equal(tclvalue(tcl(rb1, "state")), "selected")
expect_equal(tclvalue(tcl(rb2, "state")), "")
expect_equal(tclvalue(dummy[["foo"]]), "1")
tclvalue(dummy[["foo"]]) <- 2
expect_equal(tclvalue(tcl(rb1, "state")), "")
expect_equal(tclvalue(tcl(rb2, "state")), "selected")
tkdestroy(root)

