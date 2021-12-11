
exit_file("Internal gui functions - only tested interactively")

start_gui()
expect_true(inherits(.env, "Dict"))
expect_equal(names(.env), c("name", "par", "par.last", "root"))

expect_true(inherits(.env$at2("root"), "tkwin"))
expect_true(inherits(.env$at2("par"), "Dict"))
expect_true(inherits(.env$at2("par.last"), "Dict"))

expect_true(identical(lapply(as.list(.env$at2("par")), FUN = tclvalue),
                      as.list(.env$at2("par.last"))))
onQuit()
expect_true(.env$is_empty())
