
exit_file("Internal gui functions - only tested interactively")

start_gui()
expect_equal(names(.env), c("name", "par", "par.last", "root"))

expect_true(inherits(.env[["root"]], "tkwin"))

params = lapply(as.list(.env[["par"]]), FUN = tclvalue)
last_params = as.list(.env[["par.last"]])
expect_true(identical(params, last_params))
onQuit()
expect_true(.env$is_empty())
