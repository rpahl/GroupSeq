
start_gui()
expect_true(inherits(.env, "Dict"))
expect_equal(.env$keys(), c("root", "par", "par.last", "name"))

expect_true(inherits(.env$get("root"), "tkwin"))
expect_true(inherits(.env$get("par"), "Dict"))
expect_true(inherits(.env$get("par.last"), "Dict"))

expect_true(identical(lapply(as.list(.env$get("par")), FUN = tclvalue),
                      as.list(.env$get("par.last"))))
onQuit()
expect_true(.env$empty())
