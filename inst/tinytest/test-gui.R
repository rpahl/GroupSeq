# Interface

exit_file("Internal gui functions - only tested interactively")

win <- start_gui(legacy = TRUE)
expect_true(tcltk::is.tkwin(pkg.env$taskWindow))
onQuit()

e <- init_env()
win <- gui(e$at2("root"))
expect_true(tcltk::is.tkwin(win))
onQuit()
expect_true(e$is_empty())
rm(e)

if (F) {
    test_gui <- function() {
        devtools::load_all()
        root <- tcltk::tktoplevel()
        win <- gui(root)
    }
    run_tests <- function() {
        devtools::load_all()
        tinytest::run_test_dir(system.file("tinytest", package="GroupSeq"))
    }
    run_tests()

    test_gui()

    start_gui()

}

