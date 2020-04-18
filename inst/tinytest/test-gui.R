# Interface

win <- start_gui(legacy = TRUE)
expect_true(is.tkwin(pkg.env$taskWindow))
onQuit()

root <- tcltk::tktoplevel()
init_env()
win <- gui(root)
expect_true(is.tkwin(win))
onQuit()


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

