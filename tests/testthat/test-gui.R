context("gui")

test_that("gui", {
    # Interface

    win <- start_gui(legacy = TRUE)
    expect_true(is.tkwin(pkg.env$taskWindow))
    quitGroupSeq()

    load_all()
    root <- tcltk::tktoplevel()
    win <- gui(root)
    expect_true(is.tkwin(win))
    tkdestroy(root)


})

