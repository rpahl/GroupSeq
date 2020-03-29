context("gui")

test_that("gui", {
    # Interface
    load_all()

    win <- start_gui(legacy = TRUE)

    tl <- tcltk::tktoplevel()
    win <- gui(tl)
    expect_true(is.tkwin(win))
    tkdestroy(tl)


})

