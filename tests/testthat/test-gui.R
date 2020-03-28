context("gui")

test_that("gui", {
    # Interface
    load_all()

    win <- gui("tcltk")
    expect_equal(getOption("guiToolkit"), "tcltk")
    dispose(win)

    win <- gui("RGtk2")
    expect_equal(getOption("guiToolkit"), "RGtk2")
    dispose(win)
})

