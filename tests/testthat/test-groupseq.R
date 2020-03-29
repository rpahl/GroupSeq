context("groupseq")

test_that("groupseq environment", {
    expect_true(is.environment(gsEnv()))
    expect_error(gsget("foo"), "not found")
    expect_true(is.null(gsget0("foo")))

    gsput("foo", 0)
    expect_equal(gsget("foo"), 0)
    expect_equal(gsget0("foo"), 0)
})

