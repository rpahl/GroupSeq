context("GroupSeq")

test_that("calculateEqualBounds", {
    n <- 1
    t2 <- (1:n)/n
    bb <- rep(2, n)
    res <- calculateEqualBounds(0.05, bb, n, t2)
    expect_equal(res, qnorm(1 - 0.05))

    n <- 4
    t2 <- (1:n)/n
    bb <- rep(2, n)

    res <- calculateEqualBounds(0.05, bb, n, t2)
    expect_equal(res, rep(2.06739, n), tol=1e-5)

    res <- calculateEqualBounds(0.05/2, bb, n, t2)
    expect_equal(res, rep(2.361263, n), tol=1e-5)

})

