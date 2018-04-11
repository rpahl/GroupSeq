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

test_that("computeAlphaLevel", {
    alpha <- 0.05
    n <- 1
    t <- (1:n)/n
    t2 <- (1:n)/n
    lo <- -8
    drift <- 0
    nMax <- 25

    up <- qnorm(1-alpha)
    res <- computeAlphaLevel(n, t, t2, lo, up, drift, nMax)

    expect_equal(res$probAndStop, alpha)
    expect_equal(res$probAndExceedingUpper, alpha)
    expect_equal(res$probAndExceedingLower, 0, tol=1e-10)
    expect_equal(res$expectedStoppingTime, 0)
    expect_equal(res$totalTypeOneError, alpha)

    n <- 4
    t <- 0
    t2 <- (1:n)/n
    lo <- rep(-8, n)
    up <- rep(2.06739, n)
    res <- computeAlphaLevel(n, t, t2, lo, up, drift, nMax)
    expect_equal(res$probAndStop, c(0.0193487, 1, 1, 1))
    expect_equal(res$probAndExceedingUpper,
                 c(0.019348701, 0.013193548, 0.009750731, 0.007707020))
    expect_equal(res$probAndExceedingLower, rep(0, n), tol=1e-10)
    expect_equal(res$expectedStoppingTime, 0)
    expect_equal(res$totalTypeOneError, alpha, tol=1e-7)

    t <- (1:n)/n
    up <- c(4.332634, 2.963129, 2.35902, 2.014057)
    lo <- -up
    drift <- 0.5
    res <- computeAlphaLevel(n, t, t2, lo, up, drift, nMax)
    expect_equal(res$probAndStop, c(2.455984e-05, 1, 1, 1))
    expect_equal(res$probAndExceedingUpper,
                 c(2.226406e-05, 4.516532e-03, 2.356205e-02, 4.300024e-02))
    expect_equal(res$probAndExceedingLower,
                 c(2.295776e-06, 4.541068e-04, 2.341142e-03, 4.281890e-03))
    expect_equal(res$totalTypeOneError, 0.07818052, tol=1e-7)
})

