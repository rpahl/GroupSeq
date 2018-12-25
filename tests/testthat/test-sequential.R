context("sequential")

test_that("seqProb", {
    # Invalid calls
    expect_error(seqProb(), info='argument "up" is missing, with no default')
    expect_error(seqProb(up=1:3, mean=1:3), info="length of mean must be 1")
    expect_error(seqProb(lo=1, up=-1), info="up >= lo is not TRUE")
    expect_error(seqProb(lo=1:2, up=c(3:5)), info="incompatible bounds")
    expect_error(seqProb(0, sd=-1), info="sd must be > 0")
    expect_error(seqProb(up=1:3, tau=1:2/2), info="incompatible up and tau")

    # 1 dimensional
    expect_equal(seqProb(0), expected = pnorm(0))
    expect_equal(seqProb(0, exitLast=TRUE), expected = pnorm(0))
    set.seed(123)
    z <- rnorm(1)
    expect_equal(seqProb(z), expected = pnorm(z))
    expect_equal(seqProb(z, exitLast=TRUE), expected = 1-seqProb(z))
    p <- .95
    expect_equal(seqProb(qnorm(p)), expected = p)
    expect_equal(seqProb(1, mean=1), expected = pnorm(1, mean=1))
    expect_equal(seqProb(2, mean=1, sd=2), pnorm(2, mean=1/2, sd=1))
    expect_equal(seqProb(2, mean=1, sd=2, hasZbounds=FALSE),
                 pnorm(2, mean=1, sd=2))

    expect_equal(seqProb(-Inf), expected = 0)
    expect_equal(seqProb(-Inf, exitLast=TRUE), expected = 1)
    expect_equal(seqProb(Inf), expected = 1)
    expect_equal(seqProb(Inf, exitLast=TRUE), expected = 0)

    # Multivariate
    expect_equal(seqProb(c(Inf, 0)), expected = seqProb(0))
    expect_equal(seqProb(c(-Inf, 0)), expected = 0)
    set.seed(12345)
    expect_equal(seqProb(c(Inf, Inf, 0),
                         alg=mvtnorm::GenzBretz()),  # standard alg would crash
                 expected = seqProb(0))
    p1 <- seqProb(c(2, 2, 2), mean=0)
    p2 <- seqProb(c(2, 2, 2), mean=1)
    p3 <- seqProb(c(1, 1, 1), mean=1)
    p4 <- seqProb(c(1, 1, 0), mean=1)
    expect_gt(p1, p2)
    expect_gt(p2, p3)
    expect_gt(p3, p4)

    # Make sure it can handle singular covariance matrices
    up <- rep(2, 3)
    singularCovVarCausingSD <- rep(1, 3)
    expect_is(seqProb(up, mean=0, sd=singularCovVarCausingSD), class="numeric")

    # Vary tau
    ##########
    crit <- c(2, 2)
    tau <- (1:2)/2

    # If mean (or drift) is 0, scaling tau should have no effect on probability
    p0 <- seqProb(crit, mean=0, tau=tau, exitLast=TRUE)
    expect_equal(p0, seqProb(crit, mean=0, tau=tau*2, exitLast=TRUE))
    expect_equal(p0, seqProb(crit, mean=0, tau=tau/3, exitLast=TRUE))

    # In contrast, for non-zero mean, a scale-effect is expected, which
    # basically can be interpreted in terms of in-/decreasing power by
    # in-/decreasing the sample size
    p1 <- seqProb(crit, mean=1, tau=tau, exitLast=TRUE)
    expect_lt(p1, seqProb(crit, mean=1, tau=tau*2, exitLast=TRUE)) # doubled sample size
    expect_gt(p1, seqProb(crit, mean=1, tau=tau/3, exitLast=TRUE)) # 1/3 of sample size

    # For non non-linear scaling, an effect is expected as well
    expect_false(p0 == seqProb(crit, mean=0, tau=tau*c(1, 2), exitLast=TRUE))
})

test_that("Compare seqProb with reference software by Lan-DeMets", {
    # Reference values were calculated using Lan-DeMets software - see:
    # https://www.biostat.wisc.edu/sites/default/files/lan-demets/WinLD.zip
    # All results were obtained using Compute -> Probability, One-Sided and
    # manually entered test boundaries (i.e. no alpha spending function).

    # The calculated reference probabilites (lanDeMets) are found in the
    # rightmost column ('Cum exit pr') of the result table of the software.
    # The following function will perform the corresponding calculations based
    # on seqProb:
    calculate_Cum_exit_pr <- function(Time, upperBound,
                                      lowerBound=rep(-Inf, length(upperBound)), ...) {
        p <- numeric(length(Time))
        for (i in seq_along(Time)) {
            p[i] <- seqProb(upperBound[1:i], tau=Time[1:i], lo=lowerBound[1:i],
                            exitLast=TRUE, ...)
        }
        cumsum(p)
    }

    # Do some 4-stage calculations
    times <- c(0.25, 0.50, 0.75, 1.00)
    bounds <- c(4, 3, 2, 1)
    cumP <- calculate_Cum_exit_pr(times, bounds)
    lanDeMets <- c(0.00003, 0.00137, 0.02294, 0.15934)
    expect_equal(cumP, lanDeMets, tol = 1e-4)

    # Use drift = 2
    drift <- 2
    cumP <- calculate_Cum_exit_pr(times, bounds, mean=drift)
    lanDeMets <- c(0.00135, 0.05660, 0.39529, 0.84180)
    expect_equal(cumP, lanDeMets, tol = 1e-4)

    # Non-standard information times (instead of equally spaced)
    times <- c(.1, .2, .4, .7)
    cumP <- calculate_Cum_exit_pr(times, bounds)
    lanDeMets <- c(0.00003, 0.00137, 0.02321, 0.16175)
    expect_equal(cumP, lanDeMets, tol = 1e-4)

    # Again with drift
    drift <- 1.5
    cumP <- calculate_Cum_exit_pr(times, bounds, mean=drift)
    lanDeMets <- c(0.00021, 0.00999, 0.14786, 0.60399)
    expect_equal(cumP, lanDeMets, tol = 1e-4)
})


test_that("Trivial 1-stage 'sequential' designs", {
    # One stage designs represent the special (actually non-sequential) case
    alpha <- 0.05
    crit <- qnorm(1 - alpha)

    # Invalid calls
    expect_error(seqDesign(), info='crit is missing')
    expect_error(seqDesign(as.logical(crit)), info='crit must be numeric')
    expect_error(seqDesign(crit, tau=1:2/2), info="incompatible crit and tau")
    expect_error(seqDesign(crit, sd=1/sqrt(1:2)), info="incompatible crit and sd")
    expect_error(seqDesign(1:2, tau=2:1/2), info="tau not increasing")
    expect_error(seqDesign(crit, futStop=crit+0.1),
                 info="Futility stop must not exceed critical limit")

    result <- seqDesign(crit)
    expect_equal(result$p$exit, alpha)
    expect_equal(sum(unlist(result$p)), 1)

    # Set mean equal to critical limit, which means
    # z = x - mean = crit - crit = 0, which gives power of 0.5
    expect_equal(seqDesign(crit, mu=crit)$p$exit, 0.5)

    # If mean < critical limit, power falls below 0.5 ...
    pow <- seqDesign(crit, mu=crit/2)$p$exit
    expect_lt(pow, 0.5)
    # ... but increases if std dev decreases under H1
    expect_gt(seqDesign(crit, mu=crit/2, sd=1/sqrt(2))$p$exit, pow)

    # If mean > critical limit, power rises above 0.5 ...
    pow2 <- seqDesign(crit, mu=crit*2)$p$exit
    expect_gt(pow2, 0.5)
    # ... but decreases if std dev increases under H1
    expect_lt(seqDesign(crit, mu=crit*2, sd=2)$p$exit, pow2)
})

test_that("Multi-stage sequential designs consistency checks", {
    isIncreasing <- function(x) !is.unsorted(x)
    isDecreasing <- function(x) !is.unsorted(rev(x))

    # Design under H0 (i.e. mu == 0)
    crit <- rep(2, 4)
    d0 <- seqDesign(crit)
    expect_equal(length(d0$p$exit), length(crit))
    expect_true(isDecreasing(d0$p$exit))
    expect_true(isDecreasing(d0$undecided))
    # Verify that alpha under H0 does not depend on n
    expect_equal(d0$p$exit, seqDesign(crit, n=100)$p$exit)

    # Design under H1
    d1 <- seqDesign(crit, mu=1)
    expect_true(all(d1$p$undecided < d0$p$undecided))
    expect_true(isDecreasing(d1$p$undecided))

    expect_equal(d1, seqDesign(crit, mu=1, n=1))
    # Under H1, the overall power should increase with increasing n
    expect_gt(sum(seqDesign(crit, mu=1, n=5)$p$exit), sum(d1$p$exit))

    dfs0 <- seqDesign(crit, mu=0, futStop=rep(0, 4))
    expect_true(isIncreasing(dfs0$cumFutStop))
    dfs1 <- seqDesign(crit, mu=1, futStop=rep(0, 4))
    expect_true(isIncreasing(dfs1$cumFutStop))

    # Due to added futility stops, the following conditions should hold:
    expect_true(all(dfs0$p$exit[-1] < d0$p$exit[-1]))
    expect_true(all(dfs1$p$exit[-1] < d1$p$exit[-1]))
    expect_true(all(dfs0$p$undecided < d0$p$undecided))
    expect_true(all(dfs1$p$undecided < d1$p$undecided))

    df <- as.data.frame(dfs0$p)
    df$cumExit <- cumsum(df$exit)
    expect_true(all(rowSums(df[c("cumExit", "undecided", "cumFutStop")]) == 1))
    df <- as.data.frame(dfs1$p)
    df$cumExit <- cumsum(df$exit)
    expect_true(all(rowSums(df[c("cumExit", "undecided", "cumFutStop")]) == 1))
})

test_that("Compare 2-stage 'sequential' designs with LanDeMets software", {
    # Reference values were calculated using Lan-DeMets software - see:
    # https://www.biostat.wisc.edu/sites/default/files/lan-demets/WinLD.zip
    # All results were obtained using Compute -> Probability, One-Sided and
    # manually entered test boundaries (i.e. no alpha spending function).

    # The calculated reference probabilities (lanDeMets) are found as the
    # cumulative exit probability (rightmost column "Cum exit pr") in the
    # result table of the software.

    # Standard case
    cumAlpha <- cumsum(seqDesign(crit=c(2, 2))$p$exit)
    lanDeMets <- c(0.02275, 0.03799)
    expect_equal(cumAlpha, lanDeMets, tol = 1e-4)

    # Vary tau
    tau <- c(.2, .5)
    cumAlpha <- cumsum(seqDesign(crit=c(2, 2), tau=tau)$p$exit)
    lanDeMets <- c(0.02275, 0.03945)
    expect_equal(cumAlpha, lanDeMets, tol = 1e-4)
    tau <- c(.2, .75)
    cumAlpha <- cumsum(seqDesign(crit=c(2, 2), tau=tau)$p$exit)
    lanDeMets <- c(0.02275, 0.04123)
    expect_equal(cumAlpha, lanDeMets, tol = 1e-4)

    # Incorporate drift (power calculations)
    drift <- 1
    cumAlpha <- cumsum(seqDesign(crit=c(2, 2), mu=drift)$p$exit)
    lanDeMets <- c(0.09802, 0.19556)
    expect_equal(cumAlpha, lanDeMets, tol = 1e-4)

    drift <- 2
    cumAlpha <- cumsum(seqDesign(crit=c(2, 2), mu=drift)$p$exit)
    lanDeMets <- c(0.27901, 0.53892)
    expect_equal(cumAlpha, lanDeMets, tol = 1e-4)

    drift <- 3
    cumAlpha <- cumsum(seqDesign(crit=c(2, 2), mu=drift,
                                 tau=c(.3, .8))$p$exit)
    lanDeMets <- c(0.36061, 0.77441)
    expect_equal(cumAlpha, lanDeMets, tol = 1e-4)
})


