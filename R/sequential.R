#' @title Sequential probabilities
#'
#' @description Compute sequential probabilities based on normally distributed
#' test statistics and assuming independent increments.
#'
#' @param up `numeric` upper bounds
#' @param mean `numeric` the mean
#' @param lo `numeric` lower bounds
#' @param tau `numeric` vector of monotonely increasing information times
#' @param sd `numeric` vector of standard deviations
#' @param hasZbounds `logical` if true, bounds are assumed to be standardized
#' already, otherwise they will be standardized by the function
#' @param exitLast `logical` if TRUE, upper bound at the last stage is set to
#' `Inf`, and lower bound to user-specified upper bound, which means to
#' compute the probability to stay below the upper bounds in all stages except
#' the last stage. In a sequential study design this corresponds to accepting
#' the null hypothesis in stages 1 to n-1 but rejecting it at stage n, i.e.,
#' the probability to exit the study just at stage n.
#' @param alg `algorithm object` to be used for numerical integration (see
#' [mvtnorm::pmvnorm]).
#' @return `numeric` calculated (multivariate) normal probability to stay
#' between bounds, or (if exitLast was set to TRUE) to stay between bounds
#' and exceed upper bound at last stage.
#'
#' @seealso [mvtnorm::pmvnorm()]
#' @import mvtnorm
#' @examples
#' # 1-D
#' pnorm(0)               == seqProb(0)                                   # TRUE
#' pnorm(1, mean=1)       == seqProb(1, mean=1)                           # TRUE
#' pnorm(2, mean=1, sd=2) == seqProb(2, mean=1, sd=2, hasZbounds=FALSE)   # TRUE
#' seqProb(-Inf)                                        # 0
#' seqProb(Inf)                                         # 1
#' seqProb(qnorm(.95))                                  # .95
#' seqProb(qnorm(.95), exitLast=TRUE)                   # 1-.95
#'
#' # Multivariate
#' seqProb(c(Inf, 0)) == seqProb(0)                       # TRUE
#' p <- seqProb(c(2, 1), mean=0)
#' seqProb(c(2, 1), mean=0, tau=c(.25, 0.5)) == p         # TRUE
#' seqProb(c(2, 1), mean=0, tau=c(.25, 1)) == p           # FALSE
#' @export
seqProb <- function(up,
                    lo = rep(-Inf, length(up)),
                    tau = seq_along(up) / length(up),
                    mean = 0, sd = 1 / sqrt(tau),
                    hasZbounds = TRUE,
                    exitLast = FALSE,
                    alg = mvtnorm::Miwa())
{
    # Catch bad input arguments and provide informative error message
    check_input_args <- function() {
        force(up)
        len <- length
        isMonotonelyIncreasing <- function(x) !is.unsorted(x, strictly=TRUE)

        stopifnot(is.numeric(up), len(up) > 0, isTRUE(all(up >= lo)),
                  len(mean) == 1,
                  len(tau) == len(up), isTRUE(all(tau > 0)),
                  isMonotonelyIncreasing(tau),
                  len(sd) == len(up), isTRUE(all(sd > 0)))
    }
    callStr <- format(sys.call(1))

    tryCatch({
            check_input_args()

            if (hasZbounds) {
                zb <- up
                za <- lo
            } else {
                zb <- up / sd
                za <- lo / sd
            }
        },
        error = function(e) stop(callStr, ", ", e, call.=FALSE),
        warning = function(w) stop(callStr, ", ", w, call.=FALSE)
    )

    # Build covariance matrix and check for singularity
    n <- length(zb)
    covMat <- diag(n)
    for (i in 1:n) {
        for (j in i:n) covMat[i, j] <- sd[j] / sd[i]
    }
    covMat[lower.tri(covMat)] <- t(covMat)[lower.tri(covMat)]
    isSingular <- function(x) {
        inherits(tryCatch(solve(x) %*% x, error = function(e)e), "simpleError")
    }
    if (isSingular(covMat) || n > 5) alg <- mvtnorm::GenzBretz()

    integrate <- mvtnorm::pmvnorm
    # The integral under the multivariate normal is computed between bounds:
    #   - b1 b2 ... bn
    #  |
    # -  a1 a2 ... an
    #
    # unless exitLast is set to TRUE, in which case it is:
    #   - b1 b2 ... Inf
    #  |
    # -  a1 a2 ... bn
    if (exitLast) {
        za[n] <- zb[n]
        zb[n] <- Inf
    }

    # Perform integration
    out <- tryCatch(integrate(za, zb, mean/sd, sigma=covMat, algorithm=alg),
                    error = function(e) e)
    hasError <- inherits(out, "simpleError")
    if (hasError) alg <- mvtnorm::GenzBretz()   # fall back to robust algorithm
    out <- integrate(za, zb, mean/sd, sigma=covMat, algorithm=alg)
    as.numeric(out)
}

#' @title Sequential design probabilities
#'
#' @description Computes sequential probabilities, for each stage of a given
#' one-sided sequential design with specified critical limits and optional
#' futility stops (stopping bounds). When applying the design in practice, at
#' each stage, all data collected up to this point is used to calculate a
#' combined test statistic. Then, if the test statistic is larger than the
#' critical limit at the specific stage, the study is stopped and the null
#' hypothesis rejected. If futility stops have been specified and the test
#' statistic falls below them at any stage, the study is also stopped due to
#' futility while the null hypothesis in this case cannot be discarded.
#'
#' @param crit `numeric` vector of critical limits for stopping the study with
#' rejection of the null hypothesis. The length of 'crit' determines the
#' number of stages of the design.
#' @param mu `numeric` the mean (e.g. `mu == 0` under H0). Under H1 this
#' is also known as the drift parameter.
#' @param sd `numeric` standard deviation (or vector of sd)
#' @param tau `numeric` vector of monotonely increasing information times.
#' @param futStop `numeric` vector of futility stopping bounds (optional).
#' @param n `numeric` sample size, which only has an effect if `mu != 0`.
#' Useful to compute expected sample size under H1.
#' @param ... further arguments passed to `seqProb`
#' @return result is a `list` with the following components:
#' * `exitP` the alpha spent at each stage. The cumulative sum of this will
#'   provides the cumulative exit probability up to each stage, that is, the
#'   probability of having stopped due to exceeding a bound.
#' * `undecided` probability of continuing the study. In case of the last
#'   stage, this means stopping without declaring significance.
#' * `cumFutStop` cumulative futility stop probabilities.
#' * `nk` planned sample size per stage.
#' * `kExp` expected sample size per stage - the sum over all values will
#'   provide the total expected sample size.
#'
#' @examples
#' # 1-stage
#' # #######
#' alpha <- 0.05
#' crit <- qnorm(1-alpha)
#' seqDesign(crit)$p$exit                          # type1: 0.05 == alpha
#' seqDesign(crit, mu=0.3)$p$exit                  # power: 9%
#' # Increase sample size
#' seqDesign(crit, mu=0.3, n=69)$p$exit            # power: 80.1%
#'
#' # 2-stage
#' # #######
#' crit2 <- rep(crit, 2)
#'
#' # Using two stages with same critical bounds will exceed alpha
#' cumsum(seqDesign(crit2, mu=0)$p$exit)           # cumulative type1: > 0.08
#' cumsum(seqDesign(crit2, mu=0, n=69)$p$exit)     # type1 does not depend on n
#'
#' # Try Bonferroni:
#' bonf <- rep(qnorm(1-alpha/2), 2)
#' cumsum(seqDesign(bonf)$p$exit)                  # cumulative type1: 0.0415...
#' sum(seqDesign(bonf, mu=0.3, n=69)$p$exit)       # cumulative power: 73.1%
#'
#' # Bonferroni did not use all alpha, so there is room to improve power, for
#' # example using Pocock like boundaries:
#' pocock <- c(1.8662, 1.8848)
#' sum(seqDesign(pocock)$p$exit)                   # cumulative type1: 0.05
#' sum(seqDesign(pocock, mu=.3, n=69)$p$exit)      # power: 75.8%
#'
#' # Still the one stage power will never be reached, which is the price to pay
#' # for the two potential tests. On the other hand, the expected sample size is
#' # considerably smaller in comparison with the 1-stage design.
#' sum(seqDesign(pocock, mu=.3, n=69)$n$nkExp)     # expected sample size: 53
#'
#' # Increasing max sample size to 98, yields expected n = 69 and power 87.8%
#' res <- seqDesign(pocock, mu=.3, n=98)
#' lapply(unlist(res, recursive=FALSE)[c("n.nkExp", "p.exit")], sum)
#' @export
seqDesign <- function(crit,
                      mu = 0, sd = 1,
                      tau = seq_along(crit) / length(crit),
                      futStop = rep(-Inf, length(crit)),
                      n = 1, ...)
{
    K <- length(crit)
    if (missing(sd)) sd <- 1/sqrt(tau*n)

    # Catch bad input arguments and provide informative error message
    check_input_args <- function() {
        len <- length
        force(crit)
        stopifnot(len(crit) == len(futStop),
                  len(crit) == len(tau),
                  len(crit) == len(sd),
                  len(n) == 1)
    }
    callStr <- format(sys.call(1))
    tryCatch(check_input_args(),
        error = function(e) stop(callStr, ", ", e, call.=FALSE),
        warning = function(w) stop(callStr, ", ", w, call.=FALSE)
    )

    # Probabilities are calculated separately for each stage k. Most
    # parameters are identical with only mean, sd, and exitLast varying,
    # depending on the desired probability.
    prob_at_k <- function(k, ...) {
        up <- crit[1:k]
        seqProb(up, mean=mu, lo=futStop[1:k], tau=tau[1:k], sd=sd[1:k], ...)
    }

    exitProbs <- sapply(1:K, FUN = prob_at_k, exitLast=TRUE, ...)
    withinP <- sapply(1:K, FUN = prob_at_k, exitLast=FALSE, ...)
    stopped <- 1 - (cumsum(exitProbs) + withinP)
    nCum = tau*n
    nk = c(nCum[1], diff(nCum))

    # The expected sample size at stage k+1 is the proportion of ongoing
    # subjects at stage k (i.e., P(within) at k) times the planned sample size
    # at stage k+1 (i.e. n_(k+1))
    nkExp <- nk * c(1, head(withinP, n = -1L))

    list(p = list(exit = exitProbs, undecided = withinP, cumFutStop = stopped),
         n = list(nk = nk, nkExp = nkExp))
}

