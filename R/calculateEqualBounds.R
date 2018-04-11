"calculateEqualBounds" <-
function(targetAlpha, provisionallyBounds, n, t2)
{
    accuracy <- 10^-7

    # Start values
    meanOfBounds <- sum(provisionallyBounds)/n
    startingBounds <- seq(meanOfBounds, meanOfBounds, length=n)
    lowerBounds <- seq(-8,-8, length=n)

    # Apply the secant method based on Newton Iteration, that is:
    #             x  _  x
    #              k     k-1
    #  x    =  ------------------ * f(x )
    #   k+1     f(x ) - f(x   )        k
    #              k       k-1

    # x_k-1
    xkMinusOne <- startingBounds[1]

    ## fx_k-1
    res <- computeAlphaLevel(n, 0, t2, lowerBounds, startingBounds, 0, 25)
    qpos <- res[[2]]
    qneg <- res[[3]]
    fxkMinusOne <- abs(sum(qpos+qneg) - targetAlpha)

    ## x_k: the more interim analysis we have, the closer we start nearby xkMinusOne
    eps <- if (n <= 10) 10^(-n) else 10^(-10)
    xk <- xkMinusOne - eps

    ## fx_k: for this vectors of equal bounds is required
    xkUpperBounds <- seq(xk, xk, length=n)
    res <- computeAlphaLevel(n, 0, t2, lowerBounds, xkUpperBounds, 0, 25)
    qpos <- res[[2]]
    qneg <- res[[3]]
    fxk <- abs(sum(qpos + qneg) - targetAlpha)

    ## Max 20 iterations - if not finished by then, probably never converges
    for (j in 1:20) {
        xkPlusOne <- xk - ( (xk - xkMinusOne)/(fxk - fxkMinusOne) * fxk )
        if(is.nan(xkPlusOne) || is.infinite(xkPlusOne)) break

        ## for calculating fxkPlusOne we will need a vector of equal bounds
        xkPlusOneUpperBounds <- seq(xkPlusOne, xkPlusOne, length=n)
        res <- computeAlphaLevel(n, 0, t2, lowerBounds, xkPlusOneUpperBounds, 0, 25)
        qpos <- res[[2]]
        qneg <- res[[3]]
        fxkPlusOne <- abs(sum(qpos+qneg) - targetAlpha)

        hasConverged <- fxkPlusOne <= accuracy
        if (hasConverged) {
            ## accuracy is fulfilled - return xkPlusOneBoundVector
            upperBounds <- xkPlusOneUpperBounds
            return(upperBounds)
        } else {
            ## not within tolerance yet - set new values and do next iteration
            xkMinusOne<- xk
            fxkMinusOne <- fxk
            xk <- xkPlusOne
            fxk <- fxkPlusOne
        }
    }

    # If we end up here, iteration did not converge
    cat("!!! Convergence problem while computing exact Pocock Bounds !!!","\n")
    cat("    Computation has been stopped!","\n")
    return(FALSE)
}

