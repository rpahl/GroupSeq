"computeAlphaLevel" <-
function(n, t, t2, lowerBounds, upperBounds, drift, nMax)
{
    # Initialize
    p.stop <-0 #is the probaility of reaching ith analysis and stopping.
    gridSize <- 0.05 # the grid size for numerical integration by trapezoidal rule
    gridVec <- 0 # the grid vector of the most recent joint density calculation
    gridVec.maxLen <- 5000 # the maximum dimension for gridVec

    if(n > nMax) {
        stop(" Number of analyses too large - got ", n, " but allowed are at most ", nMax)
    }

    # Dtandard deviations of increments and process
    sd.inc <- c(sqrt(t2[1]), sqrt(diff(t2)))
    sd.proc <- sqrt(t2)

    # Normalized upper and lower integration limits
    upper <- (upperBounds*sd.proc) - (drift*t)
    lower <- (lowerBounds*sd.proc) - (drift*t)

    # Number of intervals for numerical integration
    gridLen <- abs(trunc((upper-lower) / (gridSize*sd.inc)) + 1)

    if (any(gridLen > gridVec.maxLen)) {
        stop("Too many grid points - got ", max(gridLen, na.rm=TRUE),
             " but allowed are at most ", gridVec.maxLen)
    }

    # Calculate probabilities
    # -----------------------
    # One-dimensional probs at t[1] are calculated using standard stats::pnorm
    ndrift <- drift*t[1]/sd.inc[1]  # normalized drift
    p.stop <- 1 - (pnorm(upperBounds[1] - ndrift) - pnorm(lowerBounds[1] - ndrift))
    p.exit.upper <- 1 - pnorm(upperBounds[1] - ndrift)
    p.exit.lower <- pnorm(lowerBounds[1] - ndrift)

    # Multidimensional probabilities require numerical integration
    if(n>=2) {
        for (i in 2:n) {
            if(i==2) {
                # Density of process at first look
                gridVec <- jointDensity(1, lower[1], upper[1], sd.inc[1], gridLen, gridVec)
            }

            res <- computeCurrentProbability(gridVec, gridLen, lower, upper,i,gridSize, sd.inc[i])
            p.stop[i] <- res[[1]]
            p.exit.upper[i] <- res[[2]]
            p.exit.lower[i] <- res[[3]]
            p.stop[i] <- 1 - p.stop[i]

            # If density needed for next step, compute it with parameter i
            if (i != n) {
                gridVec <- jointDensity(i, lower, upper, sd.inc[i], gridLen, gridVec)
            }
        }
    }

    p.total <- sum(p.exit.upper) + sum(p.exit.lower)

    list(probAndStop=p.stop, probAndExceedingUpper=p.exit.upper,
         probAndExceedingLower=p.exit.lower, expectedStoppingTime=0,
         totalTypeOneError=p.total)
}
