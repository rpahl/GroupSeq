"asHSdeCani" <- function( alpha, tk, oneOrTwoSided, gamma) {
  if ( gamma == 0 ) {
    return (alpha * tk)
  } else {
    return ((alpha/oneOrTwoSided) * ((1 - exp(-gamma * tk)) / (1 - exp(-gamma))))
  }
}


"asOBF" <- function( alpha, tk, oneOrTwoSided ) {
  2 * (1 - stats::pnorm ((stats::qnorm(1 - (alpha / oneOrTwoSided)/2)) / sqrt(tk)))
}


"asPocock" <- function( alpha, tk, oneOrTwoSided ) {
  (alpha/oneOrTwoSided) * log(1 + ( exp(1) - 1 ) * tk)
}


"asPowerFamily" <- function( alpha, tk, oneOrTwoSided, delta ) {
  (alpha/oneOrTwoSided)*(tk^delta)
}


"alphaByUseFunction" <-
function(whatSpendingFunctionIsUsed,n,alpha,phi,OneOrTwoSidedBounds,t)
{
  ###Initialize variables###
  probExit<-0


  #calculate probabilities according to use function

  ###O'Brien and Fleming-type ###
  if (whatSpendingFunctionIsUsed==1)
  {
    for (k in 1:n)
    {
    probExit[k] <- OneOrTwoSidedBounds * asOBF( alpha, t[k], OneOrTwoSidedBounds )
    }
  }

    ### Pocock-type ###
    else if (whatSpendingFunctionIsUsed==2 || whatSpendingFunctionIsUsed==5)
         {
           for (k in 1:n)
           {
             probExit[k] <- OneOrTwoSidedBounds *  asPocock( alpha, t[k], OneOrTwoSidedBounds )
           }
         }

         ### Power Family ###
         else if (whatSpendingFunctionIsUsed==3)
              {
                for (k in 1:n)
                {
                  probExit[k] <- OneOrTwoSidedBounds * asPowerFamily( alpha, t[k], OneOrTwoSidedBounds, phi )
                }
              }

              ### Hwang-Shih-de Cani family ###
              else if (whatSpendingFunctionIsUsed==4)
                   {
                     for (k in 1:n)
                     {
                     probExit[k] <- OneOrTwoSidedBounds * asHSdeCani( alpha, t[k], OneOrTwoSidedBounds, phi )
                     }
                   }

                   ###-----------------------------------------------###
                   ##-- more spending functions could be added here --##
                   ###-----------------------------------------------###
                   else
                   {
                     #if this else-tree is reached something went wrong before
                     print(" Chosen Use Function does not exist", quote=FALSE)
                     print(" or is not implemented correctly yet!", quote=FALSE)
                   }


  ## returns the vector 'probExit' of type I error spent at each analysis
  ## with length 'n'
  return(probExit)


}#end <--*function(...)*


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
    error.msg <- paste0("!!! Convergence problem while computing exact Pocock Bounds !!!\n",
                        "    Computation has been aborted!\n")
    for (j in 1:20) {
        xkPlusOne <- xk - ( (xk - xkMinusOne)/(fxk - fxkMinusOne) * fxk )
        if(is.nan(xkPlusOne) || is.infinite(xkPlusOne)) {
            cat(error.msg)
            return(FALSE)
        }

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
    cat(error.msg)
    return(FALSE)
}


"calculateTask1" <-
function(n,t,t2,equallySpacedTimesInput,secondTimeScaleIsUsedInput, BoundsSymmetry, alpha, phi,
         usedFunction,TruncateBoundsInput, taskWindow)
{
  #Initialize Variables
  lowerBounds<-0 # lowerBounds is the vector of lower standardized boundaries
  upperBounds<-0 # upperBounds is the vector of upper standardized boundaries
  probExit<-0 # probExit is a vector of exit probabilities
  probDifference<-0 # probDifference(i) = probExit(i)-probExit(i-1) where probExit is a vector of exit probabilities

  ##Symmetric bounds - call function computeBounds(...)

    if (!BoundsSymmetry==3)
    {
      usedFunction[2]=usedFunction[1]
      results<- computeBounds(n, 0, alpha[1], phi[1], t, t2, BoundsSymmetry, usedFunction[1], TruncateBoundsInput)
    }

    else
    ## Asymmetric bounds - call function computeBounds(...) twice
    ## first time for upper bounds, second time for lower bounds
    {
      resultsUpperBounds <- computeBounds(n, 0, alpha[1], phi[1], t, t2, 1, usedFunction[1], TruncateBoundsInput)
      resultsLowerBounds <- computeBounds(n, 0, alpha[2], phi[2], t, t2, 1, usedFunction[2], TruncateBoundsInput)
    }

    ## get the values depending on one-sided or two-sided test had been made ##
    ##-- symmetric bounds --##
    if (!BoundsSymmetry==3)
    {
      lowerBounds <- results[[1]]
      upperBounds <- results[[2]]
      probExit <- results[[3]]
      probDifference <- results[[4]]
    }

    ##-- asymmetric bounds --##
    else
    {
      upperBounds <- resultsUpperBounds[[2]]
      lowerBounds <- (-1)*resultsLowerBounds[[2]]
      probExit <- resultsUpperBounds[[3]] + resultsLowerBounds[[3]]
      probDifference <- resultsUpperBounds[[4]] + resultsLowerBounds[[4]]
    }

    ## if (5) Pocock Type - the real Pocock Bounds' was chosen -
    ## we have to do some extra calculations
    ## The Spending function gives us an approximately Pocock-Design.
    ## To compute the exact Pocock Bounds we will do according to the following pattern:
    ## (1st)we give the bounds with all bounds are equal. As starting value we are using the
    ##      mean of the bounds computed by our Pocock spending function. I figured out that
    ##      in almost every case this is a quite good approximation so far.
    ## (2nd)we compute the probability according to our equal bounds, as we would do, if user
    ##      had chosen Task -3- at the beginning
    ## (3rd)we use Newton Iteration to adjust the bounds in every Iteration until we get the appropriate alpha

    if(usedFunction[1]==5 || usedFunction[2]==5)
    {
      ##check for symmetric bounds
      #one-sided
      if(BoundsSymmetry==1)
      {
        upperBounds <- calculateEqualBounds(alpha[1],upperBounds,n,t2)
      }

      ##two-sided symmetric
      else if(BoundsSymmetry==2)
           {
             {
               upperBounds <- calculateEqualBounds(alpha[1]/2,upperBounds,n,t2)
               lowerBounds <- -upperBounds
             }
           }

           else
           ## asymmetric bounds -> maybe we have to calculate 2 times
           {
             ##check where (5) Pocock Type - the real Pocock Bounds was chosen
             if(usedFunction[1]==5)
             {
               upperBounds <- calculateEqualBounds(alpha[1],upperBounds,n,t2)
             }
             if(usedFunction[2]==5)
             {
               lowerBounds <- (-1)*calculateEqualBounds(alpha[2],-lowerBounds,n,t2)
             }
           }

    ##-----------------------------------------------------------##
    ##--Probabilities from bounds, possibly with non zero drift--##
    ##-----------------------------------------------------------##

    vectorOfResults <- computeAlphaLevel(n,t2,t2,lowerBounds,upperBounds,0,25)
    probExceedingUpper <- vectorOfResults[[2]]
    probExceedingLower <- vectorOfResults[[3]]

    ## re-compute exit probability and cumulative exit probability
    probDifference<-0
    probExit<-0

    for(i in 1:n)
    {
      probDifference[i] <- probExceedingUpper[i]+probExceedingLower[i]

      if(i==1)
      {
        probExit[i] <- probDifference[i]
      }
      else
      {
        probExit[i] <- probExit[i-1] + probDifference[i]
      }
    }

  }#end <--*if(whatSpendingFunctionIsUsed[1]==5 || whatSpendingFunctionIsUsed[2]==5)*

    guiOutputTask1(n, alpha, phi, t, lowerBounds, upperBounds, probDifference, probExit,
                   BoundsSymmetry, usedFunction, taskWindow)


}#end <--*function(...)*


"calculateTask2" <-
function(n,nMax,t,t2,t2max,t3,confidenceLevel,drift,equallySpacedTimesInput,secondTimeScaleIsUsed, BoundsSymmetry, alpha, phi, usedFunction,TruncateBoundsInput,
         enterBoundsManually, upperBounds, lowerBounds, taskWindow)

{
  ### did user enter bounds himself? otherwise they must be computed before we can go on with calculating the drift ###
  if(!enterBoundsManually)
  {
    ##Symmetric bounds - call function computeBounds(...)

    if (!BoundsSymmetry==3)
    {
      usedFunction[2]=usedFunction[1]
      results<- computeBounds(n, 0, alpha[1], phi[1], t, t2, BoundsSymmetry, usedFunction[1], TruncateBoundsInput)
    }

    else
    ## Asymmetric bounds - call function computeBounds(...) twice
    ## first time for upper bounds, second time for lower bounds
    {
      resultsUpperBounds <- computeBounds(n, 0, alpha[1], phi[1], t, t2, 1, usedFunction[1], TruncateBoundsInput)
      resultsLowerBounds <- computeBounds(n, 0, alpha[2], phi[2], t, t2, 1, usedFunction[2], TruncateBoundsInput)
    }

    ## get the values depending on one-sided or two-sided test had been made ##
    ##-- symmetric bounds --##
    if (!BoundsSymmetry==3)
    {
      lowerBounds <- results[[1]]
      upperBounds <- results[[2]]
    }

    ##-- asymmetric bounds --##
    else
    {
      upperBounds <- resultsUpperBounds[[2]]
      lowerBounds <- (-1)*resultsLowerBounds[[2]]
    }


    ## if (5) Pocock Type - the real Pocock Bounds' was chosen -
    ## we have to do some extra calculations
    ## The Spending function gives us an approximately Pocock-Design.
    ## To compute the exact Pocock Bounds we will do according to the following pattern:
    ## (1st)we give the bounds with all bounds are equal. As starting value we are using the
    ##      mean of the bounds computed by our Pocock spending function. I figured out that
    ##      in almost every case this is a quite good approximation so far.
    ## (2nd)we compute the probability according to our equal bounds, as we would do, if user
    ##      had chosen Task-3- at the beginning
    ## (3rd)we use Newton Iteration to adjust the bounds in every Iteration until we get the appropriate alpha

    if(usedFunction[1]==5 || usedFunction[2]==5)
    {
      ##check for symmetric bounds
      #one-sided
      if(BoundsSymmetry==1)
      {
        upperBounds <- calculateEqualBounds(alpha[1],upperBounds,n,t2)
      }

      ##two-sided symmetric
      else if(BoundsSymmetry==2)
           {
             {
               upperBounds <- calculateEqualBounds(alpha[1]/2,upperBounds,n,t2)
               lowerBounds <- -upperBounds
             }
           }

           else
           ## asymmetric bounds -> maybe we have to calculate 2 times
           {
             ##check where (5) Pocock Type - the real Pocock Bounds was chosen
             if(usedFunction[1]==5)
             {
               upperBounds <- calculateEqualBounds(alpha[1],upperBounds,n,t2)
             }
             if(usedFunction[2]==5)
             {
               lowerBounds <- (-1)*calculateEqualBounds(alpha[2],-lowerBounds,n,t2)
             }
           }
    }#end <--*if(whatSpendingFunctionIsUsed[1]==5 || whatSpendingFunctionIsUsed[2]==5)*

   }#end <--*if(!enterBoundsManually)*



  ##--Return drift for given bounds and power.------------------------------------##
  ##-- Note: the drift return produces exit probability confidenceLevel at t[n].--##
  ##--If t(n) < 1 (or t3(n) < 1) this is not the study's power.-------------------##


  drift <- findDrift(n, t3, t2, lowerBounds, upperBounds, confidenceLevel, drift, nMax)


  ##check whether drift was calculated correctly
  if(is.numeric(drift))
  {
    ##we got result - output drift
    vectorOfResults <- computeAlphaLevel(n, t, t2, lowerBounds, upperBounds, drift, nMax)
    probStopping <- vectorOfResults[[1]]
    probExceedingUpper <- vectorOfResults[[2]]
    probExceedingLower <- vectorOfResults[[3]]
    expectedStoppingTime <- vectorOfResults[[4]]
    probTotal <- vectorOfResults[[5]]

    ##output results from function 'computeAlphaLevel'
    guiOutputTask2(n,probTotal,drift,expectedStoppingTime,secondTimeScaleIsUsed,t,t2,t2max,
                   lowerBounds,upperBounds,probStopping,probExceedingUpper,probExceedingLower,
                   confidenceLevel,BoundsSymmetry,enterBoundsManually,alpha,phi,usedFunction, taskWindow)
  }
  else
  {
    ##something went wrong
    tkmessageBox(title="ERROR",message="ERROR WhiLE COMPUTING THE DRIFT!",icon="error",type="ok")
  }



}#end <--*function calculateTask2*


"calculateTask3" <-
function(n,nMax,t,t2,t2max,t3,drift,equallySpacedTimesInput,secondTimeScaleIsUsed, BoundsSymmetry, alpha, phi, usedFunction,TruncateBoundsInput,
         enterBoundsManually, upperBounds, lowerBounds, taskWindow)
{
  ### did user enter bounds himself? otherwise they must be computed before we can go on with calculating the drift ###
  if(!enterBoundsManually)
  {
    ##Symmetric bounds - call function computeBounds(...)

    if (!BoundsSymmetry==3)
    {
      usedFunction[2]=usedFunction[1]
      results<- computeBounds(n, 0, alpha[1], phi[1], t, t2, BoundsSymmetry, usedFunction[1], TruncateBoundsInput)
    }

    else
    ## Asymmetric bounds - call function computeBounds(...) twice
    ## first time for upper bounds, second time for lower bounds
    {
      resultsUpperBounds <- computeBounds(n, 0, alpha[1], phi[1], t, t2, 1, usedFunction[1], TruncateBoundsInput)
      resultsLowerBounds <- computeBounds(n, 0, alpha[2], phi[2], t, t2, 1, usedFunction[2], TruncateBoundsInput)
    }

    ## get the values depending on one-sided or two-sided test had been made ##
    ##-- symmetric bounds --##
    if (!BoundsSymmetry==3)
    {
      lowerBounds <- results[[1]]
      upperBounds <- results[[2]]
    }

    ##-- asymmetric bounds --##
    else
    {
      upperBounds <- resultsUpperBounds[[2]]
      lowerBounds <- (-1)*resultsLowerBounds[[2]]
    }


    ## if (5) Pocock Type - the real Pocock Bounds' was chosen -
    ## we have to do some extra calculations
    ## The Spending function gives us an approximately Pocock-Design.
    ## To compute the exact Pocock Bounds we will do according to the following pattern:
    ## (1st)we give the bounds with all bounds are equal. As starting value we are using the
    ##      mean of the bounds computed by our Pocock spending function. I figured out that
    ##      in almost every case this is a quite good approximation so far.
    ## (2nd)we compute the probability according to our equal bounds, as we would do, if user
    ##      had chosen Task-3- at the beginning
    ## (3rd)we use Newton Iteration to adjust the bounds in every Iteration until we get the appropriate alpha

    if(usedFunction[1]==5 || usedFunction[2]==5)
    {
      ##check for symmetric bounds
      #one-sided
      if(BoundsSymmetry==1)
      {
        upperBounds <- calculateEqualBounds(alpha[1],upperBounds,n,t2)
      }

      ##two-sided symmetric
      else if(BoundsSymmetry==2)
           {
             {
               upperBounds <- calculateEqualBounds(alpha[1]/2,upperBounds,n,t2)
               lowerBounds <- -upperBounds
             }
           }

           else
           ## asymmetric bounds -> maybe we have to calculate 2 times
           {
             ##check where (5) Pocock Type - the real Pocock Bounds was chosen
             if(usedFunction[1]==5)
             {
               upperBounds <- calculateEqualBounds(alpha[1],upperBounds,n,t2)
             }
             if(usedFunction[2]==5)
             {
               lowerBounds <- (-1)*calculateEqualBounds(alpha[2],-lowerBounds,n,t2)
             }
           }
    }#end <--*if(whatSpendingFunctionIsUsed[1]==5 || whatSpendingFunctionIsUsed[2]==5)*

   }#end <--*if(!enterBoundsManually)*



  ##-----------------------------------------------------------##
  ##--Probabilities from bounds, possibly with non zero drift--##
  ##-----------------------------------------------------------##
  vectorOfResults <- computeAlphaLevel(n,t,t2,lowerBounds,upperBounds,drift,nMax)
  probStopping <- vectorOfResults[[1]]
  probExceedingUpper <- vectorOfResults[[2]]
  probExceedingLower <- vectorOfResults[[3]]
  expectedStoppingTime <- vectorOfResults[[4]]
  probTotal <- vectorOfResults[[5]]

  ##output results from function 'computeAlphaLevel'
  guiOutputTask3(n,probTotal,drift,expectedStoppingTime,secondTimeScaleIsUsed,t,t2,t2max,
                 lowerBounds,upperBounds,probStopping,probExceedingUpper,probExceedingLower,
                 BoundsSymmetry,enterBoundsManually,alpha,phi,usedFunction, taskWindow)




}#end <--*function calculateTask3*


"calculateTask4" <-
function(n,nMax,t,t2,t2max,t3,confidenceLevel,equallySpacedTimesInput,secondTimeScaleIsUsed, BoundsSymmetry, alpha, phi, usedFunction,TruncateBoundsInput,
         enterBoundsManually, upperBounds, lowerBounds, Zvalue, taskWindow)
{
  ### did user enter bounds himself? otherwise they must be computed before we can go on with calculating the drift ###
  if(!enterBoundsManually)
  {
    ##Symmetric bounds - call function computeBounds(...)

    if (!BoundsSymmetry==3)
    {
      usedFunction[2]=usedFunction[1]
      results<- computeBounds(n, 0, alpha[1], phi[1], t, t2, BoundsSymmetry, usedFunction[1], TruncateBoundsInput)
    }

    else
    ## Asymmetric bounds - call function computeBounds(...) twice
    ## first time for upper bounds, second time for lower bounds
    {
      resultsUpperBounds <- computeBounds(n, 0, alpha[1], phi[1], t, t2, 1, usedFunction[1], TruncateBoundsInput)
      resultsLowerBounds <- computeBounds(n, 0, alpha[2], phi[2], t, t2, 1, usedFunction[2], TruncateBoundsInput)
    }

    ## get the values depending on one-sided or two-sided test had been made ##
    ##-- symmetric bounds --##
    if (!BoundsSymmetry==3)
    {
      lowerBounds <- results[[1]]
      upperBounds <- results[[2]]
    }

    ##-- asymmetric bounds --##
    else
    {
      upperBounds <- resultsUpperBounds[[2]]
      lowerBounds <- (-1)*resultsLowerBounds[[2]]
    }


    ## if (5) Pocock Type - the real Pocock Bounds' was chosen -
    ## we have to do some extra calculations
    ## The Spending function gives us an approximately Pocock-Design.
    ## To compute the exact Pocock Bounds we will do according to the following pattern:
    ## (1st)we give the bounds with all bounds are equal. As starting value we are using the
    ##      mean of the bounds computed by our Pocock spending function. I figured out that
    ##      in almost every case this is a quite good approximation so far.
    ## (2nd)we compute the probability according to our equal bounds, as we would do, if user
    ##      had chosen Task-3- at the beginning
    ## (3rd)we use Newton Iteration to adjust the bounds in every Iteration until we get the appropriate alpha

    if(usedFunction[1]==5 || usedFunction[2]==5)
    {
      ##check for symmetric bounds
      #one-sided
      if(BoundsSymmetry==1)
      {
        upperBounds <- calculateEqualBounds(alpha[1],upperBounds,n,t2)
      }

      ##two-sided symmetric
      else if(BoundsSymmetry==2)
           {
             {
               upperBounds <- calculateEqualBounds(alpha[1]/2,upperBounds,n,t2)
               lowerBounds <- -upperBounds
             }
           }

           else
           ## asymmetric bounds -> maybe we have to calculate 2 times
           {
             ##check where (5) Pocock Type - the real Pocock Bounds was chosen
             if(usedFunction[1]==5)
             {
               upperBounds <- calculateEqualBounds(alpha[1],upperBounds,n,t2)
             }
             if(usedFunction[2]==5)
             {
               lowerBounds <- (-1)*calculateEqualBounds(alpha[2],-lowerBounds,n,t2)
             }
           }
    }#end <--*if(whatSpendingFunctionIsUsed[1]==5 || whatSpendingFunctionIsUsed[2]==5)*

   }#end <--*if(!enterBoundsManually)*



  ##-----------------------------------------------------------##
  ##-Confidence limits from bounds and final statistics value.-##
  ##-----------------------------------------------------------##
  confidenceIntervall <- computeConfidenceIntervall(confidenceLevel,Zvalue,n,t,t2,lowerBounds,upperBounds,nMax)

  ##output results
  guiOutputTask4(n,confidenceLevel,secondTimeScaleIsUsed,t,t2,t2max,lowerBounds,upperBounds,BoundsSymmetry,
                   enterBoundsManually,alpha,phi,confidenceIntervall,usedFunction,Zvalue, taskWindow)


}#end <--*function calculateTask4*


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
    pnorm = stats::pnorm
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


"computeBounds" <-
function(n,drift,alpha,phi,t,t2,OneOrTwoSidedBounds,whatSpendingFunctionIsUsed,boundsTruncation)
{

###########################################################################
######################### INITIALIZE VARIABLES ############################
###########################################################################
  probExit<-0 # probExit is a vector of exit probabilities
  probDifference<-0 # probDifference(i) = probExit(i)-probExit(i-1) where probExit is a vector of exit probabilities.
  lowerBounds <-0 # lowerBounds is the vector of lower standardized boundaries
  upperBounds <-0 # upperBounds is the vector of upper standardized boundaries
  gridSize<-0.05 # the grid size for numerical integration by trapezoidal rule
  lastGrid<-0 # the grid of the joint density at the last analysis.
  numberOfIntegrationIntervalls<-0 #"integer" the number of intervals for numerical integration

  lowerIntegrationLimit<-0 # the vector of lower integration limits.
  upperIntegrationLimit<-0 # the vector of upper integration limits.
  noError<-TRUE

  pnorm <- stats::pnorm
  qnorm <- stats::qnorm



###########################################################################
############################### START #####################################
###########################################################################

  ## Negative infinity is set to -8 by default
  negInf<-(-8)  #negInf is "negative infinity" for the program.

########## COMPUTE PROBABILITIES ACCORDING TO USE FUNCTION ################

  ## Therefore function 'alphaByUseFunction(...)' is called ###
  probExit <- alphaByUseFunction(whatSpendingFunctionIsUsed,n,alpha,phi,OneOrTwoSidedBounds,t)

  ## Compute 'probDifference' - the change in type I error to spent
  toleranceProbDiff <- 1.0e-13

  for (i in 1:n)
  {
    ##catch first loop
    if (i==1)
    {
      probDifference[1]<-probExit[1]
    }
    ##do the rest
    else
    {
      probDifference[i]<-probExit[i]-probExit[i-1]
    }

    ##Check type I error to spend
    if (probDifference[i]<0 || probDifference[i]>1)
    {
      probDifference[i]<-min(1,probDifference[i])
      probDifference[i]<-max(0,probDifference[i])
      cat("\n")
      cat(" Error in spending function at interim time:",i,"\n")
      cat(" Calculated probabilites are:",probExit,"\n")
      cat(" Calculated function is not increasing strictly or out of range!","\n")
      cat(" the differences intype I error spent between analyses","\n")
      cat(" at this point will be set to:",probDifference[i],"\n")

    }

    if (probDifference[i]<toleranceProbDiff)
    {
      cat("\n")
      cat(" Type I error spent too small at interim time:",i,"\n")
      cat(" Zero used as approximation for:","\n")
      print(probDifference[i],digits=22)
      cat("\n")
    }
   }#end <--*for*


   # Standard deviations of increments and process
   stdDev.inc <- c(sqrt(t2[1]), sqrt(diff(t2)))
   stdDev.proc <- sqrt(t2)

###########################################################################
################### BEGIN CALCULATING BOUNDARIES ##########################
###########################################################################

##--------------------------------------------------------##
##-- Direct calculations can be made for first analysis --##
##--------------------------------------------------------##

  ###Check type I error to spend
  if (probDifference[1]<0 || probDifference[1] >1)
  {
    print("",quote=FALSE)
    print(" Error in spending function - alpha is not in [0,1].",quote=FALSE)
    probDifference[1]<-min(1,probDifference[1])
    probDifference[1]<-max(0,probDifference[1])
  }

  ##Spending probability is zero (or less - so it was set to zero)
  if (probDifference[1]==0)
  {
    ##if boundaries should be truncated set upperBounds[1] accordingly
    ##instead of -negInf that is "negative infinity" for the programm
    upperBounds[1]<- -negInf
    if (upperBounds[1] > boundsTruncation)
    {
      upperBounds[1] <- boundsTruncation
      probDifference[1] <- OneOrTwoSidedBounds* ( 1-pnorm(upperBounds[1]) )
      probExit[1] <- probDifference[1]

      ##Difference between second and first exit probability
      if (n > 1)
      {
        probDifference[2] <- probExit[2] - probExit[1]
      }
    }
    upperIntegrationLimit[1] <- upperBounds[1]*stdDev.inc[1]
  }#end <--*if*

  ##Spending probability is one (or more - so it was set to one)
  else if (probDifference[1]==1)
       {
         upperBounds[1] <- 0
         upperIntegrationLimit[1] <- upperBounds[1]*stdDev.inc[1]
       }

       else
       {
         ##First bound based on normal distribution.
         upperBounds[1] <- qnorm( 1- (probDifference[1]/OneOrTwoSidedBounds) )

         ##check whether bound must be truncated
         if (upperBounds[1]>boundsTruncation)
         {
           upperBounds[1] <- boundsTruncation
           probDifference[1] <- OneOrTwoSidedBounds * ( 1 - pnorm(upperBounds[1]) )
           probExit[1] <- probDifference[1]

           ##Difference between second and first exit probability
           if (n > 1)
           {
             probDifference[2] <- probExit[2] - probExit[1]
             cat("probExit: ",probExit,"\n")
             cat("probDifference: ",probDifference[2],"\n")
           }
         }
         upperIntegrationLimit[1] <- upperBounds[1]*stdDev.inc[1]
       }#end <--*else*
  ###end Checking type I error to spend


  ##Lower bound is either "negative infinity" (one-sided test)
  ##or -upperIntegrationLimit (two-sided test )
  if (OneOrTwoSidedBounds==1)
  {
    lowerBounds[1] <- negInf
    lowerIntegrationLimit[1] <- lowerBounds[1]*stdDev.inc[1]
  }
  else
  {
    lowerBounds[1] <- -upperBounds[1]
    lowerIntegrationLimit[1] <- -upperIntegrationLimit[1]
  }



  ##Number of intervals for numerical integration
  numberOfIntegrationIntervalls[1] <- trunc( (upperIntegrationLimit[1]-lowerIntegrationLimit[1]) / (gridSize*stdDev.inc[1]) )

##------------------------------------------------##
##-- Calculations for second and later analyses --##
##------------------------------------------------##


  ##catch n<2 that is user choosed just one interim analysis
  if(n<2)
  {
    #do nothing
  }

  else #go on with further analysis
  {
    for (i in 2:n)
    {
      ##Calculate joint density for use in next step
      if (i==2)
      {
        ##call function 'jointDensity' with parameter==1,
        ##for computing joint Density in 1st analysis - look also function jointDensity
        lastGrid <- jointDensity(1, lowerIntegrationLimit[1], upperIntegrationLimit[1], stdDev.inc[1], numberOfIntegrationIntervalls, lastGrid)
        #                    |
        #               parameter==1
      }

      ###Check type I error to spend
      if (probDifference[i]<0 || probDifference[i] >1)
      {
        print("",quote=FALSE)
        print(" Error in spending function - alpha is not in [0,1].",quote=FALSE)
        probDifference[i]<-min(1,probDifference[i])
        probDifference[i]<-max(0,probDifference[i])
      }

      ##Spending probability is zero (or less than tolerance)##
      if (probDifference[i] < toleranceProbDiff)
      {
        upperBounds[i] <- -negInf

        ##check whether bound must be truncated
        if (upperBounds[i]>boundsTruncation)
        {
          upperBounds[i] <- boundsTruncation
          probDifference[i]<-OneOrTwoSidedBounds*tailProbability(upperBounds[i]*stdDev.proc[i],lastGrid,numberOfIntegrationIntervalls[i-1],lowerIntegrationLimit[i-1],upperIntegrationLimit[i-1],stdDev.inc[i])
          probExit[i] <- probDifference[i] + probExit[i-1]

          if(n>i)
          {
            probDifference[i+1] <- probExit[i+1]-probExit[i]
          }
        }
        upperIntegrationLimit[i] <- upperBounds[i]*stdDev.proc[i]

      }#end <--*if*


      ##Spending probability is one (or more).##
      else if (probDifference[i]==1)
           {
             upperBounds[i] <- 0
             upperIntegrationLimit[i] <- upperBounds[i]*stdDev.inc[i] ## that is <- 0
           }

      ##-------------------------------------------------##
      ##-- Using a search algorithm to find the bounds --##
      ##-------------------------------------------------##

           ##Bounds are found using a search starting at the bound from
           ##the previous analysis
           else
           {
             upperIntegrationLimit[i] <- searchForBound(lastGrid,numberOfIntegrationIntervalls,i,gridSize, probDifference[i]/OneOrTwoSidedBounds, stdDev.inc[i], lowerIntegrationLimit, upperIntegrationLimit,n)

             ##check if function searchForBound(...) worked correctly
             if (!is.numeric(upperIntegrationLimit[i]))
             {
               ##in this case something went wrong - function returned noError==FALSE
               print(" Error in function 'searchForBound' - analysis aborted!",quote=FALSE)
               noError <- FALSE
               break
             }
             else ##everything went ok - function returned a numeric
             {
               #standarize upper boundary
               upperBounds[i] <- upperIntegrationLimit[i]/stdDev.proc[i]
             }

             ##If a truncation point is used, check to see if it
             ##applies and recompute probabilities if necessary.
             if(upperBounds[i]>boundsTruncation)
             {
               upperBounds[i]<-boundsTruncation
               probDifference[i]<-OneOrTwoSidedBounds*tailProbability(upperBounds[i]*stdDev.proc[i],lastGrid,numberOfIntegrationIntervalls[i-1],lowerIntegrationLimit[i-1],upperIntegrationLimit[i-1],stdDev.inc[i])

               if(n>i)
               {
                 probDifference[i+1] <- probExit[i+1]-probExit[i]
               }
             }
             upperIntegrationLimit[i] <- upperBounds[i]*stdDev.proc[i]

           }#end <--*else*


      ##Lower bound is either "negative infinity" (one-sided test)
      ##or -upperIntegrationLimit (two-sided test )
      if (OneOrTwoSidedBounds==1)
      {
        lowerIntegrationLimit[i] <- negInf*stdDev.proc[i]
        lowerBounds[i] <- negInf
      }
      else
      {
        lowerIntegrationLimit[i] <- -upperIntegrationLimit[i]
        lowerBounds[i] <- -upperBounds[i]
      }

      ##Number of intervals for numerical integration.
      numberOfIntegrationIntervalls[i] <- trunc( (upperIntegrationLimit[i]-lowerIntegrationLimit[i]) / (gridSize*stdDev.inc[i]) )

      ##Calculate joint density for use in next step.
      if (i!=n)
      {
        lastGrid <- jointDensity(i, lowerIntegrationLimit, upperIntegrationLimit, stdDev.inc[i], numberOfIntegrationIntervalls, lastGrid)
      }

      #goto next step i.e. to i-th step in the for-loop
    }#end <--*for (i in 2:n)*
  }#end <--*else #go on with further analysis*

  ##Return a list containing the vectors lowerBounds, upperBounds, probExit, probDifference
  list(lowerBounds=lowerBounds, upperBounds=upperBounds, exitProbabilities=probExit,differencesExitProbabilities=probDifference)
}#end <--*function(...)*


"computeConfidenceIntervall" <-
function(confidenceLevel, Zvalue, n, t, t2, lowerBounds, upperBounds,nMax)
{
  ###INITIALIZE VARIABLES###
  confidenceLimit<-0
  zcrit<-0

  ##Save upperBounds[n]
  tempUpperBounds <- upperBounds[n]
  upperBounds[n] <- Zvalue

  ##Use naive limits as starting values
  zcrit <- stats::qnorm(1-(1-confidenceLevel)/2)
  confidenceLimit[1] <- (upperBounds[n]-zcrit)/sqrt(t[n])
  confidenceLimit[2] <- (upperBounds[n]+zcrit)/sqrt(t[n])

  for(i in 1:2)
  {
    ##compute target Zvalue which function 'computeDrift' has to fullfill
    target <- (i-1)*confidenceLevel + (1-confidenceLevel)/2
    confidenceLimit[i] <- computeDrift(n,t,t2,lowerBounds,upperBounds,target,confidenceLimit[i],nMax)
  }

  ##Re-Replace upperBounds[n]
  upperBounds[n] <- tempUpperBounds

  return(confidenceLimit)


}#end <--*function(...)*


"computeCurrentProbability" <-
function( lastGrid, numberOfIntegrationIntervalls, lowerIntegrationLimit, upperIntegrationLimit, i , gridSize, standardDeviation )
{
  ######################
  #INITIALIZE VARIABLES#
  ######################

  ## Returned variables:
  probStopping <-0 #is the probaility of reaching ith analysis and stopping
                   #here the value of the integral from lowerIntegrationLimit to upperIntegrationLimit.
  probExceedingUpper<-0 # the probaility of reaching ith and exceeding upper.
                   #here the value of the integral from upperIntegrationLimit to infinity.
  probExceedingLower<-0 #is the probability of reaching ith and exceeding lower
                        #here the value of the integral from lowerIntegrationLimit to minus infinity

  ## Local variables:
  valProbStopping<-0 #vector of integrand values for probStopping
  valProbExceedingUpper<-0 #vector of integrand values for probExceedingUpper
  valProbExceedingLower<-0 #vector of integrand values for probExceedingLower
  grid<-0 # grid is the argument for valProbStopping, valProbExceedingUpper and valProbExceedingLower.

  # numberOfIntegrationIntervalls is the number of steps of size gridSize between lowerIntegrationLimit and upperIntegrationLimit.
  gridFromLastStep<-0 #grid size from last step.

  ##Previous grid size.
  gridFromLastStep <- (upperIntegrationLimit[i-1]-lowerIntegrationLimit[i-1]) / numberOfIntegrationIntervalls[i-1]


  ##Function values to be passed to numerical integration
  ##routine are calculated.
  grid <- seq(lowerIntegrationLimit[i-1],upperIntegrationLimit[i-1],length=numberOfIntegrationIntervalls[i-1]+1)
  valProbExceedingUpper <- ( 1 - stats::pnorm( (upperIntegrationLimit[i]-grid) / standardDeviation ) ) * lastGrid
  valProbExceedingLower <- (     stats::pnorm( (lowerIntegrationLimit[i]-grid) / standardDeviation ) ) * lastGrid

  ##--Calls to numerical integration routine.--##

  ## probStopping depends on valProbStopping
  ## probStopping <- integrateByTrapez(valProbStopping, numberOfIntegrationIntervalls[i-1], gridFromLastStep)
  probStopping <- 0

  probExceedingUpper <- integrateByTrapez(valProbExceedingUpper, numberOfIntegrationIntervalls[i-1], gridFromLastStep)
  probExceedingLower <- integrateByTrapez(valProbExceedingLower, numberOfIntegrationIntervalls[i-1], gridFromLastStep)


  ##Return a vector containing the results probStopping, probExceedingUpper, probExceedingLower
  toBeReturned<-list(probAndStop=probStopping, probAndExceedingUpper=probExceedingUpper, probAndExceedingLower=probExceedingLower)
  return(toBeReturned)


}#end <--*function(...)*


"computeDrift" <-
function(n, t, t2, lowerBounds, upperBounds, target, drift, nMax)
{
  probStopping <-0 #is the probability of reaching ith analysis and stopping.
  probExceedingUpper<-0 # the probability of reaching ith and exceeding upper.
  probExceedingLower<-0 #is the probability of reaching ith and exceeding lower.
  probTotal<-0 #is the total probability of rejecting, sum(1,n)(probExceedingUpper+probExceedingLower)

  ##variables from old Fortran Implementation (they are only used if Newton Iteration fails which usually should not happen)
  lo<-0
  hi<-0
  prev<-0
  gotLo<-0
  gotHi<-0
  tol<-1.0e-6
  del<-0.25



  noError<-FALSE
  ##backup start value 'drift', in case Newton Iteration fails
  saveDrift <-drift

  ##---------------------------------------------------------------------##
  ##---- first use the "Sekanten-Verfahren" based on Newton Iteration ---##
  ##---------------------------------------------------------------------##
  ## ##
  ## calculation obeys following pattern whereby x    converges against  ##
  ## the value we are searching for               k+1 ##

  ##             x  _  x                                                 ##
  ##              k     k-1                                              ##
  ##  x    =  ------------------ * f(x )                                 ##
  ##   k+1     f(x ) - f(x   )        k    ##
  ##              k       k-1              ##

    ## Initialize values
    newtonFailed <- FALSE
    numberOfInterimAnalysis <- n


    ##set x    and f(x   )
    ##     k-1        k-1
    ##--------------------
    xkMinusOne <- drift

    ## first calculate fxkMinusOne... ##
    vectorOfResults <- computeAlphaLevel(n, t, t2, lowerBounds, upperBounds, xkMinusOne, nMax)
    vectorOfProbabilities <- vectorOfResults[[2]]

    ##...then set it
    fxkMinusOne <- abs( sum(vectorOfProbabilities)-target )


    ## Choose start point for iteration -
    ## the more interim analysis we have the closer we choose our startpoint to yb[i-1]
    ## which is done by yb[i-1]-epsilon.

    ## This choice leads to an average number of about 4 iterations to converge -
    ## in case of equally spaced interim analysis it is mostly even better leading to about 3 iterations.

    ## There may be better choices than that.

    ## If we got very many interim analysis we have to limit our epsilon to 10^-10
    if(numberOfInterimAnalysis > 10)
    {
      epsilon <- 10^(-10)
    }
    else
    {
      epsilon <- 10^(-numberOfInterimAnalysis)
    }

    ##set x  and f(x )
    ##     k        k
    ##----------------
    xk <- xkMinusOne-epsilon

    ## first calculate fxk... ##
    vectorOfResults <- computeAlphaLevel(n, t, t2, lowerBounds, upperBounds, xk, nMax)
    vectorOfProbabilities <- vectorOfResults[[2]]

    ##...then set it
    fxk <- abs( sum(vectorOfProbabilities)-target )

    ##number of loops with purpose of controlling convergence
    numberOfLoops<-0

    ## We do 20 iterations at maximum
    ## if we do not have finished then, we won't converge at all
    for (j in 1:20)
    {
      numberOfLoops <- j

      ## get new xkPlusOne like shown above
      xkPlusOne <- xk - ( (xk - xkMinusOne)/(fxk - fxkMinusOne) * fxk )

      ##catch xkPlusOne is NOT defined for any reason for example if (fxk-fxkMinusOne == 0)
      if(is.nan(xkPlusOne))
      {
        newtonFailed <- TRUE
        break
      }

      ##catch diverging xkPlusOne
      if(xkPlusOne==Inf || xkPlusOne==-Inf)
      {
        newtonFailed <- TRUE
        break
      }

      ##calculate new fxkPlusOne
      vectorOfResults <- computeAlphaLevel(n, t, t2, lowerBounds, upperBounds, xkPlusOne, nMax)
      vectorOfProbabilities <- vectorOfResults[[2]]
      fxkPlusOne <- abs( sum(vectorOfProbabilities)-target )

      ## check if we reached tolerance
      if ( fxkPlusOne <= tol)
      {
        ##tolerance is fulfilled - return xkPlusOne
        drift<-xkPlusOne
        noError<-TRUE
        break  #leave the loop
      }
      else
      {
        ## not within tolerance yet - set new values and do next iteration
        xkMinusOne<- xk
        fxkMinusOne <- fxk

        xk <- xkPlusOne
        fxk <- fxkPlusOne
      }
    }#end <--*for (j in 1:20)*

    ##If all 20 loops were made, something must be wrong -
    ##therefore we try old method from Fortran Implementation
    if (numberOfLoops==20)
    {
      newtonFailed <- TRUE
    }

###########################################################################
########### Old Implementation - usually it should NOT be used ############
###########################################################################


  ##"Sekanten-Verfahren" failed - so we try old conservative method
  if(newtonFailed)
  {
    ##--------------------------------------------------------##
    ##--Compute upper exit prob for drift, should be target.--##
    ##--------------------------------------------------------##
    count<-0
    probTotalWithinTolerance<-FALSE
    while(!probTotalWithinTolerance)
    {
      count<-count+1
      vectorOfResults <- computeAlphaLevel(n, t, t2, lowerBounds, upperBounds, drift, nMax)
      probExceedingUpper <- vectorOfResults[[2]]
      probTotal<-0

      ##THIS OUGHT TO BE probTotal + probStopping FOR POWER!  It works for
      ##positive drifts since probExceedingLower is tiny but for drift < 0
      ##this ought to fail miserably.
      probTotal <- sum(probExceedingUpper)

      if( abs(probTotal-target) <= tol )
      {
        ##probTotal is within tolerance of target ->leave while loop
        probTotalWithinTolerance<-TRUE  ##leave while-loop
        noError<-TRUE
      }
      #check convergence
      else if( abs(drift-prev) <= tol/10 )
           {
             ##drift changes by less than tol/10, stop calculating
             cat(" Convergence problem in function 'computeDrift' during computing the drift.","\n")
             cat(" !!!Calculation stopped now!!!","\n")
             break
           }

           else if( probTotal>target+tol)
                {
                  ##Make sure hi gives probTotal<=target+tol
                  hi <- drift
                  drift <- drift-del
                  gotHi<-1
                }

                else
                {
                  ##Make sure lo gives probTotal<=target+tol
                  lo<-drift
                  drift<-drift+del
                  gotLo<-1
                }

      ##If bracketing values have been found, bisect.
      if(gotLo==1 && gotHi==1)
      {
        prev <- drift
        drift <- (lo+hi)/2
      }

    }#end <--*while(!probTotalWithinTolerance)*
  }

  ##if one of the routines above worked correctly -> return the calculated value...
  if (noError)
  {
    ##Return value of drift
    return(drift)
  }

  ##...else abort analysis
  else
  {
    print("Error in function 'computeDrift': not converging. Abort analysis!",quote=FALSE)
    return(noError)
  }





}#end <--*function*


"findDrift" <-
function(n,t,t2,lowerBounds,upperBounds,confidenceLevel,drift, nMax)
{
  ######################
  #INITIALIZE VARIABLES#
  ######################
  drift<-0

  ## Starting value for drift: there may be better choices.
  drift[1] <- ( upperBounds[n]+stats::qnorm(confidenceLevel) ) / sqrt(t[n])
  drift <- computeDrift(n,t,t2,lowerBounds,upperBounds,confidenceLevel,drift[1],nMax)

  return(drift)

}#end <--*function(...)*


"guiInputTask1" <-
function(taskWindow)
{

  ### Initialize Variables ###
  #default inputs
  n<-1 # number of interim analyses
  equallySpacedTimesInput<-TRUE #variable denoting whether equally spaced times (equallySpacedTimesInput==TRUE) or different spaced (equallySpacedTimesInput==FALSE)
  secondTimeScaleIsUsedInput<-FALSE # second time/information scale (yes=>secondTimeScaleIsUsedInput==TRUE, no=>secondTimeScaleIsUsedInput==FALSE)
  alphaInput<-0.05 # 'alphaInput' is input of the desired overall size.
  enterBoundsManually<-FALSE # denotes whether bounds are entered manually by user (enterBoundsManually==TRUE) or to be computed by spending function
  BoundsSymmetry<-1 # BoundsSymmetry==1 means one-sided bounds, BoundsSymmetry==2 means two-sided symmetric and BoundsSymmetry==3 means asymmetric bounds.
  functionInput<-1 # indicates type I error spending rate function e.g. the function(s) the user choosed
  TruncateBoundsInput<-8 ## here 8 equals infity
  truncateBoundsYesNo<-FALSE # default is no truncating of bounds


  #some lists (names are self-explanatory)
  t<-1  # list of interim analyses
  t2<-t    # list of second time scale - by default t2==t
  listOfTimePointLabel.unequalTimes<-list()
  listOfInputFields.unequalTimes<-list()
  listOfEntries.unequalTimes<-list()

  listOfTimePointLabel.secondTimes<-list()
  listOfInputFields.secondTimes<-list()
  listOfEntries.secondTimes<-list()

  #some status variables (names are self-explanatory)
  nInputEdited<-FALSE
  equallySpacedTimesCheckBoxClicked<-FALSE
  secondTimeScaleCheckBoxClicked<-FALSE

  #operating variables
  nBackup<-n # backup n
  boundBackup<-BoundsSymmetry # backup BoundsSymmetry
  alpha1<- alphaInput # set alpha
  alpha2<- 0 # alpha2 is need in case of asymmetric bounds
  function1<-functionInput # set function1
  function2<-functionInput # function2 is need in case of asymmetric bounds
  phi1<-1 # optional Parameter referring to Power Family
  phi2<-1 # phi2 is need in case of asymmetric bounds both using Power family

  #Define some Fonts
  fontItalic <- tkfont.create(family="times",size=10,slant="italic")
  fontBig <- tkfont.create(family="times",size=10,weight="bold")


####################################################################################################
#################------------------------------------------------------------#######################
##################  FUNCTIONS THAT HANDLE EVENTS TAKING PLACE IN THE WINDOW ########################
#################------------------------------------------------------------#######################
####################################################################################################

  #########################################################
  # function handles change on number of interim analyses #
  #########################################################
  onChangeInterimAnalyses<-function(nValue)
  {
    #First check whether n has changed
    if(n==nValue)
    {
      #nothing changed - so do nothing
    }
    else #interim times changed
    {
      #set new n
      n<<-nValue

      #First we'll have to recompute the lists t,t2 new e.g
      #they got default values equally spaced
      t<<-1
      for(i in 1:n)
      {
        t[i] <<- i/n
      }
      t2<<-t

      #update n in menu bar
      tkdelete(topMenu,0,1)
      tkadd(topMenu,"cascade",label=paste("#Interim Times: K=",as.character(n)),menu=nMenu)

      ### equally or unequally spaced times? get it from the checkbox ###
      equallySpacedTimesInput <- as.logical(as.numeric(tclvalue(equallySpacedTimesCheckBoxValue)))

      # check case unequally checkboxes - grid input fields with number of interim analyses into the frames
      if(!equallySpacedTimesInput)
      {
        #first remove "old" labels and input fields - old n is stored in nBackup
        for(i in 1:nBackup)
        {
  #remove labels and input fields
          tkgrid.remove(listOfTimePointLabel.unequalTimes[[i]],listOfEntries.unequalTimes[[i]])
        }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.unequalTimes<<-list()
        listOfTimePointLabel.unequalTimes<<-list()
        listOfEntries.unequalTimes<<-list()

        #create new labels by calling function 'onCheckBoxEquallySpacedTimes()'
        onCheckBoxEquallySpacedTimes()

      }#end <--*if(!equallySpacedTimesInput)*


      ### Second Time scale - will it be used? ###
      secondTimeScaleIsUsedInput <- as.logical(as.numeric(tclvalue(secondTimeScaleIsUsedCheckBoxValue)))

      #check case second times scale checkbox is activated
      if(secondTimeScaleIsUsedInput)
      {
        #first remove "old" labels and input fields - old n is stored in nBackup
        for(i in 1:nBackup)
        {
  #remove labels and input fields
          tkgrid.remove(listOfTimePointLabel.secondTimes[[i]],listOfEntries.secondTimes[[i]])
        }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.secondTimes<<-list()
        listOfTimePointLabel.secondTimes<<-list()
        listOfEntries.secondTimes<<-list()

        #create new labels by calling function 'onCheckBoxSecondTimeScale()'
        onCheckBoxSecondTimeScale()

      }#end <--*if(secondTimeScaleIsUsedInput)*

    }#end <--*else #interim times changed*

    #update nBackup
    nBackup<<-n
  }#end <--*onChangeInterimAnalyses<-function(nValue)*


  ###################################################################################
  # function handles a click on checkbox for equally/unequally spaced interim times #
  ###################################################################################
  onCheckBoxEquallySpacedTimes <- function()
  {
    #equally or unequally spaced times? get it from the checkbox
    equallySpacedTimesInput <- as.logical(as.numeric(tclvalue(equallySpacedTimesCheckBoxValue)))

    # case unequally checkboxes - grid input fields with number of interim analyses into the frames
    if(!equallySpacedTimesInput)
    {
      for(i in 1:n)
      {
        #create label in a list thus we can dynamically change number of input fields
        listOfTimePointLabel.unequalTimes<<-c(listOfTimePointLabel.unequalTimes,list(tklabel(unEquallyDynamicFrame, text=paste("time",as.character(i)))))

        #We need a list of Input Fields to be able to save the dynamic created tclVar's
        listOfInputFields.unequalTimes<<-c(listOfInputFields.unequalTimes,list(tclVar(as.character(t[i]))))
        listOfEntries.unequalTimes<<-c(listOfEntries.unequalTimes, list(tkentry(unEquallyDynamicFrame,width="11",textvariable=as.character(listOfInputFields.unequalTimes[[i]]))))

        #put label with Input field
        tkgrid(listOfTimePointLabel.unequalTimes[[i]],listOfEntries.unequalTimes[[i]])
        tkgrid.configure(listOfTimePointLabel.unequalTimes[[i]],sticky="nw")
        tkgrid.configure(listOfEntries.unequalTimes[[i]],sticky="nw")
      }#end <--*for*
    #put frame
    tkgrid(unEquallyDynamicFrame)
    }#end <--*if*

    else #equally spaced - remove all input fields cause they should disappear in the window
    {
      #fade out frame
      tkgrid.forget(unEquallyDynamicFrame)

      for(i in 1:n)
      {
        #remove labels and input fields
tkgrid.remove(listOfTimePointLabel.unequalTimes[[i]],listOfEntries.unequalTimes[[i]])
      }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.unequalTimes<<-list()
        listOfTimePointLabel.unequalTimes<<-list()
        listOfEntries.unequalTimes<<-list()
    }
  }#end <--*function()*

  ##############################################################
  # function handles a click on checkbox for second time scale
  #
  # ATTENTION: this feature of a second time scale is currently NOT used!
  #
  ##############################################################
  onCheckBoxSecondTimeScale <- function()
  {
    #second time scale used?
    secondTimeScaleIsUsedInput <- as.logical(as.numeric(tclvalue(secondTimeScaleIsUsedCheckBoxValue)))

    # case unequally checkboxes - grid input fields with number of interim analyses into the frames
    if(secondTimeScaleIsUsedInput)
    {
      for(i in 1:n)
      {
        #create label in a list thus we can dynamically change number of input fields
        listOfTimePointLabel.secondTimes<<-c(listOfTimePointLabel.secondTimes,list(tklabel(secondTimesDynamicFrame, text=paste("time",as.character(i)))))

        #We need a list of Input Fields to be able to save the dynamic created tclVar's
        listOfInputFields.secondTimes<<-c(listOfInputFields.secondTimes,list(tclVar(as.character(t2[i]))))
        listOfEntries.secondTimes<<-c(listOfEntries.secondTimes, list(tkentry(secondTimesDynamicFrame,width="11",textvariable=as.character(listOfInputFields.secondTimes[[i]]))))

        #put label with Input field
        tkgrid(listOfTimePointLabel.secondTimes[[i]],listOfEntries.secondTimes[[i]])
        tkgrid.configure(listOfTimePointLabel.secondTimes[[i]],sticky="nw")
        tkgrid.configure(listOfEntries.secondTimes[[i]],sticky="nw")
      }#end <--*for*
    #put frame
    tkgrid(secondTimesDynamicFrame)
    }#end <--*if*

    else #equally spaced - remove all input fields cause they should disappear in the window
    {
      #fade out frame
      tkgrid.forget(secondTimesDynamicFrame)

      for(i in 1:n)
      {
#remove labels and input fields
tkgrid.remove(listOfTimePointLabel.secondTimes[[i]],listOfEntries.secondTimes[[i]])
      }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.secondTimes<<-list()
        listOfTimePointLabel.secondTimes<<-list()
        listOfEntries.secondTimes<<-list()
    }

  }#end <--*onCheckBoxSecondTimeScale <- function()*



  ####################################################################
  # functions handle a click on CONFIRM-Buttons to choose a function #
  ####################################################################
  onConfirmFunction1 <-function()
  {
    #check whether we got symmetric or asymmetric bounds
    BoundsSymmetry <<- as.numeric(tclvalue(SymmetryValue))

    #get value from listbox and ask for additional parameters if necessary
    if( BoundsSymmetry==1 || BoundsSymmetry==2)
    {
      function1<<-as.numeric(tkcurselection(listBoxFunction1of1))+1
    }
    else
    {
      function1<<-as.numeric(tkcurselection(listBoxFunction1of2))+1
    }

    #check whether user selected a function
    if( length(function1)==0 )
    {
      tkmessageBox(message="You must select a function!",icon="error",type="ok")
    }

    else # handle select
    {
      ### ASYMMETRIC ###
      if(BoundsSymmetry==3)
      {
        #first of all remove earlier input field
        tkgrid.remove(phiLabel1of2,entry.functionParameter1of2)

        #case Power Family
        if (function1==3)
        {
          #set new label and input field
          phiLabel1of2<<-tklabel(additionalParametersFrame1of2,text="Enter Paramter phi>0:")
          entry.functionParameter1of2 <<-tkentry(additionalParametersFrame1of2,width="6",textvariable=phi1of2)
          tkgrid(phiLabel1of2,sticky="w")
          tkgrid(entry.functionParameter1of2,sticky="w")
        }

        #case Hwang-Shih-DeCani family
        else if (function1==4)
             {
               #set new label and input field
               phiLabel1of2<<-tklabel(additionalParametersFrame1of2,text="Enter Parameter phi=/=0:")
               entry.functionParameter1of2 <<-tkentry(additionalParametersFrame1of2,width="6",textvariable=phi1of2)
               tkgrid(phiLabel1of2,sticky="w")
               tkgrid(entry.functionParameter1of2,sticky="w")
             }
             #case no additional parameters needed
             else
             {
               #do nothing else
             }
      }#end <--*if(BoundsSymmetry==3)*

      else ### SYMMETRIC - do the same in other frame###
      {
        #first of all remove earlier input field
        tkgrid.remove(phiLabel1of1,entry.functionParameter1of1)

        #get value from listbox and ask for additional parameters if necessary
        functionInput <<- as.numeric(tkcurselection(listBoxFunction1of1))+1

        #case Power Family
        if (function1==3)
        {
          #set new label and input field
          phiLabel1of1<<-tklabel(additionalParametersFrame1of1,text="Enter Paramter phi>0:")
          entry.functionParameter1of1 <<-tkentry(additionalParametersFrame1of1,width="6",textvariable=phi1of1)
          tkgrid(phiLabel1of1,sticky="w")
          tkgrid(entry.functionParameter1of1,sticky="w")
        }

        #case Hwang-Shih-DeCani family
        else if (function1==4)
             {
               #set new label and input field
               phiLabel1of1<<-tklabel(additionalParametersFrame1of1,text="Enter Parameter phi=/=0:")
               entry.functionParameter1of1 <<-tkentry(additionalParametersFrame1of1,width="6",textvariable=phi1of1)
               tkgrid(phiLabel1of1,sticky="w")
               tkgrid(entry.functionParameter1of1,sticky="w")
             }
             #case no additional parameters needed
             else
             {
               #do nothing else
             }
      }#end <--*else ### SYMMETRIC - do the same in other frame###     *
    }#end <--*else # handle select*
  }#end <--*onConfirmFunction1 <-function()*




  onConfirmFunction2 <-function()
  {
    #get value from listbox and ask for additional parameters if necessary
    function2<<-as.numeric(tkcurselection(listBoxFunction2of2))+1

    #check whether user selected a function
    if( length(function2)==0 )
    {
      tkmessageBox(message="You must have select a function!",icon="error",type="ok")
    }

    else # handle select
    {
      #first of all remove earlier input field
      tkgrid.remove(phiLabel2of2,entry.functionParameter2of2)

      #case Power Family
      if (function2==3)
      {
        #set new label and input field
        phiLabel2of2<<-tklabel(additionalParametersFrame2of2,text="Enter Paramter phi>0:")
        entry.functionParameter2of2 <<-tkentry(additionalParametersFrame2of2,width="6",textvariable=phi2of2)
        tkgrid(phiLabel2of2,sticky="w")
        tkgrid(entry.functionParameter2of2,sticky="w")
      }

      #case Hwang-Shih-DeCani family
      else if (function2==4)
           {
             #set new label and input field
             phiLabel2of2<<-tklabel(additionalParametersFrame2of2,text="Enter Parameter phi=/=0:")
             entry.functionParameter2of2 <<-tkentry(additionalParametersFrame2of2,width="6",textvariable=phi2of2)
             tkgrid(phiLabel2of2,sticky="w")
             tkgrid(entry.functionParameter2of2,sticky="w")
           }
           #case no additional parameters needed
           else
           {
             #do nothing else
           }
    }#end <--*else # handle select*
  }#end <--*onConfirmFunction2 <-function()*



  ##################################################################
  # function handles a click on Radio Button for ASYMMETRIC BOUNDS #
  ##################################################################
  onBoundsChosen <- function()
  {
    #check whether we got asymmetric bounds
    BoundsSymmetry <<- as.numeric(tclvalue(SymmetryValue))

    #Asymmetric Bounds!
    if( (BoundsSymmetry==1 || BoundsSymmetry==2) & boundBackup!=3)
    {
      #nothing to change
    }
    else if( (BoundsSymmetry==1 || BoundsSymmetry==2) & boundBackup==3)
         {
           #exchange frames
           tkgrid.remove(nonSymmetricBoundsFrame)
           tkgrid(symmetricBoundsFrame,sticky="nw")
         }
         else if(BoundsSymmetry==3 & boundBackup!=3)
              {
                #exchange frames
                tkgrid.remove(symmetricBoundsFrame)
                tkgrid(nonSymmetricBoundsFrame,sticky="nw")

              }
  #update boundBackup
  boundBackup<<-BoundsSymmetry
  }


  #######################################################
  # function handles a click on Trunate Bounds Checkbox #
  #######################################################
  onTruncateCheckbox <- function()
  {
    #checkbox activated?
    truncateBoundsYesNo <<- as.logical(as.numeric(tclvalue(TruncateBoundsCheckBoxValue)))

    if(truncateBoundsYesNo) #activated
    {
      ##grid edit box
      tkgrid(TruncateDynamicFrame)
    }
    else #deactivated
    {
      #ungrid edit box
      tkgrid.forget(TruncateDynamicFrame)
    }
  }


  ##################################################
  # function handles a click on 'CALCULATE'-Button #
  ##################################################
  OnCalculateInputTask1 <- function()
  {
    readyForCalculate <- TRUE

    #get values from checkboxes, listboxes and radiobuttons
    equallySpacedTimesInput <<- as.logical(as.numeric(tclvalue(equallySpacedTimesCheckBoxValue)))
    secondTimeScaleIsUsedInput <<- as.logical(as.numeric(tclvalue(secondTimeScaleIsUsedCheckBoxValue)))
    BoundsSymmetry <<- as.numeric(tclvalue(SymmetryValue))
    truncateBoundsYesNo <<- as.logical(as.numeric(tclvalue(TruncateBoundsCheckBoxValue)))

    if(truncateBoundsYesNo)
    {
      TruncateBoundsInput <<- abs(as.numeric(tclvalue(boundsTruncation)))
    }
    else
    {
      TruncateBoundsInput<-8
    }

    #get chosen function(s) and check alpha
    ###################
    # case asymmetric #
    ###################
    if(BoundsSymmetry==3)
    {
      alpha1 <<- as.numeric(tclvalue(alpha1of2))
      alpha2 <<- as.numeric(tclvalue(alpha2of2))

      #check alpha
      alphaAll<- alpha1 + alpha2
      if( !(alpha1>=0 & alpha1<=1 & alpha2>=0 & alpha2<=1 & alphaAll<=1) )
      {
        readyForCalculate<-FALSE
        tkmessageBox(message="Alpha out of range! Correct it and try again.",icon="error",type="ok")
      }

      #check phi if entered as parameter
      phi1 <<- as.numeric(tclvalue(phi1of2))
      phi2 <<- as.numeric(tclvalue(phi2of2))
      #function for UPPER bounds
      if(function1==3)
      {
        if( !(phi1>0) )
        {
          readyForCalculate<-FALSE
          tkmessageBox(message="Parameter phi in function for UPPER bounds must be >0 !",icon="error",type="ok")
        }
      }
      else if(function1==4)
           {
             if(phi1==0)
             {
               readyForCalculate<-FALSE
               tkmessageBox(message="Parameter phi in function for UPPER bounds may NOT be zero!",icon="error",type="ok")
             }
        }

      #same with function for LOWER bounds
      if(function2==3)
      {
        if( !(phi2>0) )
        {
          readyForCalculate<-FALSE
          tkmessageBox(message="Parameter phi in function for LOWER bounds must be >0 !",icon="error",type="ok")
        }
      }
      else if(function2==4)
           {
             if(phi2==0)
             {
               readyForCalculate<-FALSE
               tkmessageBox(message="Parameter phi in function for LOWER bounds may NOT be zero!",icon="error",type="ok")
             }
        }

    }#end <--*if(BoundsSymmetry==3)*


    ##################
    # case symmetric #
    ##################
    else #one function cause of symmetric bounds
    {
      alpha1 <<- as.numeric(tclvalue(alpha1of1))
      #check alpha
      if( !(alpha1>=0 & alpha1<=1) )
      {
        readyForCalculate<-FALSE
        tkmessageBox(message="Alpha out of range! Correct it and try again.",icon="error",type="ok")
      }

      #check phi if entered as parameter
      phi1 <<- as.numeric(tclvalue(phi1of1))
      #what function used?
      if(function1==3)
      {
        if( !(phi1>0) )
        {
          readyForCalculate<-FALSE
          tkmessageBox(message="Parameter phi in function must be >0 !",icon="error",type="ok")
        }
      }
      else if(function1==4)
           {
             if(phi1==0)
             {
               readyForCalculate<-FALSE
               tkmessageBox(message="Parameter phi in function may NOT be zero!",icon="error",type="ok")
             }
        }
    }


    ##if user typed in unequally spaced times - get them and
    ##check them to be in intervall(0,1] and in right order
    if(!equallySpacedTimesInput)
    {
      tempVal<-0
      interimTimesBad<-FALSE
      for(i in 1:n)
      {
        tempVal[i] <- as.numeric(tclvalue(listOfInputFields.unequalTimes[[i]]))

        if (tempVal[i]<=0) { interimTimesBad<-TRUE }
        if (tempVal[i]>1) { interimTimesBad<-TRUE }
        if (i>1)
        {
          if (tempVal[i]<=tempVal[i-1]) { interimTimesBad<-TRUE }
        }
      }#end <--*for(i in 1:n)*

      ##if times are not good => error and keep old times
        if(interimTimesBad)
        {
          readyForCalculate <- FALSE
          tkmessageBox(message="Bad Interim Times entered - old Times are kept so far! ",icon="error",type="ok")
        }
        ##else take new times
        else
        {
          t<<-tempVal
        }

    }#end <--*if(equallySpacedTimesInput)*
    else
    {
     for(i in 1:n)
     {
       t[i]<<-i/n
     }
    }

    ##if user typed in second time scales - get them and
    ##check them to be in intervall(0,1] and in right order
    if(secondTimeScaleIsUsedInput)
    {
      tempVal<-0
      secondTimesBad<-FALSE
      for(i in 1:n)
      {
        tempVal[i] <- as.numeric(tclvalue(listOfInputFields.secondTimes[[i]]))

        if (tempVal[i]<=0) { secondTimesBad<-TRUE }
        if (tempVal[i]>1) { secondTimesBad<-TRUE }
        if (i>1)
        {
          if (tempVal[i]<=tempVal[i-1]) { secondTimesBad<-TRUE }
        }
      }#end <--*for(i in 1:n)*

      ##if times are not good => error and keep old times
        if(secondTimesBad)
        {
          readyForCalculate <- FALSE
          tkmessageBox(message="Bad Second Time Scale entered - old Times are kept so far! ",icon="error",type="ok")
        }
        ##else take new times
        else
        {
          t2<<-tempVal
        }

    }#end <--*if(secondTimeScaleIsUsedInput)*

    if(readyForCalculate)
    {
      # second time scale is not used so far --> set t2=t
      t2<-t
      calculateTask1(n,t,t2,equallySpacedTimesInput,secondTimeScaleIsUsedInput, BoundsSymmetry, c(alpha1,alpha2),
                     c(phi1,phi2), c(function1,function2),TruncateBoundsInput, taskWindow)
    }
    else
    {
      tkmessageBox(message="Error - maybe you forgot to CONFIRM your function(s)",icon="error",type="ok")
    }
  }



#######################################################################################################
#################------------------------------------------------------------------####################
##################  FROM HERE ON LABELS AND FRAMES ARE CREATED AND PUT TOGETHER   #####################
#################------------------------------------------------------------------####################
#######################################################################################################

  #Set Toplevel
  task1 <- tktoplevel(taskWindow)
  tkwm.title(task1,"-1- Compute Bounds")

  #Define main Frame
  InputTask1 <- tkframe(task1, relief="groove",borderwidth=2)


  ##--------------------------------------------------------------------------------##
  ##------------------------  number of Interim Times  -----------------------------##
  ##--------------------------------------------------------------------------------##

  #create pull down menu to select interim analyses from n==1 to n==25(=nMax)
  topMenu <- tkmenu(task1)
  tkconfigure(task1,menu=topMenu)
  nMenu <- tkmenu(topMenu,tearoff=FALSE,background="grey",activebackground="red")
  tkadd(nMenu,"command",label="1",command=function() onChangeInterimAnalyses(1))
  tkadd(nMenu,"command",label="2",command=function() onChangeInterimAnalyses(2))
  tkadd(nMenu,"command",label="3",command=function() onChangeInterimAnalyses(3))
  tkadd(nMenu,"command",label="4",command=function() onChangeInterimAnalyses(4))
  tkadd(nMenu,"command",label="5",command=function() onChangeInterimAnalyses(5))
  tkadd(nMenu,"command",label="6",command=function() onChangeInterimAnalyses(6))
  tkadd(nMenu,"command",label="7",command=function() onChangeInterimAnalyses(7))
  tkadd(nMenu,"command",label="8",command=function() onChangeInterimAnalyses(8))
  tkadd(nMenu,"command",label="9",command=function() onChangeInterimAnalyses(9))
  tkadd(nMenu,"command",label="10",command=function() onChangeInterimAnalyses(10))
  tkadd(nMenu,"command",label="11",command=function() onChangeInterimAnalyses(11))
  tkadd(nMenu,"command",label="12",command=function() onChangeInterimAnalyses(12))
  tkadd(nMenu,"command",label="13",command=function() onChangeInterimAnalyses(13))
  tkadd(nMenu,"command",label="14",command=function() onChangeInterimAnalyses(14))
  tkadd(nMenu,"command",label="15",command=function() onChangeInterimAnalyses(15))
  tkadd(nMenu,"command",label="16",command=function() onChangeInterimAnalyses(16))
  tkadd(nMenu,"command",label="17",command=function() onChangeInterimAnalyses(17))
  tkadd(nMenu,"command",label="18",command=function() onChangeInterimAnalyses(18))
  tkadd(nMenu,"command",label="19",command=function() onChangeInterimAnalyses(19))
  tkadd(nMenu,"command",label="20",command=function() onChangeInterimAnalyses(20))
  tkadd(nMenu,"command",label="21",command=function() onChangeInterimAnalyses(21))
  tkadd(nMenu,"command",label="22",command=function() onChangeInterimAnalyses(22))
  tkadd(nMenu,"command",label="23",command=function() onChangeInterimAnalyses(23))
  tkadd(nMenu,"command",label="24",command=function() onChangeInterimAnalyses(24))
  tkadd(nMenu,"command",label="25",command=function() onChangeInterimAnalyses(25))
  tkadd(topMenu,"cascade",label=paste("#Interim Times: K= ",as.character(n)),menu=nMenu)

  tkgrid(tklabel(InputTask1,text="")) # Blank line


  ##--------------------------------------------------------------------------------##
  ##-------------  Interim Times equally or unequally spaced? ----------------------##
  ##-------------       Second Time Scale will be used?       ----------------------##
  ##--------------------------------------------------------------------------------##

  ## prepare Frames
  interimTimesFrame<- tkframe(InputTask1,relief="groove",borderwidth=0)
  equallySpacedTimesFrame <- tkframe(interimTimesFrame,relief="groove",borderwidth=0)
  secondTimesFrame <- tkframe(interimTimesFrame,relief="groove",borderwidth=0)

  #again we need a frame for dynamic working in it to not affect
  #the format of 'equallySpacedTimesFrame' respective 'secondTimesFrame'
  equallySpacedLabelFrame<-tkframe(equallySpacedTimesFrame,relief="groove",borderwidth=0)
  unEquallyDynamicFrame<-tkframe(equallySpacedTimesFrame,relief="groove",borderwidth=0)
  secondTimesLabelFrame<-tkframe(secondTimesFrame,relief="groove",borderwidth=0)
  secondTimesDynamicFrame<-tkframe(secondTimesFrame,relief="groove",borderwidth=0)

  #Default is Equally Spaced Times and no Second Time Scale
  #create Checkboxes
  #equally spaced
  equallySpacedTimesCheckBox<-tkcheckbutton(equallySpacedLabelFrame,command=onCheckBoxEquallySpacedTimes)
  equallySpacedTimesCheckBoxValue <- tclVar(as.character(as.numeric(equallySpacedTimesInput)))
  tkconfigure(equallySpacedTimesCheckBox,variable=equallySpacedTimesCheckBoxValue)
  #second time scale
  secondTimeScaleIsUsedCheckBox<-tkcheckbutton(secondTimesLabelFrame,command=onCheckBoxSecondTimeScale)
  secondTimeScaleIsUsedCheckBoxValue <- tclVar(as.character(as.numeric(secondTimeScaleIsUsedInput)))
  tkconfigure(secondTimeScaleIsUsedCheckBox,variable=secondTimeScaleIsUsedCheckBoxValue)

  #put checkbox and other frames togehter
  equallyTimesBoxLabel<-tklabel(equallySpacedLabelFrame,text="Equally Spaced Times")
  secondTimesBoxLabel<-tklabel(secondTimesLabelFrame,text="Use Second Time Scale")
  tkgrid(equallyTimesBoxLabel,equallySpacedTimesCheckBox)
  # tkgrid(secondTimesBoxLabel,secondTimeScaleIsUsedCheckBox) - this feature is currently removed
  tkgrid(equallySpacedTimesFrame,secondTimesFrame,sticky="nw")
  tkgrid(equallySpacedLabelFrame,sticky="nw")
  tkgrid(secondTimesLabelFrame,sticky="nw")
  tkgrid(unEquallyDynamicFrame,sticky="nw")
  tkgrid(secondTimesDynamicFrame,sticky="nw")
  tkgrid(interimTimesFrame,sticky="w")
  tkgrid(tklabel(InputTask1,text="")) # Blank line

  ###One- or Two-Sided Bounds or asymmetric Bounds###
  #create frames
  boundsLabelFrame <- tkframe(InputTask1,relief="groove",borderwidth=0)
  boundsRadioButtonFrame <- tkframe(InputTask1,relief="groove",borderwidth=0)

  #create radio buttons
  oneSided <- tkradiobutton(boundsRadioButtonFrame,command=onBoundsChosen)
  twoSided <- tkradiobutton(boundsRadioButtonFrame,command=onBoundsChosen)
  asymmetric <- tkradiobutton(boundsRadioButtonFrame,command=onBoundsChosen)
  SymmetryValue <- tclVar(as.character(BoundsSymmetry))
  tkconfigure(oneSided,variable=SymmetryValue,value="1")
  tkconfigure(twoSided,variable=SymmetryValue,value="2")
  tkconfigure(asymmetric,variable=SymmetryValue,value="3")

  #grid labels and buttons together
  tkgrid(tklabel(boundsLabelFrame,text="One-, Two-Sided-Symmetric or Asymmetric Bounds?"),sticky="w")
  tkgrid(tklabel(boundsRadioButtonFrame,text="One-Sided "),oneSided)
  tkgrid(tklabel(boundsRadioButtonFrame,text="Two-Sided "),twoSided)
  tkgrid(tklabel(boundsRadioButtonFrame,text="Asymmetric "),asymmetric)

  #put frames
  tkgrid(boundsLabelFrame,sticky="w")
  tkgrid(boundsRadioButtonFrame,sticky="w")
  tkgrid(tklabel(InputTask1,text="")) # Blank line



  ### Significance Level(s) alpha and function(s) to be used to calculate bounds###
  ## if user choses asymmetric bounds two different functions could be used ##
  ##create Frames
  alphaAndFunctionsFrame<-tkframe(InputTask1,relief="groove",borderwidth=0)
  symmetricBoundsFrame<-tkframe(alphaAndFunctionsFrame,relief="groove",borderwidth=0)
  nonSymmetricBoundsFrame<-tkframe(alphaAndFunctionsFrame,relief="groove",borderwidth=0)

  ##Default alpha1==0.05, alpha2==0.025
  alpha1of1 <- tclVar(as.character(alphaInput))
  alpha1of2 <- tclVar(as.character("0.025"))
  alpha2of2 <- tclVar(as.character("0.025"))

  #########################################################
  ### case symmetric bounds or one-sided test (default) ###
  #########################################################
  #frames
  alphaFrame1of1 <- tkframe(symmetricBoundsFrame,relief="groove",borderwidth=0)
  functionsFrame1of1 <- tkframe(symmetricBoundsFrame,relief="groove",borderwidth=0)
  additionalParametersFrame1of1<-tkframe(symmetricBoundsFrame,relief="groove",borderwidth=0)

  ##create Labels for alpha
  alphaLabel1of1<-tklabel(alphaFrame1of1,text="Significance Level: alpha=")
  entry.alpha1of1 <-tkentry(alphaFrame1of1,width="6",textvariable=alpha1of1)

  #create Listbox for function choice
  functionLabel1of1<-tklabel(functionsFrame1of1,text="What function should be used?")
  listBoxFunction1of1<-tklistbox(functionsFrame1of1,height=5,width=30,selectmode="single",background="grey")
  functionChoice1of1 <- c("(1) O'Brien-Fleming Type","(2) Pocock Type",
    "(3) Power Family: alpha* t^phi","(4) Hwang-Shih-DeCani Family","(5) Exact Pocock Bounds")
  for (i in (1:5))
  {
    tkinsert(listBoxFunction1of1,"end",functionChoice1of1[i])
  }
  tkselection.set(listBoxFunction1of1, functionInput-1)  # Default function is O'Brien-Fleming Type.  Indexing starts at zero.

  #create and put button to confirm a function because for example in case of 'Power family: alpha* t^phi'
  #user has to enter additional parameter 'phi'
  confirmFun.button1of1 <-tkbutton(functionsFrame1of1,text=" CONFIRM FUNCTION ",command=onConfirmFunction1)

  #create variable for edit box which we will need if additional parameters must be entered
  #edit box is unvisible at beginning since default function O'Brien-Fleming Type does not need any additional parameters
  phi1of1 <- tclVar(as.character(phi1))
  phiLabel1of1<-tklabel(additionalParametersFrame1of1,text="")
  entry.functionParameter1of1 <-tkentry(additionalParametersFrame1of1,width="3",textvariable=phi1of1)

  #grid together
  #alpha
  tkgrid(alphaLabel1of1,entry.alpha1of1)
  tkgrid.configure(alphaLabel1of1,sticky="w")
  tkgrid.configure(entry.alpha1of1,sticky="w")
  tkgrid(tklabel(alphaFrame1of1,text="")) # Blank line
  tkgrid(functionLabel1of1,sticky="w")
  tkgrid(listBoxFunction1of1)

  #put frames and button
  tkgrid(alphaFrame1of1,sticky="w")
  tkgrid(functionsFrame1of1,additionalParametersFrame1of1)
  tkgrid(confirmFun.button1of1)
  tkgrid.configure(functionsFrame1of1,sticky="w")
  tkgrid(tklabel(symmetricBoundsFrame,text="")) # Blank line

  #Finally grid frame for symmetric case as default
  tkgrid(symmetricBoundsFrame,sticky="nw")
  tkgrid(alphaAndFunctionsFrame,sticky="w")


  ##############################
  ### case Asymmetric bounds ###
  ##############################
  #frames
  alphaFrame1of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  functionsFrame1of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  additionalParametersFrame1of2<-tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  alphaFrame2of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  functionsFrame2of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  additionalParametersFrame2of2<-tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)

  ##create Labels for alpha
  alphaLabel1of2<-tklabel(alphaFrame1of2,text="UPPER Bounds: alpha=")
  entry.alpha1of2 <-tkentry(alphaFrame1of2,width="6",textvariable=alpha1of2)

  alphaLabel2of2<-tklabel(alphaFrame2of2,text="LOWER Bounds: alpha=")
  entry.alpha2of2 <-tkentry(alphaFrame2of2,width="6",textvariable=alpha2of2)

  #create Listboxes for function choice
  #################
  # List Box 1of2 #
  #################
  functionLabel1of2<-tklabel(functionsFrame1of2,text="Choose Function for UPPER Bounds")
  listBoxFunction1of2<-tklistbox(functionsFrame1of2,height=5,width=30,selectmode="single",background="grey")
  functionChoice1of2 <- c("(1) O'Brien-Fleming Type","(2) Pocock Type",
    "(3) Power Family: alpha* t^phi","(4) Hwang-Shih-DeCani Family","(5) Exact Pocock Bounds")
  for (i in (1:5))
  {
    tkinsert(listBoxFunction1of2,"end",functionChoice1of2[i])
  }

  #create and put first Confirm button which "commands" same function as in symmetric case did
  confirmFun.button1of2 <-tkbutton(functionsFrame1of2,text=" CONFIRM FUNCTION ",command=onConfirmFunction1)
  #edit box for parameter phi[1]
  phi1of2 <- tclVar(as.character(phi1))
  phiLabel1of2<-tklabel(additionalParametersFrame1of2,text="")
  entry.functionParameter1of2 <-tkentry(additionalParametersFrame1of2,width="3",textvariable=phi1of2)

  #################
  # List Box 2of2 #
  #################
  functionLabel2of2<-tklabel(functionsFrame2of2,text="Choose Function for LOWER Bounds")
  listBoxFunction2of2<-tklistbox(functionsFrame2of2,height=5,width=30,selectmode="single",background="grey")
  functionChoice2of2 <- c("(1) O'Brien-Fleming Type","(2) Pocock Type",
    "(3) Power Family: alpha* t^phi","(4) Hwang-Shih-DeCani Family","(5) Exact Pocock Bounds")
  for (i in (1:5))
  {
    tkinsert(listBoxFunction2of2,"end",functionChoice2of2[i])
  }

  #create and put first Confirm button
  confirmFun.button2of2 <-tkbutton(functionsFrame2of2,text=" CONFIRM FUNCTION ",command=onConfirmFunction2)

  #edit box for parameter phi[2]
  phi2of2 <- tclVar(as.character(phi2))
  phiLabel2of2<-tklabel(additionalParametersFrame2of2,text="")
  entry.functionParameter2of2 <-tkentry(additionalParametersFrame2of2,width="3",textvariable=phi2of2)

  #grid together

  #1of2
  tkgrid(alphaLabel1of2,entry.alpha1of2)
  tkgrid.configure(alphaLabel1of2,sticky="w")
  tkgrid.configure(entry.alpha1of2,sticky="w")
  tkgrid(functionLabel1of2,sticky="w")
  tkgrid(listBoxFunction1of2)

  #put frames and button
  tkgrid(alphaFrame1of2,sticky="w")
  tkgrid(functionsFrame1of2,additionalParametersFrame1of2)
  tkgrid(confirmFun.button1of2)
  tkgrid.configure(functionsFrame1of2,sticky="w")
  tkgrid(tklabel(nonSymmetricBoundsFrame,text="")) # Blank line


  #2of2
  tkgrid(alphaLabel2of2,entry.alpha2of2)
  tkgrid.configure(alphaLabel2of2,sticky="w")
  tkgrid.configure(entry.alpha2of2,sticky="w")
  tkgrid(functionLabel2of2,sticky="w")
  tkgrid(listBoxFunction2of2)

  #put frames and button
  tkgrid(alphaFrame2of2,sticky="w")
  tkgrid(functionsFrame2of2,additionalParametersFrame2of2)
  tkgrid(confirmFun.button2of2)
  tkgrid.configure(functionsFrame2of2,sticky="w")
  tkgrid(tklabel(nonSymmetricBoundsFrame,text="")) # Blank line

  #################################################################

  ###Truncate Bounds?###
  TruncateBoundsFrame <- tkframe(InputTask1,relief="groove",borderwidth=0)
  TruncateLabelFrame <- tkframe(TruncateBoundsFrame,relief="groove",borderwidth=0)
  TruncateDynamicFrame <-tkframe(TruncateBoundsFrame,relief="groove",borderwidth=0)

  #create checkbox
  TruncateBoundsCheckBox<-tkcheckbutton(TruncateLabelFrame,command=onTruncateCheckbox)
  TruncateBoundsCheckBoxValue <- tclVar(as.character(as.numeric(truncateBoundsYesNo)))
  tkconfigure(TruncateBoundsCheckBox,variable=TruncateBoundsCheckBoxValue)

  #create variable for edit box which we will need if user wants truncation of bounds -
  boundsTruncation <- tclVar(as.character(TruncateBoundsInput))
  boundsTruncationLabel<-tklabel(TruncateDynamicFrame,text="Enter Truncation Point:")
  entry.truncationValue <-tkentry(TruncateDynamicFrame,width="3",textvariable=boundsTruncation)

  #put frames
  tkgrid(tklabel(TruncateLabelFrame,text="Truncate Standardized Bounds?"),TruncateBoundsCheckBox)
  tkgrid(boundsTruncationLabel,entry.truncationValue,sticky="w")
  tkgrid(TruncateLabelFrame,TruncateDynamicFrame,sticky="w")
  tkgrid(TruncateBoundsFrame,sticky="w")
  tkgrid.forget(TruncateDynamicFrame) #default is no Truncating
  tkgrid(tklabel(InputTask1,text="")) # Blank line

  ##put Overall Frame
  tkgrid(InputTask1)

  #frame for the buttons
  buttonFrame<-tkframe(task1,relief="groove",borderwidth=0)

  #create and put button for calculating
  calculate.button <-tkbutton(buttonFrame,text=" CALCULATE ",command=OnCalculateInputTask1)

  # function handles click onto button to Cancel i.e. close current window
  onCancel <- function()
  {
   tkdestroy(task1)
  }
  cancel.button <-tkbutton(buttonFrame,text=" Cancel ",command=onCancel)

  # grid buttons
  tkgrid( tklabel(buttonFrame, text=""))   #blank line
  tkgrid(calculate.button, tklabel(buttonFrame, text="            "),
         cancel.button, sticky="we" )
  tkgrid( tklabel(buttonFrame, text=""))   #blank line
  tkgrid(buttonFrame)

  tkfocus(task1)

}


"guiInputTask2" <-
function(taskWindow)
{

  ### Initialize Variables ###
  #default inputs
  nMax<-25 # Number of interim analyses is limited to 25
  n<-1 # number of interim analyses
  confidenceLevel<-0.8 # desired power respective confidence level
  alphaInput<-0.05 # 'alphaInput' is input of the desired overall size.
  t<-1  # vector containing interim analyses
  t2<-t # vector containing second time scale - by default t2==t
  t3<-t2 # t3 is t2 divided by the maximum information, if needed
  t2max<-0 # maximum value of t2
  upperBounds<-0 # vector containing upper bounds
  lowerBounds<-0 # vector containing lower bounds
  BoundsSymmetry<-1 # BoundsSymmetry==1 means one-sided bounds, BoundsSymmetry==2 means two-sided symmetric and BoundsSymmetry==3 means asymmetric bounds.
  functionInput<-1 # indicates type I error spending rate function e.g. the function(s) the user choosed
  TruncateBoundsInput<-8 ## here 8 equals infinity

  #some status variables (names are self-explanatory)
  nInputEdited<-FALSE
  equallySpacedTimesCheckBoxClicked<-FALSE
  secondTimeScaleCheckBoxClicked<-FALSE
  equallySpacedTimesInput<-TRUE
  secondTimeScaleIsUsedInput<-FALSE
  enterBoundsManually<-FALSE
  truncateBoundsYesNo<-FALSE # default is no truncating of bounds

  #some lists (names are self-explanatory)
  listOfTimePointLabel.unequalTimes<-list() #
  listOfInputFields.unequalTimes<-list()
  listOfEntries.unequalTimes<-list()

  listOfTimePointLabel.secondTimes<-list()
  listOfInputFields.secondTimes<-list()
  listOfEntries.secondTimes<-list()

  listOfTimePointLabel.boundsUPPER<-list() #
  listOfInputFields.boundsUPPER<-list()
  listOfEntries.boundsUPPER<-list()

  listOfTimePointLabel.boundsLOWER<-list() #
  listOfInputFields.boundsLOWER<-list()
  listOfEntries.boundsLOWER<-list()

  #operating variables
  nBackup<-n # backup n
  boundBackup<-BoundsSymmetry # backup BoundsSymmetry
  alpha1<- alphaInput # set alpha
  alpha2<- 0 # alpha2 is need in case of asymmetric bounds
  function1<-functionInput # set function1
  function2<-functionInput # function2 is need in case of asymmetric bounds
  phi1<-1 # optional Parameter referring to Power Family
  phi2<-1 # phi2 is need in case of asymmetric bounds both using Power family
  drift<-0 # drift by default 0

  #Define some Fonts
  fontItalic <- tkfont.create(family="times",size=10,slant="italic")
  fontBig <- tkfont.create(family="times",size=10,weight="bold")


####################################################################################################
#################------------------------------------------------------------#######################
##################  FUNCTIONS THAT HANDLE EVENTS TAKING PLACE IN THE WINDOW ########################
#################------------------------------------------------------------#######################
####################################################################################################

  #########################################################
  # function handles change on number of interim analyses #
  #########################################################
  onChangeInterimAnalyses<-function(nValue)
  {
    #First check whether n has changed
    if(n==nValue)
    {
      #nothing changed - so do nothing
    }
    else #interim times changed
    {
      #set new n
      n<<-nValue

      #we'll have to recompute the lists t,t2 new e.g
      #they got default values equally spaced and also set the bounds to some default values
      t<<-1
      for(i in 1:n)
      {
        t[i]<<-i/n
        upperBounds[i]<<-i
      }
      t2<<-t
      lowerBounds<<- -upperBounds


      #update n in menu bar
      tkdelete(topMenu,0,1)
      tkadd(topMenu,"cascade",label=paste("#Interim Times: K=",as.character(n)),menu=nMenu)

      ### equally or unequally spaced times? get it from the checkbox ###
      equallySpacedTimesInput <- as.logical(as.numeric(tclvalue(equallySpacedTimesCheckBoxValue)))

      # check case unequally checkboxes - grid input fields with number of interim analyses into the frames
      if(!equallySpacedTimesInput)
      {
        #first remove "old" labels and input fields - old n is stored in nBackup
        for(i in 1:nBackup)
        {
  #remove labels and input fields
          tkgrid.remove(listOfTimePointLabel.unequalTimes[[i]],listOfEntries.unequalTimes[[i]])
        }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.unequalTimes<<-list()
        listOfTimePointLabel.unequalTimes<<-list()
        listOfEntries.unequalTimes<<-list()

        #create new labels by calling function 'onCheckBoxEquallySpacedTimes()'
        onCheckBoxEquallySpacedTimes()

      }#end <--*if(!equallySpacedTimesInput)*


      ### Second Time scale - will it be used? ###
      secondTimeScaleIsUsedInput <- as.logical(as.numeric(tclvalue(secondTimeScaleIsUsedCheckBoxValue)))

      #check case second times scale checkbox is activated
      if(secondTimeScaleIsUsedInput)
      {
        #first remove "old" labels and input fields - old n is stored in nBackup
        for(i in 1:nBackup)
        {
  #remove labels and input fields
          tkgrid.remove(listOfTimePointLabel.secondTimes[[i]],listOfEntries.secondTimes[[i]])
        }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfTimePointLabel.secondTimes<<-list() #
        listOfInputFields.secondTimes<<-list()
        listOfEntries.secondTimes<<-list()

        #create new labels and input fields by calling function 'onCheckBoxSecondTimeScale()'
        onCheckBoxSecondTimeScale()

      }#end <--*if(secondTimeScaleIsUsedInput)*

      ### if user enters bounds manually we have to adapt appropriate input fields
      if(enterBoundsManually)
      {

        #check on symmetric or asymmetric bounds used
        if(!BoundsSymmetry==3)
        {
          #symmetric
          #first remove "old" labels and input fields - old n is stored in nBackup
          for(i in 1:nBackup)
          {
    #remove labels and input fields
            tkgrid.remove(listOfTimePointLabel.boundsUPPER[[i]],listOfEntries.boundsUPPER[[i]])
          }
          #set the lists to NULL otherwise we would duplicate entries in a next loop
          listOfInputFields.boundsUPPER<<-list()
          listOfTimePointLabel.boundsUPPER<<-list()
          listOfEntries.boundsUPPER<<-list()

          #create new labels and input fields by calling function 'onManualBounds()'
          onManualBounds()

        }#end <--*if(!BoundsSymmetry==3)*

        else #asymmetric bounds - do the same as with symmetric for both frames
        {
          for(i in 1:nBackup)
          {
    #remove labels and input fields
            tkgrid.remove(listOfTimePointLabel.boundsUPPER[[i]],listOfEntries.boundsUPPER[[i]])
            tkgrid.remove(listOfTimePointLabel.boundsLOWER[[i]],listOfEntries.boundsLOWER[[i]])
          }
          #set the lists to NULL otherwise we would duplicate entries in a next loop
          listOfInputFields.boundsUPPER<<-list()
          listOfTimePointLabel.boundsUPPER<<-list()
          listOfEntries.boundsUPPER<<-list()
          listOfInputFields.boundsLOWER<<-list()
          listOfTimePointLabel.boundsLOWER<<-list()
          listOfEntries.boundsLOWER<<-list()

          #create new labels and input fields by calling function 'onManualBounds()'
          onManualBounds()
        }#end <--*else #asymmetric bounds - do the same as with symmetric for both frames*

      }#end <--*if(enterBoundsManually)*


    }#end <--*else #interim times changed*

    #update nBackup
    nBackup<<-n
  }#end <--*onChangeInterimAnalyses<-function(nValue)*


  ###################################################################################
  # function handles a click on checkbox for equally/unequally spaced interim times #
  ###################################################################################
  onCheckBoxEquallySpacedTimes <- function()
  {
    #equally or unequally spaced times? get it from the checkbox
    equallySpacedTimesInput <- as.logical(as.numeric(tclvalue(equallySpacedTimesCheckBoxValue)))

    # case unequally checkboxes - grid input fields with number of interim analyses into the frames
    if(!equallySpacedTimesInput)
    {
      for(i in 1:n)
      {
        #create label in a list thus we can dynamically change number of input fields
        listOfTimePointLabel.unequalTimes<<-c(listOfTimePointLabel.unequalTimes,list(tklabel(unEquallyDynamicFrame, text=paste("time",as.character(i)))))

        #We need a list of Input Fields to be able to save the dynamic created tclVar's
        listOfInputFields.unequalTimes<<-c(listOfInputFields.unequalTimes,list(tclVar(as.character(t[i]))))
        listOfEntries.unequalTimes<<-c(listOfEntries.unequalTimes, list(tkentry(unEquallyDynamicFrame,width="11",textvariable=as.character(listOfInputFields.unequalTimes[[i]]))))

        #put label with Input field
        tkgrid(listOfTimePointLabel.unequalTimes[[i]],listOfEntries.unequalTimes[[i]])
        tkgrid.configure(listOfTimePointLabel.unequalTimes[[i]],sticky="nw")
        tkgrid.configure(listOfEntries.unequalTimes[[i]],sticky="nw")
      }#end <--*for*
    #put frame
    tkgrid(unEquallyDynamicFrame)
    }#end <--*if*

    else #equally spaced - remove all input fields cause they should disappear in the window
    {
      #fade out frame
      tkgrid.forget(unEquallyDynamicFrame)

      for(i in 1:n)
      {
        #remove labels and input fields
tkgrid.remove(listOfTimePointLabel.unequalTimes[[i]],listOfEntries.unequalTimes[[i]])
      }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.unequalTimes<<-list()
        listOfTimePointLabel.unequalTimes<<-list()
        listOfEntries.unequalTimes<<-list()
    }
  }#end <--*function()*

  ##############################################################
  # function handles a click on checkbox for second time scale
  #
  # ATTENTION: this feature of a second time scale is currently NOT used!
  #
  ##############################################################
  onCheckBoxSecondTimeScale <- function()
  {
    #second time scale used?
    secondTimeScaleIsUsedInput <- as.logical(as.numeric(tclvalue(secondTimeScaleIsUsedCheckBoxValue)))

    # case unequally checkboxes - grid input fields with number of interim analyses into the frames
    if(secondTimeScaleIsUsedInput)
    {
      for(i in 1:n)
      {
        #create label in a list thus we can dynamically change number of input fields
        listOfTimePointLabel.secondTimes<<-c(listOfTimePointLabel.secondTimes,list(tklabel(secondTimesDynamicFrame, text=paste("time",as.character(i)))))

        #We need a list of Input Fields to be able to save the dynamic created tclVar's
        listOfInputFields.secondTimes<<-c(listOfInputFields.secondTimes,list(tclVar(as.character(t2[i]))))
        listOfEntries.secondTimes<<-c(listOfEntries.secondTimes, list(tkentry(secondTimesDynamicFrame,width="11",textvariable=as.character(listOfInputFields.secondTimes[[i]]))))

        #put label with Input field
        tkgrid(listOfTimePointLabel.secondTimes[[i]],listOfEntries.secondTimes[[i]])
        tkgrid.configure(listOfTimePointLabel.secondTimes[[i]],sticky="nw")
        tkgrid.configure(listOfEntries.secondTimes[[i]],sticky="nw")
      }#end <--*for*
    #put frame
    tkgrid(secondTimesDynamicFrame)
    }#end <--*if*

    else #equally spaced - remove all input fields cause they should disappear in the window
    {
      #fade out frame
      tkgrid.forget(secondTimesDynamicFrame)

      for(i in 1:n)
      {
#remove labels and input fields
tkgrid.remove(listOfTimePointLabel.secondTimes[[i]],listOfEntries.secondTimes[[i]])
      }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.secondTimes<<-list()
        listOfTimePointLabel.secondTimes<<-list()
        listOfEntries.secondTimes<<-list()
    }

  }#end <--*onCheckBoxSecondTimeScale <- function()*




  #################################################################
  # function handles a click on checkbox to enter bounds manually #
  #################################################################
  onManualBounds <-function()
  {
    #get checkbox value
    enterBoundsManually<<-as.numeric(tclvalue(manualBoundsCheckBoxValue))


    ### user wants to enter bounds manually ###
    if(enterBoundsManually)
    {
      #fade out frame with choice of functions and truncating bounds checkbox
      tkgrid.forget(computedBoundsFrame)
      tkgrid.forget(TruncateBoundsFrame)

      #symmetric or asymmetric bounds? get checkbox value and check it out
      BoundsSymmetry <<- as.numeric(tclvalue(SymmetryValue))


      #at least we need one input field
      for(i in 1:n)
      {
        #create label in a list thus we can dynamically change number of input fields
        listOfTimePointLabel.boundsUPPER<<-c(listOfTimePointLabel.boundsUPPER,list(tklabel(manualBoundsUPPERframe.InputFields, text=paste("time",as.character(i)))))

        #We need a list of Input Fields to be able to save the dynamic created tclVar's
        listOfInputFields.boundsUPPER<<-c(listOfInputFields.boundsUPPER,list(tclVar(as.character(upperBounds[i]))))
        listOfEntries.boundsUPPER<<-c(listOfEntries.boundsUPPER, list(tkentry(manualBoundsUPPERframe.InputFields,width="11",textvariable=as.character(listOfInputFields.boundsUPPER[[i]]))))

        #put label with Input field
        tkgrid(listOfTimePointLabel.boundsUPPER[[i]],listOfEntries.boundsUPPER[[i]])
        tkgrid.configure(listOfTimePointLabel.boundsUPPER[[i]],sticky="nw")
        tkgrid.configure(listOfEntries.boundsUPPER[[i]],sticky="nw")
      }#end <--*for*

      #if asymmetric bounds we need an additional second input field
      if(BoundsSymmetry==3)
      {
        for(i in 1:n)
        {
          #create label in a list thus we can dynamically change number of input fields
          listOfTimePointLabel.boundsLOWER<<-c(listOfTimePointLabel.boundsLOWER,list(tklabel(manualBoundsLOWERframe.InputFields, text=paste("time",as.character(i)))))

          #We need a list of Input Fields to be able to save the dynamic created tclVar's
          listOfInputFields.boundsLOWER<<-c(listOfInputFields.boundsLOWER,list(tclVar(as.character(lowerBounds[i]))))
          listOfEntries.boundsLOWER<<-c(listOfEntries.boundsLOWER, list(tkentry(manualBoundsLOWERframe.InputFields,width="11",textvariable=as.character(listOfInputFields.boundsLOWER[[i]]))))

          #put label with Input field
          tkgrid(listOfTimePointLabel.boundsLOWER[[i]],listOfEntries.boundsLOWER[[i]])
          tkgrid.configure(listOfTimePointLabel.boundsLOWER[[i]],sticky="nw")
          tkgrid.configure(listOfEntries.boundsLOWER[[i]],sticky="nw")
        }#end <--*for*
        tkgrid(manualBoundsUPPERframe,manualBoundsLOWERframe,sticky="nw")
      }
      else
      {
        tkgrid(manualBoundsUPPERframe,sticky="nw")
      }

      #put whole frame
      tkgrid(manualBoundsFrame,sticky="w")

    }#end <--*if(enterBoundsManually)*

    else #user deactivated checkbox to enter bounds manual
    {
      #fade out frame containing input fields for manual bounds and fade in truncating bounds checkbox
      tkgrid.forget(manualBoundsUPPERframe)
      tkgrid.forget(manualBoundsLOWERframe)
      tkgrid.forget(manualBoundsFrame)
      tkgrid(TruncateBoundsFrame)

      #check on symmetric or asymmetric bounds used
      if(!BoundsSymmetry==3)
      {
        #symmetric
        #remove labels and input fields and clear lists
        for(i in 1:n)
        {
          #remove labels and input fields
          tkgrid.remove(listOfTimePointLabel.boundsUPPER[[i]],listOfEntries.boundsUPPER[[i]])
        }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.boundsUPPER<<-list()
        listOfTimePointLabel.boundsUPPER<<-list()
        listOfEntries.boundsUPPER<<-list()

      }#end <--*if(!BoundsSymmetry==3)*

      else #asymmetric bounds - do the same as with symmetric for both frames
      {
        for(i in 1:n)
        {
          #remove labels and input fields and clear lists
          tkgrid.remove(listOfTimePointLabel.boundsUPPER[[i]],listOfEntries.boundsUPPER[[i]])
          tkgrid.remove(listOfTimePointLabel.boundsLOWER[[i]],listOfEntries.boundsLOWER[[i]])
        }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.boundsUPPER<<-list()
        listOfTimePointLabel.boundsUPPER<<-list()
        listOfEntries.boundsUPPER<<-list()
        listOfInputFields.boundsLOWER<<-list()
        listOfTimePointLabel.boundsLOWER<<-list()
        listOfEntries.boundsLOWER<<-list()

      }#end <--*else #asymmetric bounds - do the same as with symmetric for both frames*

      #call onBoundsChosen which will fade in the needed frames
      onBoundsChosen()
      tkgrid(computedBoundsFrame)

    }#end <--*else #user deactivated checkbox to enter bounds manual*

  }#end <--*onManualBounds <-function()*


  ####################################################################
  # functions handle a click on CONFIRM-Buttons to choose a function #
  ####################################################################
  onConfirmFunction1 <-function()
  {
    #check whether we got symmetric or asymmetric bounds
    BoundsSymmetry <<- as.numeric(tclvalue(SymmetryValue))

    #get value from listbox and ask for additional parameters if necessary
    if( BoundsSymmetry==1 || BoundsSymmetry==2)
    {
      function1<<-as.numeric(tkcurselection(listBoxFunction1of1))+1
    }
    else
    {
      function1<<-as.numeric(tkcurselection(listBoxFunction1of2))+1
    }

    #check whether user selected a function
    if( length(function1)==0 )
    {
      tkmessageBox(message="You must select a function!",icon="error",type="ok")
    }

    else # handle select
    {
      ### ASYMMETRIC ###
      if(BoundsSymmetry==3)
      {
        #first of all remove earlier input field
        tkgrid.remove(phiLabel1of2,entry.functionParameter1of2)

        #case Power Family
        if (function1==3)
        {
          #set new label and input field
          phiLabel1of2<<-tklabel(additionalParametersFrame1of2,text="Enter Paramter phi>0:")
          entry.functionParameter1of2 <<-tkentry(additionalParametersFrame1of2,width="6",textvariable=phi1of2)
          tkgrid(phiLabel1of2,sticky="w")
          tkgrid(entry.functionParameter1of2,sticky="w")
        }

        #case Hwang-Shih-DeCani family
        else if (function1==4)
             {
               #set new label and input field
               phiLabel1of2<<-tklabel(additionalParametersFrame1of2,text="Enter Parameter phi=/=0:")
               entry.functionParameter1of2 <<-tkentry(additionalParametersFrame1of2,width="6",textvariable=phi1of2)
               tkgrid(phiLabel1of2,sticky="w")
               tkgrid(entry.functionParameter1of2,sticky="w")
             }
             #case no additional parameters needed
             else
             {
               #do nothing else
             }
      }#end <--*if(BoundsSymmetry==3)*

      else ### SYMMETRIC - do the same in other frame###
      {
        #first of all remove earlier input field
        tkgrid.remove(phiLabel1of1,entry.functionParameter1of1)

        #get value from listbox and ask for additional parameters if necessary
        functionInput <<- as.numeric(tkcurselection(listBoxFunction1of1))+1

        #case Power Family
        if (function1==3)
        {
          #set new label and input field
          phiLabel1of1<<-tklabel(additionalParametersFrame1of1,text="Enter Paramter phi>0:")
          entry.functionParameter1of1 <<-tkentry(additionalParametersFrame1of1,width="6",textvariable=phi1of1)
          tkgrid(phiLabel1of1,sticky="w")
          tkgrid(entry.functionParameter1of1,sticky="w")
        }

        #case Hwang-Shih-DeCani family
        else if (function1==4)
             {
               #set new label and input field
               phiLabel1of1<<-tklabel(additionalParametersFrame1of1,text="Enter Parameter phi=/=0:")
               entry.functionParameter1of1 <<-tkentry(additionalParametersFrame1of1,width="6",textvariable=phi1of1)
               tkgrid(phiLabel1of1,sticky="w")
               tkgrid(entry.functionParameter1of1,sticky="w")
             }
             #case no additional parameters needed
             else
             {
               #do nothing else
             }
      }#end <--*else ### SYMMETRIC - do the same in other frame###     *
    }#end <--*else # handle select*
  }#end <--*onConfirmFunction1 <-function()*




  onConfirmFunction2 <-function()
  {
    #get value from listbox and ask for additional parameters if necessary
    function2<<-as.numeric(tkcurselection(listBoxFunction2of2))+1

    #check whether user selected a function
    if( length(function2)==0 )
    {
      tkmessageBox(message="You must have select a function!",icon="error",type="ok")
    }

    else # handle select
    {
      #first of all remove earlier input field
      tkgrid.remove(phiLabel2of2,entry.functionParameter2of2)

      #case Power Family
      if (function2==3)
      {
        #set new label and input field
        phiLabel2of2<<-tklabel(additionalParametersFrame2of2,text="Enter Paramter phi>0:")
        entry.functionParameter2of2 <<-tkentry(additionalParametersFrame2of2,width="6",textvariable=phi2of2)
        tkgrid(phiLabel2of2,sticky="w")
        tkgrid(entry.functionParameter2of2,sticky="w")
      }

      #case Hwang-Shih-DeCani family
      else if (function2==4)
           {
             #set new label and input field
             phiLabel2of2<<-tklabel(additionalParametersFrame2of2,text="Enter Parameter phi=/=0:")
             entry.functionParameter2of2 <<-tkentry(additionalParametersFrame2of2,width="6",textvariable=phi2of2)
             tkgrid(phiLabel2of2,sticky="w")
             tkgrid(entry.functionParameter2of2,sticky="w")
           }
           #case no additional parameters needed
           else
           {
             #do nothing else
           }
    }#end <--*else # handle select*
  }#end <--*onConfirmFunction2 <-function()*



  ##################################################################
  # function handles a click on Radio Button for ASYMMETRIC BOUNDS #
  ##################################################################
  onBoundsChosen <- function()
  {
    #check whether we got asymmetric bounds
    BoundsSymmetry <<- as.numeric(tclvalue(SymmetryValue))

    #check whether bounds are computed or entered manually by user
    #get checkbox value
    enterBoundsManually<-as.numeric(tclvalue(manualBoundsCheckBoxValue))



    #Asymmetric Bounds!
    if( (BoundsSymmetry==1 || BoundsSymmetry==2) & boundBackup!=3)
    {
      #nothing to change
    }
    else if( (BoundsSymmetry==1 || BoundsSymmetry==2) & boundBackup==3)
         {
           #if users enters bounds manually update frames, if necessary
           if(enterBoundsManually)
           {
             tkgrid.forget(manualBoundsUPPERframe)
             tkgrid.forget(manualBoundsLOWERframe)
             tkgrid.forget(manualBoundsFrame)
             onManualBounds()
           }
           #exchange frames
           tkgrid.remove(nonSymmetricBoundsFrame)
           tkgrid(symmetricBoundsFrame,sticky="nw")
         }
         else if(BoundsSymmetry==3 & boundBackup!=3)
              {
                #if users enters bounds manually update frames, if necessary
                if(enterBoundsManually)
                {
                  tkgrid.forget(manualBoundsUPPERframe)
                  tkgrid.forget(manualBoundsLOWERframe)
                  tkgrid.forget(manualBoundsFrame)
                  onManualBounds()
                }
                #exchange frames
                tkgrid.remove(symmetricBoundsFrame)
                tkgrid(nonSymmetricBoundsFrame,sticky="nw")

              }
  #update boundBackup
  boundBackup<<-BoundsSymmetry
  }


  #######################################################
  # function handles a click on Trunate Bounds Checkbox #
  #######################################################
  onTruncateCheckbox <- function()
  {
    #checkbox activated?
    truncateBoundsYesNo <<- as.logical(as.numeric(tclvalue(TruncateBoundsCheckBoxValue)))

    if(truncateBoundsYesNo) #activated
    {
      ##grid edit box
      tkgrid(TruncateDynamicFrame)
    }
    else #deactivated
    {
      #ungrid edit box
      tkgrid.forget(TruncateDynamicFrame)
    }
  }


  ##################################################
  # function handles a click on 'CALCULATE'-Button #
  ##################################################
  OnCalculateInputTask1 <- function()
  {
    readyForCalculate <- TRUE

    #get values from checkboxes, listboxes and radiobuttons
    equallySpacedTimesInput <<- as.logical(as.numeric(tclvalue(equallySpacedTimesCheckBoxValue)))
    secondTimeScaleIsUsedInput <<- as.logical(as.numeric(tclvalue(secondTimeScaleIsUsedCheckBoxValue)))
    BoundsSymmetry <<- as.numeric(tclvalue(SymmetryValue))
    truncateBoundsYesNo <<- as.logical(as.numeric(tclvalue(TruncateBoundsCheckBoxValue)))


    #get and check power input to be in (0,1]
    confidenceLevel<<-as.numeric(tclvalue(powerTclVar))
    if( !( confidenceLevel>0 & confidenceLevel<1) )
    {
      readyForCalculate<-FALSE
      tkmessageBox(message="Incorrect Power value entered - please correct!",icon="error",type="ok")
    }


    #truncation point set?
    if(truncateBoundsYesNo)
    {
      TruncateBoundsInput <<- abs(as.numeric(tclvalue(boundsTruncation)))
    }
    else
    {
      TruncateBoundsInput<-8
    }

    #evaluate whether function is used to compute bounds or user entered them manually
    if(enterBoundsManually)
    {
      #manually entered bounds
      for(i in 1:n)
      {
        upperBounds[i]<<- as.numeric(tclvalue(listOfInputFields.boundsUPPER[[i]]))
      }

      #elicit lower bounds
      if(BoundsSymmetry==1)
      {
        #one-sided => lower Bounds == -8 (that is -infinity)
        lowerBounds <<- seq(-8,-8,length=n)
      }
      else if(BoundsSymmetry==2)
           {
             #two-sided symmetric
             lowerBounds <<- -upperBounds
           }
           else
           {
             #asymmetric
             for(i in 1:n)
             {
               lowerBounds[i]<<- as.numeric(tclvalue(listOfInputFields.boundsLOWER[[i]]))
             }
           }
    }#end <--*if(enterBoundsManually)*

    else #bounds are computed
    {
      #get chosen function(s) and check alpha
      ###################
      # case asymmetric #
      ###################
      if(BoundsSymmetry==3)
      {
        alpha1 <<- as.numeric(tclvalue(alpha1of2))
        alpha2 <<- as.numeric(tclvalue(alpha2of2))

        #check alpha
        alphaAll<- alpha1 + alpha2
        if( !(alpha1>=0 & alpha1<=1 & alpha2>=0 & alpha2<=1 & alphaAll<=1) )
        {
          readyForCalculate<-FALSE
          tkmessageBox(message="Alpha out of range! Correct it and try again.",icon="error",type="ok")
        }

        #check phi if entered as parameter
        phi1 <<- as.numeric(tclvalue(phi1of2))
        phi2 <<- as.numeric(tclvalue(phi2of2))
        #function for UPPER bounds
        if(function1==3)
        {
          if( !(phi1>0) )
          {
            readyForCalculate<-FALSE
            tkmessageBox(message="Parameter phi in function for UPPER bounds must be >0 !",icon="error",type="ok")
          }
        }
        else if(function1==4)
             {
               if(phi1==0)
               {
                 readyForCalculate<-FALSE
                 tkmessageBox(message="Parameter phi in function for UPPER bounds may NOT be zero!",icon="error",type="ok")
               }
             }

        #same with function for LOWER bounds
        if(function2==3)
        {
          if( !(phi2>0) )
          {
            readyForCalculate<-FALSE
            tkmessageBox(message="Parameter phi in function for LOWER bounds must be >0 !",icon="error",type="ok")
          }
        }
        else if(function2==4)
             {
               if(phi2==0)
               {
                 readyForCalculate<-FALSE
                 tkmessageBox(message="Parameter phi in function for LOWER bounds may NOT be zero!",icon="error",type="ok")
               }
             }
      }#end <--*if(BoundsSymmetry==3)*

      ##################
      # case symmetric #
      ##################
      else #one function cause of symmetric bounds
      {
        alpha1 <<- as.numeric(tclvalue(alpha1of1))
        #check alpha
        if( !(alpha1>=0 & alpha1<=1) )
        {
          readyForCalculate<-FALSE
          tkmessageBox(message="Alpha out of range! Correct it and try again.",icon="error",type="ok")
        }

        #check phi if entered as parameter
        phi1 <<- as.numeric(tclvalue(phi1of1))
        #what function used?
        if(function1==3)
        {
          if( !(phi1>0) )
          {
            readyForCalculate<-FALSE
            tkmessageBox(message="Parameter phi in function must be >0 !",icon="error",type="ok")
          }
        }
        else if(function1==4)
             {
               if(phi1==0)
               {
                 readyForCalculate<-FALSE
                 tkmessageBox(message="Parameter phi in function may NOT be zero!",icon="error",type="ok")
                 }
             }
      }

    }#end <--*else #bounds are computed*
    ##if user typed in unequally spaced times - get them and
    ##check them to be in intervall(0,1] and in right order
    if(!equallySpacedTimesInput)
    {
      tempVal<-0
      interimTimesBad<-FALSE
      for(i in 1:n)
      {
        tempVal[i] <- as.numeric(tclvalue(listOfInputFields.unequalTimes[[i]]))

        if (tempVal[i]<=0) { interimTimesBad<-TRUE }
        if (tempVal[i]>1) { interimTimesBad<-TRUE }
        if (i>1)
        {
          if (tempVal[i]<=tempVal[i-1]) { interimTimesBad<-TRUE }
        }
      }#end <--*for(i in 1:n)*

      ##if times are not good => error and keep old times
        if(interimTimesBad)
        {
          readyForCalculate <- FALSE
          tkmessageBox(message="Bad Interim Times entered - old Times are kept so far! ",icon="error",type="ok")
        }
        ##else take new times
        else
        {
          t<<-tempVal
        }

    }#end <--*if(equallySpacedTimesInput)*
    else
    {
     for(i in 1:n)
     {
       t[i]<<-i/n
     }
    }

    ##if user typed in second time scales - get them and
    ##check them to be in intervall(0,1] and in right order
    if(secondTimeScaleIsUsedInput)
    {
      tempVal<-0
      secondTimesBad<-FALSE
      for(i in 1:n)
      {
        tempVal[i] <- as.numeric(tclvalue(listOfInputFields.secondTimes[[i]]))

        if (tempVal[i]<=0) { secondTimesBad<-TRUE }
        if (tempVal[i]>1) { secondTimesBad<-TRUE }
        if (i>1)
        {
          if (tempVal[i]<=tempVal[i-1]) { secondTimesBad<-TRUE }
        }
      }#end <--*for(i in 1:n)*

      ##if times are not good => error and keep old times
        if(secondTimesBad)
        {
          readyForCalculate <- FALSE
          tkmessageBox(message="Bad Second Time Scale entered - old Times are kept so far! ",icon="error",type="ok")
        }
        ##else take new times
        else
        {
          t2<<-tempVal
        }

      if(readyForCalculate)
      {

        ###########################################################################
        ############### RESCALE SECOND TIME SCALE IF NECESSARY ####################
        ###########################################################################
        # When second time scale is not on (0,1], computation with
        # non-zero drift parameters are incorrect, since the drift
        # always scaled to (0,1].  If the trial is complete (t[n]=1)
        # then t2 can be rescaled as t3 = t2/t2[n].  Otherwise, if
        # the second time scale is to be used for covariances, the
        # user must enter a maximum value.
        #
        # drift[ t[i] ] = drift*t[i]
        #
        # Start with t3 = t2.
        # (t2=t by default e.g. if user did not enter second time scale.)
        t3<<-t2
        t2max<<-0

        ##If t[n]=1, t2[n] is maximum of t2.
        if(t[n]==1)
        {
         tkmessageBox(title="-2- Compute Drift given Power and Bounds",message="Second Time scale will be used to determine covariances.",icon="info",type="ok")
         t2max<<-t2[n]
         t3<<-t2/t2max
        }
        else ##Should we try to use 2nd scale?
        {
          response<-tkmessageBox(message="Do you wish to use the 2nd time scale to determine covariances?",
                                 icon="question",type="yesno",default="yes")


          ##--If yes, prompt for maximum of 2nd time scale.--##
          if( as.character(tclvalue(response))=="yes" )
          {
            ##################################################
            # function handles prompting for t2max if needed #
            ##################################################
            t2maxPrompt <- function(title,question,entryInit,entryWidth=4,returnValOnCancel="ID_CANCEL")
            {
          dlg <- tktoplevel(task2)
              tkwm.deiconify(dlg)
              tkgrab.set(dlg)
              tkfocus(dlg)
              tkwm.title(dlg,title)
              textEntryVarTcl <- tclVar(paste(entryInit))
              textEntryWidget <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl)
              tkgrid(tklabel(dlg,text="       "))
              tkgrid(tklabel(dlg,text=question),textEntryWidget)
              tkgrid(tklabel(dlg,text="       "))
              ReturnVal <- returnValOnCancel
              onOK <- function()
              {
                ReturnVal <<- as.numeric(tclvalue(textEntryVarTcl))
                #check whether numeric was entered
                if(is.na(ReturnVal))
                {
                  tkmessageBox(title="ERROR",message="You did not enter a valid numeric value!",icon="error",type="ok")
                }
                #if input numeric check whether the numeric is a valid entry
                else
                {
                  if(ReturnVal<=0)
                  {
                    tkmessageBox(title="ERROR",message="Maximum must be positive! Please try again!",icon="error",type="ok")
                  }
                  else if(ReturnVal<t2[n])
                       {
                         tkmessageBox(title="ERROR",message="The Maximum cannot be smaller than your last seond time scale value!",icon="error",type="ok")
                       }

                       #input ok - go back to main window
                       else
                       {
                         tkgrab.release(dlg)
                         tkdestroy(dlg)
                         tkfocus(task2)
                       }
                }

              }
              onCancel <- function()
              {
                readyForCalculate<<-FALSE
                ReturnVal <<- returnValOnCancel
                tkgrab.release(dlg)
                tkdestroy(dlg)
                tkfocus(task2)
              }
              OK.but     <-tkbutton(dlg,text="   OK   ",command=onOK)
              Cancel.but <-tkbutton(dlg,text=" Cancel ",command=onCancel)
              tkgrid(OK.but,Cancel.but)
              tkgrid(tklabel(dlg,text="    "))

              tkfocus(dlg)
              tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkfocus(task2)})
              tkbind(textEntryWidget, "<Return>", onOK)
              tkwait.window(dlg)

              return(ReturnVal)

            }#end <--*t2maxPrompt <- function(...)*


            ReturnVal<-t2maxPrompt("Second Time Scale will be used","Enter the maximum value of the second time scale","" )
            if(ReturnVal!="ID_CANCEL")
            {
              t2max<<-ReturnVal
              #Rescale t2
              t3<<-t2/t2max
            }

          }#end <--*if( as.character(tclvalue(response))=="yes" )*

          else if( as.character(tclvalue(response))=="no" )
          {
            ##Even if the 2nd time scale was on (0,1], if the
            ##maximum information value was not entered, set
            ##t3 to t, which causes t2 to be ignored!#
            t3<<-t

          }

        }#end <--*else ##Should we try to use 2nd scale?*

      }#end <--*if(readyForCalculate)*

    }#end <--*if(secondTimeScaleIsUsedInput)*
    else
    {
      ##set t3 to t, which causes t2 to be ignored!
      t3<<-t
    }

    if(readyForCalculate)
    {
      # second time scale is not used so far --> set t3=t2=t
      t2<-t; t3<-t
      calculateTask2(n,nMax,t,t2,t2max,t3,confidenceLevel,drift,equallySpacedTimesInput,secondTimeScaleIsUsedInput,
                     BoundsSymmetry, c(alpha1,alpha2), c(phi1,phi2), c(function1,function2),TruncateBoundsInput,
                    enterBoundsManually, upperBounds, lowerBounds, taskWindow )
    }
    else
    {
      tkmessageBox(message="Error - maybe you forgot to CONFIRM your function(s)",icon="error",type="ok")
    }
  }#end <--*OnCalculateInputTask1 <- function()*



#######################################################################################################
#################------------------------------------------------------------------####################
##################  FROM HERE ON LABELS AND FRAMES ARE CREATED AND PUT TOGETHER   #####################
#################------------------------------------------------------------------####################
#######################################################################################################

  #Set Toplevel
  task2 <- tktoplevel(taskWindow)
  tkwm.title(task2,"-2- Compute Drift given Power and Bounds")

  #Define main Frame
  InputTask2 <- tkframe(task2, relief="groove",borderwidth=2)


  ##--------------------------------------------------------------------------------##
  ##------------------------  number of Interim Times  -----------------------------##
  ##--------------------------------------------------------------------------------##

  #create pull down menu to select interim analyses from n==1 to n==25(=nMax)
  topMenu <- tkmenu(task2)
  tkconfigure(task2,menu=topMenu)
  nMenu <- tkmenu(topMenu,tearoff=FALSE,background="grey",activebackground="red")
  tkadd(nMenu,"command",label="1",command=function() onChangeInterimAnalyses(1))
  tkadd(nMenu,"command",label="2",command=function() onChangeInterimAnalyses(2))
  tkadd(nMenu,"command",label="3",command=function() onChangeInterimAnalyses(3))
  tkadd(nMenu,"command",label="4",command=function() onChangeInterimAnalyses(4))
  tkadd(nMenu,"command",label="5",command=function() onChangeInterimAnalyses(5))
  tkadd(nMenu,"command",label="6",command=function() onChangeInterimAnalyses(6))
  tkadd(nMenu,"command",label="7",command=function() onChangeInterimAnalyses(7))
  tkadd(nMenu,"command",label="8",command=function() onChangeInterimAnalyses(8))
  tkadd(nMenu,"command",label="9",command=function() onChangeInterimAnalyses(9))
  tkadd(nMenu,"command",label="10",command=function() onChangeInterimAnalyses(10))
  tkadd(nMenu,"command",label="11",command=function() onChangeInterimAnalyses(11))
  tkadd(nMenu,"command",label="12",command=function() onChangeInterimAnalyses(12))
  tkadd(nMenu,"command",label="13",command=function() onChangeInterimAnalyses(13))
  tkadd(nMenu,"command",label="14",command=function() onChangeInterimAnalyses(14))
  tkadd(nMenu,"command",label="15",command=function() onChangeInterimAnalyses(15))
  tkadd(nMenu,"command",label="16",command=function() onChangeInterimAnalyses(16))
  tkadd(nMenu,"command",label="17",command=function() onChangeInterimAnalyses(17))
  tkadd(nMenu,"command",label="18",command=function() onChangeInterimAnalyses(18))
  tkadd(nMenu,"command",label="19",command=function() onChangeInterimAnalyses(19))
  tkadd(nMenu,"command",label="20",command=function() onChangeInterimAnalyses(20))
  tkadd(nMenu,"command",label="21",command=function() onChangeInterimAnalyses(21))
  tkadd(nMenu,"command",label="22",command=function() onChangeInterimAnalyses(22))
  tkadd(nMenu,"command",label="23",command=function() onChangeInterimAnalyses(23))
  tkadd(nMenu,"command",label="24",command=function() onChangeInterimAnalyses(24))
  tkadd(nMenu,"command",label="25",command=function() onChangeInterimAnalyses(25))
  tkadd(topMenu,"cascade",label=paste("#Interim Times: K= ",as.character(n)),menu=nMenu)

  tkgrid(tklabel(InputTask2,text="")) # Blank line


  ##--------------------------------------------------------------------------------##
  ##-------------  Interim Times equally or unequally spaced? ----------------------##
  ##-------------       Second Time Scale will be used?       ----------------------##
  ##--------------------------------------------------------------------------------##

  ## prepare Frames
  interimTimesFrame<- tkframe(InputTask2,relief="groove",borderwidth=0)
  equallySpacedTimesFrame <- tkframe(interimTimesFrame,relief="groove",borderwidth=0)
  secondTimesFrame <- tkframe(interimTimesFrame,relief="groove",borderwidth=0)

  #again we need a frame for dynamic working in it to not affect
  #the format of 'equallySpacedTimesFrame' respective 'secondTimesFrame'
  equallySpacedLabelFrame<-tkframe(equallySpacedTimesFrame,relief="groove",borderwidth=0)
  unEquallyDynamicFrame<-tkframe(equallySpacedTimesFrame,relief="groove",borderwidth=0)
  secondTimesLabelFrame<-tkframe(secondTimesFrame,relief="groove",borderwidth=0)
  secondTimesDynamicFrame<-tkframe(secondTimesFrame,relief="groove",borderwidth=0)

  #Default is Equally Spaced Times and no Second Time Scale
  #create Checkboxes
  #equally spaced
  equallySpacedTimesCheckBox<-tkcheckbutton(equallySpacedLabelFrame,command=onCheckBoxEquallySpacedTimes)
  equallySpacedTimesCheckBoxValue <- tclVar(as.character(as.numeric(equallySpacedTimesInput)))
  tkconfigure(equallySpacedTimesCheckBox,variable=equallySpacedTimesCheckBoxValue)
  #second time scale
  secondTimeScaleIsUsedCheckBox<-tkcheckbutton(secondTimesLabelFrame,command=onCheckBoxSecondTimeScale)
  secondTimeScaleIsUsedCheckBoxValue <- tclVar(as.character(as.numeric(secondTimeScaleIsUsedInput)))
  tkconfigure(secondTimeScaleIsUsedCheckBox,variable=secondTimeScaleIsUsedCheckBoxValue)

  #put checkbox and other frames together
  equallyTimesBoxLabel<-tklabel(equallySpacedLabelFrame,text="Equally Spaced Times")
  secondTimesBoxLabel<-tklabel(secondTimesLabelFrame,text="Use Second Time Scale")
  tkgrid(equallyTimesBoxLabel,equallySpacedTimesCheckBox)
  # tkgrid(secondTimesBoxLabel,secondTimeScaleIsUsedCheckBox) - this feature is currently removed
  tkgrid(equallySpacedTimesFrame,secondTimesFrame,sticky="n")
  tkgrid(equallySpacedLabelFrame,sticky="n")
  tkgrid(secondTimesLabelFrame,sticky="n")
  tkgrid(unEquallyDynamicFrame,sticky="nw")
  tkgrid(secondTimesDynamicFrame,sticky="n")
  tkgrid(interimTimesFrame,sticky="w")
  tkgrid(tklabel(InputTask2,text="")) # Blank line

  #Desired Power
  powerTclVar<-tclVar(as.character(confidenceLevel))
  powerFrame <- tkframe(InputTask2,relief="groove",borderwidth=0)
  powerLabel<-tklabel(powerFrame,text="Desired Power - it must be in (0,1) :")
  entry.power <-tkentry(powerFrame,width="6",textvariable=powerTclVar)
  #grid it
  tkgrid(powerLabel,entry.power, sticky="w")
  tkgrid(tklabel(powerFrame,text="")) # Blank line
  tkgrid(powerFrame,sticky="w")

  ###One- or Two-Sided Bounds or asymmetric Bounds###
  #create frames
  boundsLabelFrame <- tkframe(InputTask2,relief="groove",borderwidth=0)
  boundsRadioButtonFrame <- tkframe(InputTask2,relief="groove",borderwidth=0)

  #create radio buttons
  oneSided <- tkradiobutton(boundsRadioButtonFrame,command=onBoundsChosen)
  twoSided <- tkradiobutton(boundsRadioButtonFrame,command=onBoundsChosen)
  asymmetric <- tkradiobutton(boundsRadioButtonFrame,command=onBoundsChosen)
  SymmetryValue <- tclVar(as.character(BoundsSymmetry))
  tkconfigure(oneSided,variable=SymmetryValue,value="1")
  tkconfigure(twoSided,variable=SymmetryValue,value="2")
  tkconfigure(asymmetric,variable=SymmetryValue,value="3")

  #grid labels and buttons together
  tkgrid(tklabel(boundsLabelFrame,text="One-, Two-sided-Symmetric or Asymmetric Bounds?"),sticky="w")
  tkgrid(tklabel(boundsRadioButtonFrame,text="One-Sided "),oneSided)
  tkgrid(tklabel(boundsRadioButtonFrame,text="Two-Sided "),twoSided)
  tkgrid(tklabel(boundsRadioButtonFrame,text="Asymmetric "),asymmetric)

  #put frames
  tkgrid(boundsLabelFrame,sticky="w")
  tkgrid(boundsRadioButtonFrame,sticky="w")
  tkgrid(tklabel(InputTask2,text="")) # Blank line

  ################################################################################
  ### User could enter bounds manually instead of let them be computed (default)##
  ################################################################################
  manualOrComputedBoundsFrame<-tkframe(InputTask2,relief="groove",borderwidth=0)
  LabelManualOrComputedBoundsFrame<-tkframe(manualOrComputedBoundsFrame,relief="groove",borderwidth=0)
  computedBoundsFrame<-tkframe(manualOrComputedBoundsFrame,relief="groove",borderwidth=0)
  manualBoundsFrame<-tkframe(manualOrComputedBoundsFrame,relief="groove",borderwidth=0)
  #upper bounds
  manualBoundsUPPERframe<-tkframe(manualBoundsFrame,relief="groove",borderwidth=0)
  manualBoundsUPPERframe.Label<-tkframe(manualBoundsUPPERframe,relief="groove",borderwidth=0)
  manualBoundsUPPERframe.InputFields<-tkframe(manualBoundsUPPERframe,relief="groove",borderwidth=0)
  #lower bounds
  manualBoundsLOWERframe<-tkframe(manualBoundsFrame,relief="groove",borderwidth=0)
  manualBoundsLOWERframe.Label<-tkframe(manualBoundsLOWERframe,relief="groove",borderwidth=0)
  manualBoundsLOWERframe.InputFields<-tkframe(manualBoundsLOWERframe,relief="groove",borderwidth=0)

  #create checkbox with label
  manualBoundsCheckbox <- tkcheckbutton(LabelManualOrComputedBoundsFrame,command=onManualBounds)
  manualBoundsCheckBoxValue <- tclVar(as.character(as.numeric(enterBoundsManually)))
  tkconfigure(manualBoundsCheckbox,variable=manualBoundsCheckBoxValue)
  manualBoundsCheckboxLabel<-tklabel(LabelManualOrComputedBoundsFrame,text="Enter Bounds Manually")
  tkgrid(manualBoundsCheckboxLabel,manualBoundsCheckbox)
  tkgrid(tklabel(LabelManualOrComputedBoundsFrame,text="")) # Blank line
  tkgrid.configure(manualBoundsCheckboxLabel,sticky="w")
  tkgrid.configure(manualBoundsCheckbox,sticky="w")

  #create labels for manual entering and grid alltogether
  manualBoundsUPPERlabel<-tklabel(manualBoundsUPPERframe.Label,text="Enter UPPER Bounds (standardized)     ")
  tkgrid(manualBoundsUPPERlabel,sticky="w")
  tkgrid(manualBoundsUPPERframe.Label,sticky="w")
  tkgrid(manualBoundsUPPERframe.InputFields,sticky="w")
  manualBoundsLOWERlabel<-tklabel(manualBoundsLOWERframe.Label,text="Enter LOWER Bounds (standardized)")
  tkgrid(manualBoundsLOWERlabel,sticky="w")
  tkgrid(manualBoundsLOWERframe.Label,sticky="w")
  tkgrid(manualBoundsLOWERframe.InputFields,sticky="w")

  ### Significance Level(s) alpha and function(s) to be used to calculate bounds###
  ## if user choses asymmetric bounds two different functions could be used ##
  ##create Frames
  alphaAndFunctionsFrame<-tkframe(computedBoundsFrame,relief="groove",borderwidth=0)
  symmetricBoundsFrame<-tkframe(alphaAndFunctionsFrame,relief="groove",borderwidth=0)
  nonSymmetricBoundsFrame<-tkframe(alphaAndFunctionsFrame,relief="groove",borderwidth=0)

  ##Default alpha1==0.05, alpha2==0
  alpha1of1 <- tclVar(as.character(alphaInput))
  alpha1of2 <- tclVar(as.character("0.025"))
  alpha2of2 <- tclVar(as.character("0.025"))

  #########################################################
  ### case symmetric bounds or one-sided test (default) ###
  #########################################################
  #frames
  alphaFrame1of1 <- tkframe(symmetricBoundsFrame,relief="groove",borderwidth=0)
  functionsFrame1of1 <- tkframe(symmetricBoundsFrame,relief="groove",borderwidth=0)
  additionalParametersFrame1of1<-tkframe(symmetricBoundsFrame,relief="groove",borderwidth=0)

  ##create Labels for alpha
  alphaLabel1of1<-tklabel(alphaFrame1of1,text="Significance Level: alpha=")
  entry.alpha1of1 <-tkentry(alphaFrame1of1,width="6",textvariable=alpha1of1)

  #create Listbox for function choice
  functionLabel1of1<-tklabel(functionsFrame1of1,text="What function should be used?")
  listBoxFunction1of1<-tklistbox(functionsFrame1of1,height=5,width=30,selectmode="single",background="grey")
  functionChoice1of1 <- c("(1) O'Brien-Fleming Type","(2) Pocock Type",
    "(3) Power Family: alpha* t^phi","(4) Hwang-Shih-DeCani Family","(5) Exact Pocock Bounds")
  for (i in (1:5))
  {
    tkinsert(listBoxFunction1of1,"end",functionChoice1of1[i])
  }
  tkselection.set(listBoxFunction1of1, functionInput-1)  # Default function is O'Brien-Fleming Type.  Indexing starts at zero.

  #create and put button to confirm a function because for example in case of 'Power family: alpha* t^phi'
  #user has to enter additional parameter 'phi'
  confirmFun.button1of1 <-tkbutton(functionsFrame1of1,text=" CONFIRM FUNCTION ",command=onConfirmFunction1)

  #create variable for edit box which we will need if additional parameters must be entered
  #edit box is unvisible at beginning since default function O'Brien-Fleming Type does not need any additional parameters
  phi1of1 <- tclVar(as.character(phi1))
  phiLabel1of1<-tklabel(additionalParametersFrame1of1,text="")
  entry.functionParameter1of1 <-tkentry(additionalParametersFrame1of1,width="3",textvariable=phi1of1)

  #grid together
  #alpha
  tkgrid(alphaLabel1of1,entry.alpha1of1)
  tkgrid.configure(alphaLabel1of1,sticky="w")
  tkgrid.configure(entry.alpha1of1,sticky="w")
  tkgrid(tklabel(alphaFrame1of1,text="")) # Blank line
  tkgrid(functionLabel1of1,sticky="w")
  tkgrid(listBoxFunction1of1)

  #put frames and button
  tkgrid(alphaFrame1of1,sticky="w")
  tkgrid(functionsFrame1of1,additionalParametersFrame1of1)
  tkgrid(confirmFun.button1of1)
  tkgrid.configure(functionsFrame1of1,sticky="w")
  tkgrid(tklabel(symmetricBoundsFrame,text="")) # Blank line

  #Finally grid frame for symmetric case as default
  tkgrid(LabelManualOrComputedBoundsFrame,sticky="w")
  tkgrid(symmetricBoundsFrame,sticky="nw")
  tkgrid(alphaAndFunctionsFrame,sticky="w")
  tkgrid(computedBoundsFrame,sticky="w")
  tkgrid(manualOrComputedBoundsFrame,sticky="w")


  ##############################
  ### case Asymmetric bounds ###
  ##############################
  #frames
  alphaFrame1of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  functionsFrame1of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  additionalParametersFrame1of2<-tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  alphaFrame2of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  functionsFrame2of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  additionalParametersFrame2of2<-tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)

  ##create Labels for alpha
  alphaLabel1of2<-tklabel(alphaFrame1of2,text="UPPER Bounds: alpha=")
  entry.alpha1of2 <-tkentry(alphaFrame1of2,width="6",textvariable=alpha1of2)

  alphaLabel2of2<-tklabel(alphaFrame2of2,text="LOWER Bounds: alpha=")
  entry.alpha2of2 <-tkentry(alphaFrame2of2,width="6",textvariable=alpha2of2)

  #create Listboxes for function choice
  #################
  # List Box 1of2 #
  #################
  functionLabel1of2<-tklabel(functionsFrame1of2,text="Choose Function for UPPER Bounds")
  listBoxFunction1of2<-tklistbox(functionsFrame1of2,height=5,width=30,selectmode="single",background="grey")
  functionChoice1of2 <- c("(1) O'Brien-Fleming Type","(2) Pocock Type",
    "(3) Power Family: alpha* t^phi","(4) Hwang-Shih-DeCani Family","(5) Exact Pocock Bounds")
  for (i in (1:5))
  {
    tkinsert(listBoxFunction1of2,"end",functionChoice1of2[i])
  }

  #create and put first Confirm button which "commands" same function as in symmetric case did
  confirmFun.button1of2 <-tkbutton(functionsFrame1of2,text=" CONFIRM FUNCTION ",command=onConfirmFunction1)
  #edit box for parameter phi[1]
  phi1of2 <- tclVar(as.character(phi1))
  phiLabel1of2<-tklabel(additionalParametersFrame1of2,text="")
  entry.functionParameter1of2 <-tkentry(additionalParametersFrame1of2,width="3",textvariable=phi1of2)

  #################
  # List Box 2of2 #
  #################
  functionLabel2of2<-tklabel(functionsFrame2of2,text="Choose Function for LOWER Bounds")
  listBoxFunction2of2<-tklistbox(functionsFrame2of2,height=5,width=30,selectmode="single",background="grey")
  functionChoice2of2 <- c("(1) O'Brien-Fleming Type","(2) Pocock Type",
    "(3) Power Family: alpha* t^phi","(4) Hwang-Shih-DeCani Family","(5) Exact Pocock Bounds")
  for (i in (1:5))
  {
    tkinsert(listBoxFunction2of2,"end",functionChoice2of2[i])
  }

  #create and put first Confirm button
  confirmFun.button2of2 <-tkbutton(functionsFrame2of2,text=" CONFIRM FUNCTION ",command=onConfirmFunction2)

  #edit box for parameter phi[2]
  phi2of2 <- tclVar(as.character(phi2))
  phiLabel2of2<-tklabel(additionalParametersFrame2of2,text="")
  entry.functionParameter2of2 <-tkentry(additionalParametersFrame2of2,width="3",textvariable=phi2of2)

  #grid together

  #1of2
  tkgrid(alphaLabel1of2,entry.alpha1of2)
  tkgrid.configure(alphaLabel1of2,sticky="w")
  tkgrid.configure(entry.alpha1of2,sticky="w")
  tkgrid(functionLabel1of2,sticky="w")
  tkgrid(listBoxFunction1of2)

  #put frames and button
  tkgrid(alphaFrame1of2,sticky="w")
  tkgrid(functionsFrame1of2,additionalParametersFrame1of2)
  tkgrid(confirmFun.button1of2)
  tkgrid.configure(functionsFrame1of2,sticky="w")
  tkgrid(tklabel(nonSymmetricBoundsFrame,text="")) # Blank line


  #2of2
  tkgrid(alphaLabel2of2,entry.alpha2of2)
  tkgrid.configure(alphaLabel2of2,sticky="w")
  tkgrid.configure(entry.alpha2of2,sticky="w")
  tkgrid(functionLabel2of2,sticky="w")
  tkgrid(listBoxFunction2of2)

  #put frames and button
  tkgrid(alphaFrame2of2,sticky="w")
  tkgrid(functionsFrame2of2,additionalParametersFrame2of2)
  tkgrid(confirmFun.button2of2)
  tkgrid.configure(functionsFrame2of2,sticky="w")
  tkgrid(tklabel(nonSymmetricBoundsFrame,text="")) # Blank line

  #################################################################

  ###Truncate Bounds?###
  PositionTruncateBoundsFrame<-tkframe(InputTask2,relief="groove",borderwidth=0)
  TruncateBoundsFrame <- tkframe(PositionTruncateBoundsFrame,relief="groove",borderwidth=0)
  TruncateLabelFrame <- tkframe(TruncateBoundsFrame,relief="groove",borderwidth=0)
  TruncateDynamicFrame <-tkframe(TruncateBoundsFrame,relief="groove",borderwidth=0)

  #create checkbox
  TruncateBoundsCheckBox<-tkcheckbutton(TruncateLabelFrame,command=onTruncateCheckbox)
  TruncateBoundsCheckBoxValue <- tclVar(as.character(as.numeric(truncateBoundsYesNo)))
  tkconfigure(TruncateBoundsCheckBox,variable=TruncateBoundsCheckBoxValue)

  #create variable for edit box which we will need if user wants truncation of bounds -
  boundsTruncation <- tclVar(as.character(TruncateBoundsInput))
  boundsTruncationLabel<-tklabel(TruncateDynamicFrame,text="Enter Truncation Point:")
  entry.truncationValue <-tkentry(TruncateDynamicFrame,width="3",textvariable=boundsTruncation)

  #put frames
  tkgrid(tklabel(TruncateLabelFrame,text="Truncate standardized Bounds?"),TruncateBoundsCheckBox)
  tkgrid(boundsTruncationLabel,entry.truncationValue,sticky="w")
  tkgrid(TruncateLabelFrame,TruncateDynamicFrame,sticky="w")
  tkgrid(TruncateBoundsFrame,sticky="w")
  tkgrid(PositionTruncateBoundsFrame,sticky="w")
  tkgrid.forget(TruncateDynamicFrame) #default is no Truncating
  tkgrid(tklabel(InputTask2,text="")) # Blank line

  ##put Overall Frame
  tkgrid(InputTask2)

  #frame for the buttons
  buttonFrame<-tkframe(task2,relief="groove",borderwidth=0)

  #create and put button for calculating
  calculate.button <-tkbutton(buttonFrame,text=" CALCULATE ",command=OnCalculateInputTask1)

  # function handles click onto button to Cancel i.e. close current window
  onCancel <- function()
  {
   tkdestroy(task2)
  }
  cancel.button <-tkbutton(buttonFrame,text=" Cancel ",command=onCancel)

  # grid buttons
  tkgrid( tklabel(buttonFrame, text=""))   #blank line
  tkgrid(calculate.button, tklabel(buttonFrame, text="            "),
         cancel.button, sticky="we" )
  tkgrid( tklabel(buttonFrame, text=""))   #blank line
  tkgrid(buttonFrame)

  tkfocus(task2)

}


"guiInputTask3" <-
function(taskWindow)
{

  ### Initialize Variables ###
  #default inputs
  nMax<-25 # Number of interim analyses is limited to 25
  n<-1 # number of interim analyses
  alphaInput<-0.05 # 'alphaInput' is input of the desired overall size.
  t<-1  # vector containing interim analyses
  t2<-t # vector containing second time scale - by default t2==t
  t3<-t2 # t3 is t2 divided by the maximum information, if needed
  t2max<-0 # maximum value of t2
  drift<-0 # drift by default 0
  upperBounds<-0 # vector containing upper bounds
  lowerBounds<-0 # vector containing lower bounds
  BoundsSymmetry<-1 # BoundsSymmetry==1 means one-sided bounds, BoundsSymmetry==2 means two-sided symmetric and BoundsSymmetry==3 means asymmetric bounds.
  functionInput<-1 # indicates type I error spending rate function e.g. the function(s) the user choosed
  TruncateBoundsInput<-8 ## here 8 equals infity

  #some status variables (names are self-explanatory)
  nInputEdited<-FALSE
  equallySpacedTimesInput<-TRUE
  secondTimeScaleIsUsedInput<-FALSE
  enterBoundsManually<-FALSE
  truncateBoundsYesNo<-FALSE # default is no truncating of bounds

  #some lists (names are self-explanatory)
  listOfTimePointLabel.unequalTimes<-list() #
  listOfInputFields.unequalTimes<-list()
  listOfEntries.unequalTimes<-list()

  listOfTimePointLabel.secondTimes<-list()
  listOfInputFields.secondTimes<-list()
  listOfEntries.secondTimes<-list()

  listOfTimePointLabel.boundsUPPER<-list() #
  listOfInputFields.boundsUPPER<-list()
  listOfEntries.boundsUPPER<-list()

  listOfTimePointLabel.boundsLOWER<-list() #
  listOfInputFields.boundsLOWER<-list()
  listOfEntries.boundsLOWER<-list()

  #operating variables
  nBackup<-n # backup n
  boundBackup<-BoundsSymmetry # backup BoundsSymmetry
  alpha1<- alphaInput # set alpha
  alpha2<- 0 # alpha2 is need in case of asymmetric bounds
  function1<-functionInput # set function1
  function2<-functionInput # function2 is need in case of asymmetric bounds
  phi1<-1 # optional Parameter referring to Power Family
  phi2<-1 # phi2 is need in case of asymmetric bounds both using Power family

  #Define some Fonts
  fontItalic <- tkfont.create(family="times",size=10,slant="italic")
  fontBig <- tkfont.create(family="times",size=10,weight="bold")


####################################################################################################
#################------------------------------------------------------------#######################
##################  FUNCTIONS THAT HANDLE EVENTS TAKING PLACE IN THE WINDOW ########################
#################------------------------------------------------------------#######################
####################################################################################################

  #########################################################
  # function handles change on number of interim analyses #
  #########################################################
  onChangeInterimAnalyses<-function(nValue)
  {
    #First check whether n has changed
    if(n==nValue)
    {
      #nothing changed - so do nothing
    }
    else #interim times changed
    {
      #set new n
      n<<-nValue

      #we'll have to recompute the lists t,t2 new e.g
      #they got default values equally spaced and also set the bounds to some default values
      t<<-1
      for(i in 1:n)
      {
        t[i]<<-i/n
        upperBounds[i]<<-i
      }
      t2<<-t
      lowerBounds<<- -upperBounds


      #update n in menu bar
      tkdelete(topMenu,0,1)
      tkadd(topMenu,"cascade",label=paste("#Interim Times: K=",as.character(n)),menu=nMenu)

      ### equally or unequally spaced times? get it from the checkbox ###
      equallySpacedTimesInput <- as.logical(as.numeric(tclvalue(equallySpacedTimesCheckBoxValue)))

      # check case unequally checkboxes - grid input fields with number of interim analyses into the frames
      if(!equallySpacedTimesInput)
      {
        #first remove "old" labels and input fields - old n is stored in nBackup
        for(i in 1:nBackup)
        {
          #remove labels and input fields
          tkgrid.remove(listOfTimePointLabel.unequalTimes[[i]],listOfEntries.unequalTimes[[i]])
        }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.unequalTimes<<-list()
        listOfTimePointLabel.unequalTimes<<-list()
        listOfEntries.unequalTimes<<-list()

        #create new labels by calling function 'onCheckBoxEquallySpacedTimes()'
        onCheckBoxEquallySpacedTimes()

      }#end <--*if(!equallySpacedTimesInput)*


      ### Second Time scale - will it be used? ###
      secondTimeScaleIsUsedInput <- as.logical(as.numeric(tclvalue(secondTimeScaleIsUsedCheckBoxValue)))

      #check case second times scale checkbox is activated
      if(secondTimeScaleIsUsedInput)
      {
        #first remove "old" labels and input fields - old n is stored in nBackup
        for(i in 1:nBackup)
        {
  #remove labels and input fields
          tkgrid.remove(listOfTimePointLabel.secondTimes[[i]],listOfEntries.secondTimes[[i]])
        }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfTimePointLabel.secondTimes<<-list() #
        listOfInputFields.secondTimes<<-list()
        listOfEntries.secondTimes<<-list()

        #create new labels and input fields by calling function 'onCheckBoxSecondTimeScale()'
        onCheckBoxSecondTimeScale()

      }#end <--*if(secondTimeScaleIsUsedInput)*

      ### if user enters bounds manually we have to adapt appropriate input fields
      if(enterBoundsManually)
      {

        #check on symmetric or asymmetric bounds used
        if(!BoundsSymmetry==3)
        {
          #symmetric
          #first remove "old" labels and input fields - old n is stored in nBackup
          for(i in 1:nBackup)
          {
    #remove labels and input fields
            tkgrid.remove(listOfTimePointLabel.boundsUPPER[[i]],listOfEntries.boundsUPPER[[i]])
          }
          #set the lists to NULL otherwise we would duplicate entries in a next loop
          listOfInputFields.boundsUPPER<<-list()
          listOfTimePointLabel.boundsUPPER<<-list()
          listOfEntries.boundsUPPER<<-list()

          #create new labels and input fields by calling function 'onManualBounds()'
          onManualBounds()

        }#end <--*if(!BoundsSymmetry==3)*

        else #asymmetric bounds - do the same as with symmetric for both frames
        {
          for(i in 1:nBackup)
          {
    #remove labels and input fields
            tkgrid.remove(listOfTimePointLabel.boundsUPPER[[i]],listOfEntries.boundsUPPER[[i]])
            tkgrid.remove(listOfTimePointLabel.boundsLOWER[[i]],listOfEntries.boundsLOWER[[i]])
          }
          #set the lists to NULL otherwise we would duplicate entries in a next loop
          listOfInputFields.boundsUPPER<<-list()
          listOfTimePointLabel.boundsUPPER<<-list()
          listOfEntries.boundsUPPER<<-list()
          listOfInputFields.boundsLOWER<<-list()
          listOfTimePointLabel.boundsLOWER<<-list()
          listOfEntries.boundsLOWER<<-list()

          #create new labels and input fields by calling function 'onManualBounds()'
          onManualBounds()
        }#end <--*else #asymmetric bounds - do the same as with symmetric for both frames*

      }#end <--*if(enterBoundsManually)*


    }#end <--*else #interim times changed*

    #update nBackup
    nBackup<<-n
  }#end <--*onChangeInterimAnalyses<-function(nValue)*


  ###################################################################################
  # function handles a click on checkbox for equally/unequally spaced interim times #
  ###################################################################################
  onCheckBoxEquallySpacedTimes <- function()
  {
    #equally or unequally spaced times? get it from the checkbox
    equallySpacedTimesInput <- as.logical(as.numeric(tclvalue(equallySpacedTimesCheckBoxValue)))

    # case unequally checkboxes - grid input fields with number of interim analyses into the frames
    if(!equallySpacedTimesInput)
    {
      for(i in 1:n)
      {
        #create label in a list thus we can dynamically change number of input fields
        listOfTimePointLabel.unequalTimes<<-c(listOfTimePointLabel.unequalTimes,list(tklabel(unEquallyDynamicFrame, text=paste("time",as.character(i)))))

        #We need a list of Input Fields to be able to save the dynamic created tclVar's
        listOfInputFields.unequalTimes<<-c(listOfInputFields.unequalTimes,list(tclVar(as.character(t[i]))))
        listOfEntries.unequalTimes<<-c(listOfEntries.unequalTimes, list(tkentry(unEquallyDynamicFrame,width="11",textvariable=as.character(listOfInputFields.unequalTimes[[i]]))))

        #put label with Input field
        tkgrid(listOfTimePointLabel.unequalTimes[[i]],listOfEntries.unequalTimes[[i]])
        tkgrid.configure(listOfTimePointLabel.unequalTimes[[i]],sticky="nw")
        tkgrid.configure(listOfEntries.unequalTimes[[i]],sticky="nw")
      }#end <--*for*
    #put frame
    tkgrid(unEquallyDynamicFrame)
    }#end <--*if*

    else #equally spaced - remove all input fields cause they should disappear in the window
    {
      #fade out frame
      tkgrid.forget(unEquallyDynamicFrame)

      for(i in 1:n)
      {
        #remove labels and input fields
tkgrid.remove(listOfTimePointLabel.unequalTimes[[i]],listOfEntries.unequalTimes[[i]])
      }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.unequalTimes<<-list()
        listOfTimePointLabel.unequalTimes<<-list()
        listOfEntries.unequalTimes<<-list()
    }
  }#end <--*function()*

  ##############################################################
  # function handles a click on checkbox for second time scale
  #
  # ATTENTION: this feature of a second time scale is currently NOT used!
  #
  ##############################################################
  onCheckBoxSecondTimeScale <- function()
  {
    #second time scale used?
    secondTimeScaleIsUsedInput <- as.logical(as.numeric(tclvalue(secondTimeScaleIsUsedCheckBoxValue)))

    # case unequally checkboxes - grid input fields with number of interim analyses into the frames
    if(secondTimeScaleIsUsedInput)
    {
      for(i in 1:n)
      {
        #create label in a list thus we can dynamically change number of input fields
        listOfTimePointLabel.secondTimes<<-c(listOfTimePointLabel.secondTimes,list(tklabel(secondTimesDynamicFrame, text=paste("time",as.character(i)))))

        #We need a list of Input Fields to be able to save the dynamic created tclVar's
        listOfInputFields.secondTimes<<-c(listOfInputFields.secondTimes,list(tclVar(as.character(t2[i]))))
        listOfEntries.secondTimes<<-c(listOfEntries.secondTimes, list(tkentry(secondTimesDynamicFrame,width="11",textvariable=as.character(listOfInputFields.secondTimes[[i]]))))

        #put label with Input field
        tkgrid(listOfTimePointLabel.secondTimes[[i]],listOfEntries.secondTimes[[i]])
        tkgrid.configure(listOfTimePointLabel.secondTimes[[i]],sticky="nw")
        tkgrid.configure(listOfEntries.secondTimes[[i]],sticky="nw")
      }#end <--*for*
    #put frame
    tkgrid(secondTimesDynamicFrame)
    }#end <--*if*

    else #equally spaced - remove all input fields cause they should disappear in the window
    {
      #fade out frame
      tkgrid.forget(secondTimesDynamicFrame)

      for(i in 1:n)
      {
#remove labels and input fields
tkgrid.remove(listOfTimePointLabel.secondTimes[[i]],listOfEntries.secondTimes[[i]])
      }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.secondTimes<<-list()
        listOfTimePointLabel.secondTimes<<-list()
        listOfEntries.secondTimes<<-list()
    }

  }#end <--*onCheckBoxSecondTimeScale <- function()*




  #################################################################
  # function handles a click on checkbox to enter bounds manually #
  #################################################################
  onManualBounds <-function()
  {
    #get checkbox value
    enterBoundsManually<<-as.numeric(tclvalue(manualBoundsCheckBoxValue))


    ### user wants to enter bounds manually ###
    if(enterBoundsManually)
    {
      #fade out frame with choice of functions and truncating bounds checkbox
      tkgrid.forget(computedBoundsFrame)
      tkgrid.forget(TruncateBoundsFrame)

      #symmetric or asymmetric bounds? get checkbox value and check it out
      BoundsSymmetry <<- as.numeric(tclvalue(SymmetryValue))


      #at least we need one input field
      for(i in 1:n)
      {
        #create label in a list thus we can dynamically change number of input fields
        listOfTimePointLabel.boundsUPPER<<-c(listOfTimePointLabel.boundsUPPER,list(tklabel(manualBoundsUPPERframe.InputFields, text=paste("time",as.character(i)))))

        #We need a list of Input Fields to be able to save the dynamic created tclVar's
        listOfInputFields.boundsUPPER<<-c(listOfInputFields.boundsUPPER,list(tclVar(as.character(upperBounds[i]))))
        listOfEntries.boundsUPPER<<-c(listOfEntries.boundsUPPER, list(tkentry(manualBoundsUPPERframe.InputFields,width="11",textvariable=as.character(listOfInputFields.boundsUPPER[[i]]))))

        #put label with Input field
        tkgrid(listOfTimePointLabel.boundsUPPER[[i]],listOfEntries.boundsUPPER[[i]])
        tkgrid.configure(listOfTimePointLabel.boundsUPPER[[i]],sticky="nw")
        tkgrid.configure(listOfEntries.boundsUPPER[[i]],sticky="nw")
      }#end <--*for*

      #if asymmetric bounds we need an additional second input field
      if(BoundsSymmetry==3)
      {
        for(i in 1:n)
        {
          #create label in a list thus we can dynamically change number of input fields
          listOfTimePointLabel.boundsLOWER<<-c(listOfTimePointLabel.boundsLOWER,list(tklabel(manualBoundsLOWERframe.InputFields, text=paste("time",as.character(i)))))

          #We need a list of Input Fields to be able to save the dynamic created tclVar's
          listOfInputFields.boundsLOWER<<-c(listOfInputFields.boundsLOWER,list(tclVar(as.character(lowerBounds[i]))))
          listOfEntries.boundsLOWER<<-c(listOfEntries.boundsLOWER, list(tkentry(manualBoundsLOWERframe.InputFields,width="11",textvariable=as.character(listOfInputFields.boundsLOWER[[i]]))))

          #put label with Input field
          tkgrid(listOfTimePointLabel.boundsLOWER[[i]],listOfEntries.boundsLOWER[[i]])
          tkgrid.configure(listOfTimePointLabel.boundsLOWER[[i]],sticky="nw")
          tkgrid.configure(listOfEntries.boundsLOWER[[i]],sticky="nw")
        }#end <--*for*
        tkgrid(manualBoundsUPPERframe,manualBoundsLOWERframe,sticky="nw")
      }
      else
      {
        tkgrid(manualBoundsUPPERframe,sticky="nw")
      }

      #put whole frame
      tkgrid(manualBoundsFrame,sticky="w")

    }#end <--*if(enterBoundsManually)*

    else #user deactivated checkbox to enter bounds manual
    {
      #fade out frame containing input fields for manual bounds and fade in truncating bounds checkbox
      tkgrid.forget(manualBoundsUPPERframe)
      tkgrid.forget(manualBoundsLOWERframe)
      tkgrid.forget(manualBoundsFrame)
      tkgrid(TruncateBoundsFrame)

      #check on symmetric or asymmetric bounds used
      if(!BoundsSymmetry==3)
      {
        #symmetric
        #remove labels and input fields and clear lists
        for(i in 1:n)
        {
          #remove labels and input fields
          tkgrid.remove(listOfTimePointLabel.boundsUPPER[[i]],listOfEntries.boundsUPPER[[i]])
        }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.boundsUPPER<<-list()
        listOfTimePointLabel.boundsUPPER<<-list()
        listOfEntries.boundsUPPER<<-list()

      }#end <--*if(!BoundsSymmetry==3)*

      else #asymmetric bounds - do the same as with symmetric for both frames
      {
        for(i in 1:n)
        {
          #remove labels and input fields and clear lists
          tkgrid.remove(listOfTimePointLabel.boundsUPPER[[i]],listOfEntries.boundsUPPER[[i]])
          tkgrid.remove(listOfTimePointLabel.boundsLOWER[[i]],listOfEntries.boundsLOWER[[i]])
        }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.boundsUPPER<<-list()
        listOfTimePointLabel.boundsUPPER<<-list()
        listOfEntries.boundsUPPER<<-list()
        listOfInputFields.boundsLOWER<<-list()
        listOfTimePointLabel.boundsLOWER<<-list()
        listOfEntries.boundsLOWER<<-list()

      }#end <--*else #asymmetric bounds - do the same as with symmetric for both frames*

      #call onBoundsChosen which will fade in the needed frames
      onBoundsChosen()
      tkgrid(computedBoundsFrame)

    }#end <--*else #user deactivated checkbox to enter bounds manual*

  }#end <--*onManualBounds <-function()*


  ####################################################################
  # functions handle a click on CONFIRM-Buttons to choose a function #
  ####################################################################
  onConfirmFunction1 <-function()
  {
    #check whether we got symmetric or asymmetric bounds
    BoundsSymmetry <<- as.numeric(tclvalue(SymmetryValue))

    #get value from listbox and ask for additional parameters if necessary
    if( BoundsSymmetry==1 || BoundsSymmetry==2)
    {
      function1<<-as.numeric(tkcurselection(listBoxFunction1of1))+1
    }
    else
    {
      function1<<-as.numeric(tkcurselection(listBoxFunction1of2))+1
    }

    #check whether user selected a function
    if( length(function1)==0 )
    {
      tkmessageBox(message="You must select a function!",icon="error",type="ok")
    }

    else # handle select
    {
      ### ASYMMETRIC ###
      if(BoundsSymmetry==3)
      {
        #first of all remove earlier input field
        tkgrid.remove(phiLabel1of2,entry.functionParameter1of2)

        #case Power Family
        if (function1==3)
        {
          #set new label and input field
          phiLabel1of2<<-tklabel(additionalParametersFrame1of2,text="Enter Paramter phi>0:")
          entry.functionParameter1of2 <<-tkentry(additionalParametersFrame1of2,width="6",textvariable=phi1of2)
          tkgrid(phiLabel1of2,sticky="w")
          tkgrid(entry.functionParameter1of2,sticky="w")
        }

        #case Hwang-Shih-DeCani family
        else if (function1==4)
             {
               #set new label and input field
               phiLabel1of2<<-tklabel(additionalParametersFrame1of2,text="Enter Parameter phi=/=0:")
               entry.functionParameter1of2 <<-tkentry(additionalParametersFrame1of2,width="6",textvariable=phi1of2)
               tkgrid(phiLabel1of2,sticky="w")
               tkgrid(entry.functionParameter1of2,sticky="w")
             }
             #case no additional parameters needed
             else
             {
               #do nothing else
             }
      }#end <--*if(BoundsSymmetry==3)*

      else ### SYMMETRIC - do the same in other frame###
      {
        #first of all remove earlier input field
        tkgrid.remove(phiLabel1of1,entry.functionParameter1of1)

        #get value from listbox and ask for additional parameters if necessary
        functionInput <<- as.numeric(tkcurselection(listBoxFunction1of1))+1

        #case Power Family
        if (function1==3)
        {
          #set new label and input field
          phiLabel1of1<<-tklabel(additionalParametersFrame1of1,text="Enter Paramter phi>0:")
          entry.functionParameter1of1 <<-tkentry(additionalParametersFrame1of1,width="6",textvariable=phi1of1)
          tkgrid(phiLabel1of1,sticky="w")
          tkgrid(entry.functionParameter1of1,sticky="w")
        }

        #case Hwang-Shih-DeCani family
        else if (function1==4)
             {
               #set new label and input field
               phiLabel1of1<<-tklabel(additionalParametersFrame1of1,text="Enter Parameter phi=/=0:")
               entry.functionParameter1of1 <<-tkentry(additionalParametersFrame1of1,width="6",textvariable=phi1of1)
               tkgrid(phiLabel1of1,sticky="w")
               tkgrid(entry.functionParameter1of1,sticky="w")
             }
             #case no additional parameters needed
             else
             {
               #do nothing else
             }
      }#end <--*else ### SYMMETRIC - do the same in other frame###     *
    }#end <--*else # handle select*
  }#end <--*onConfirmFunction1 <-function()*




  onConfirmFunction2 <-function()
  {
    #get value from listbox and ask for additional parameters if necessary
    function2<<-as.numeric(tkcurselection(listBoxFunction2of2))+1

    #check whether user selected a function
    if( length(function2)==0 )
    {
      tkmessageBox(message="You must have select a function!",icon="error",type="ok")
    }

    else # handle select
    {
      #first of all remove earlier input field
      tkgrid.remove(phiLabel2of2,entry.functionParameter2of2)

      #case Power Family
      if (function2==3)
      {
        #set new label and input field
        phiLabel2of2<<-tklabel(additionalParametersFrame2of2,text="Enter Paramter phi>0:")
        entry.functionParameter2of2 <<-tkentry(additionalParametersFrame2of2,width="6",textvariable=phi2of2)
        tkgrid(phiLabel2of2,sticky="w")
        tkgrid(entry.functionParameter2of2,sticky="w")
      }

      #case Hwang-Shih-DeCani family
      else if (function2==4)
           {
             #set new label and input field
             phiLabel2of2<<-tklabel(additionalParametersFrame2of2,text="Enter Parameter phi=/=0:")
             entry.functionParameter2of2 <<-tkentry(additionalParametersFrame2of2,width="6",textvariable=phi2of2)
             tkgrid(phiLabel2of2,sticky="w")
             tkgrid(entry.functionParameter2of2,sticky="w")
           }
           #case no additional parameters needed
           else
           {
             #do nothing else
           }
    }#end <--*else # handle select*
  }#end <--*onConfirmFunction2 <-function()*



  ##################################################################
  # function handles a click on Radio Button for ASYMMETRIC BOUNDS #
  ##################################################################
  onBoundsChosen <- function()
  {
    #check whether we got asymmetric bounds
    BoundsSymmetry <<- as.numeric(tclvalue(SymmetryValue))

    #check whether bounds are computed or entered manually by user
    #get checkbox value
    enterBoundsManually<-as.numeric(tclvalue(manualBoundsCheckBoxValue))



    #Asymmetric Bounds!
    if( (BoundsSymmetry==1 || BoundsSymmetry==2) & boundBackup!=3)
    {
      #nothing to change
    }
    else if( (BoundsSymmetry==1 || BoundsSymmetry==2) & boundBackup==3)
         {
           #if users enters bounds manually update frames, if necessary
           if(enterBoundsManually)
           {
             tkgrid.forget(manualBoundsUPPERframe)
             tkgrid.forget(manualBoundsLOWERframe)
             tkgrid.forget(manualBoundsFrame)
             onManualBounds()
           }
           #exchange frames
           tkgrid.remove(nonSymmetricBoundsFrame)
           tkgrid(symmetricBoundsFrame,sticky="nw")
         }
         else if(BoundsSymmetry==3 & boundBackup!=3)
              {
                #if users enters bounds manually update frames, if necessary
                if(enterBoundsManually)
                {
                  tkgrid.forget(manualBoundsUPPERframe)
                  tkgrid.forget(manualBoundsLOWERframe)
                  tkgrid.forget(manualBoundsFrame)
                  onManualBounds()
                }
                #exchange frames
                tkgrid.remove(symmetricBoundsFrame)
                tkgrid(nonSymmetricBoundsFrame,sticky="nw")

              }
  #update boundBackup
  boundBackup<<-BoundsSymmetry
  }


  #######################################################
  # function handles a click on Trunate Bounds Checkbox #
  #######################################################
  onTruncateCheckbox <- function()
  {
    #checkbox activated?
    truncateBoundsYesNo <<- as.logical(as.numeric(tclvalue(TruncateBoundsCheckBoxValue)))

    if(truncateBoundsYesNo) #activated
    {
      ##grid edit box
      tkgrid(TruncateDynamicFrame)
    }
    else #deactivated
    {
      #ungrid edit box
      tkgrid.forget(TruncateDynamicFrame)
    }
  }


  ##################################################
  # function handles a click on 'CALCULATE'-Button #
  ##################################################
  OnCalculateInputTask1 <- function()
  {
    readyForCalculate <- TRUE

    #get values from checkboxes, listboxes and radiobuttons
    equallySpacedTimesInput <<- as.logical(as.numeric(tclvalue(equallySpacedTimesCheckBoxValue)))
    secondTimeScaleIsUsedInput <<- as.logical(as.numeric(tclvalue(secondTimeScaleIsUsedCheckBoxValue)))
    BoundsSymmetry <<- as.numeric(tclvalue(SymmetryValue))
    truncateBoundsYesNo <<- as.logical(as.numeric(tclvalue(TruncateBoundsCheckBoxValue)))
    drift <<- as.numeric(tclvalue(driftTclVar))

    #truncation point set?
    if(truncateBoundsYesNo)
    {
      TruncateBoundsInput <<- abs(as.numeric(tclvalue(boundsTruncation)))
    }
    else
    {
      TruncateBoundsInput<-8
    }

    #evaluate whether function is used to compute bounds or user entered them manually
    if(enterBoundsManually)
    {
      #manually entered bounds
      for(i in 1:n)
      {
        upperBounds[i]<<- as.numeric(tclvalue(listOfInputFields.boundsUPPER[[i]]))
      }

      #elicit lower bounds
      if(BoundsSymmetry==1)
      {
        #one-sided => lower Bounds == -8 (that is -infinity)
        lowerBounds <<- seq(-8,-8,length=n)
      }
      else if(BoundsSymmetry==2)
           {
             #two-sided symmetric
             lowerBounds <<- -upperBounds
           }
           else
           {
             #asymmetric
             for(i in 1:n)
             {
               lowerBounds[i]<<- as.numeric(tclvalue(listOfInputFields.boundsLOWER[[i]]))
             }
           }
    }#end <--*if(enterBoundsManually)*

    else #bounds are computed
    {
      #get chosen function(s) and check alpha
      ###################
      # case asymmetric #
      ###################
      if(BoundsSymmetry==3)
      {
        alpha1 <<- as.numeric(tclvalue(alpha1of2))
        alpha2 <<- as.numeric(tclvalue(alpha2of2))

        #check alpha
        alphaAll<- alpha1 + alpha2
        if( !(alpha1>=0 & alpha1<=1 & alpha2>=0 & alpha2<=1 & alphaAll<=1) )
        {
          readyForCalculate<-FALSE
          tkmessageBox(message="Alpha out of range! Correct it and try again.",icon="error",type="ok")
        }

        #check phi if entered as parameter
        phi1 <<- as.numeric(tclvalue(phi1of2))
        phi2 <<- as.numeric(tclvalue(phi2of2))
        #function for UPPER bounds
        if(function1==3)
        {
          if( !(phi1>0) )
          {
            readyForCalculate<-FALSE
            tkmessageBox(message="Parameter phi in function for UPPER bounds must be >0 !",icon="error",type="ok")
          }
        }
        else if(function1==4)
             {
               if(phi1==0)
               {
                 readyForCalculate<-FALSE
                 tkmessageBox(message="Parameter phi in function for UPPER bounds may NOT be zero!",icon="error",type="ok")
               }
             }

        #same with function for LOWER bounds
        if(function2==3)
        {
          if( !(phi2>0) )
          {
            readyForCalculate<-FALSE
            tkmessageBox(message="Parameter phi in function for LOWER bounds must be >0 !",icon="error",type="ok")
          }
        }
        else if(function2==4)
             {
               if(phi2==0)
               {
                 readyForCalculate<-FALSE
                 tkmessageBox(message="Parameter phi in function for LOWER bounds may NOT be zero!",icon="error",type="ok")
               }
             }
      }#end <--*if(BoundsSymmetry==3)*

      ##################
      # case symmetric #
      ##################
      else #one function cause of symmetric bounds
      {
        alpha1 <<- as.numeric(tclvalue(alpha1of1))
        #check alpha
        if( !(alpha1>=0 & alpha1<=1) )
        {
          readyForCalculate<-FALSE
          tkmessageBox(message="Alpha out of range! Correct it and try again.",icon="error",type="ok")
        }

        #check phi if entered as parameter
        phi1 <<- as.numeric(tclvalue(phi1of1))
        #what function used?
        if(function1==3)
        {
          if( !(phi1>0) )
          {
            readyForCalculate<-FALSE
            tkmessageBox(message="Parameter phi in function must be >0 !",icon="error",type="ok")
          }
        }
        else if(function1==4)
             {
               if(phi1==0)
               {
                 readyForCalculate<-FALSE
                 tkmessageBox(message="Parameter phi in function may NOT be zero!",icon="error",type="ok")
                 }
             }
      }

    }#end <--*else #bounds are computed*
    ##if user typed in unequally spaced times - get them and
    ##check them to be in intervall(0,1] and in right order
    if(!equallySpacedTimesInput)
    {
      tempVal<-0
      interimTimesBad<-FALSE
      for(i in 1:n)
      {
        tempVal[i] <- as.numeric(tclvalue(listOfInputFields.unequalTimes[[i]]))

        if (tempVal[i]<=0) { interimTimesBad<-TRUE }
        if (tempVal[i]>1) { interimTimesBad<-TRUE }
        if (i>1)
        {
          if (tempVal[i]<=tempVal[i-1]) { interimTimesBad<-TRUE }
        }
      }#end <--*for(i in 1:n)*

      ##if times are not good => error and keep old times
        if(interimTimesBad)
        {
          readyForCalculate <- FALSE
          tkmessageBox(message="Bad Interim Times entered - old Times are kept so far! ",icon="error",type="ok")
        }
        ##else take new times
        else
        {
          t<<-tempVal
        }

    }#end <--*if(!equallySpacedTimesInput)*
    else
    {
     for(i in 1:n)
     {
       t[i]<<-i/n
     }
    }


    ##if user typed in second time scales - get them and
    ##check them to be in intervall(0,1] and in right order
    if(secondTimeScaleIsUsedInput)
    {
      tempVal<-0
      secondTimesBad<-FALSE
      for(i in 1:n)
      {
        tempVal[i] <- as.numeric(tclvalue(listOfInputFields.secondTimes[[i]]))

        if (tempVal[i]<=0) { secondTimesBad<-TRUE }
        if (tempVal[i]>1) { secondTimesBad<-TRUE }
        if (i>1)
        {
          if (tempVal[i]<=tempVal[i-1]) { secondTimesBad<-TRUE }
        }
      }#end <--*for(i in 1:n)*

      ##if times are not good => error and keep old times
        if(secondTimesBad)
        {
          readyForCalculate <- FALSE
          tkmessageBox(message="Bad Second Time Scale entered - old Times are kept so far! ",icon="error",type="ok")
        }
        ##else take new times
        else
        {
          t2<<-tempVal
        }

      if(readyForCalculate & drift!=0)
      {

        ###########################################################################
        ############### RESCALE SECOND TIME SCALE IF NECESSARY ####################
        ###########################################################################
        # When second time scale is not on (0,1], computation with
        # non-zero drift parameters are incorrect, since the drift
        # always scaled to (0,1].  If the trial is complete (t[n]=1)
        # then t2 can be rescaled as t3 = t2/t2[n].  Otherwise, if
        # the second time scale is to be used for covariances, the
        # user must enter a maximum value.
        #
        # drift[ t[i] ] = drift*t[i]
        #
        # Start with t3 = t2.
        # (t2=t by default e.g. if user did not enter second time scale.)
        t3<<-t2
        t2max<<-0

        ##If t[n]=1, t2[n] is maximum of t2.
        if(t[n]==1)
        {
         tkmessageBox(title="-3- Compute Probabilities given Bounds and Drift",message="Second Time scale will be used to determine covariances.",icon="info",type="ok")
         t2max<<-t2[n]
         t3<<-t2/t2max
        }
        else ##Should we try to use 2nd scale?
        {
          response<-tkmessageBox(message="Do you wish to use the 2nd time scale to determine covariances?",
                                 icon="question",type="yesno",default="yes")


          ##--If yes, prompt for maximum of 2nd time scale.--##
          if( as.character(tclvalue(response))=="yes" )
          {
            ##################################################
            # function handles prompting for t2max if needed #
            ##################################################
            t2maxPrompt <- function(title,question,entryInit,entryWidth=4,returnValOnCancel="ID_CANCEL")
            {
          dlg <- tktoplevel(taskWindow)
              tkwm.deiconify(dlg)
              tkgrab.set(dlg)
              tkfocus(dlg)
              tkwm.title(dlg,title)
              textEntryVarTcl <- tclVar(paste(entryInit))
              textEntryWidget <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl)
              tkgrid(tklabel(dlg,text="       "))
              tkgrid(tklabel(dlg,text=question),textEntryWidget)
              tkgrid(tklabel(dlg,text="       "))
              ReturnVal <- returnValOnCancel
              onOK <- function()
              {
                ReturnVal <<- as.numeric(tclvalue(textEntryVarTcl))
                #check whether numeric was entered
                if(is.na(ReturnVal))
                {
                  tkmessageBox(title="ERROR",message="You did not enter a valid numeric value!",icon="error",type="ok")
                }
                #if input numeric check whether the numeric is a valid entry
                else
                {
                  if(ReturnVal<=0)
                  {
                    tkmessageBox(title="ERROR",message="Maximum must be positive! Please try again!",icon="error",type="ok")
                  }
                  else if(ReturnVal<t2[n])
                       {
                         tkmessageBox(title="ERROR",message="The Maximum cannot be smaller than your last seond time scale value!",icon="error",type="ok")
                       }

                       #input ok - go back to main window
                       else
                       {
                         tkgrab.release(dlg)
                         tkdestroy(dlg)
                         tkfocus(task3)
                       }
                }

              }
              onCancel <- function()
              {
                readyForCalculate<<-FALSE
                ReturnVal <<- returnValOnCancel
                tkgrab.release(dlg)
                tkdestroy(dlg)
                tkfocus(task3)
              }
              OK.but     <-tkbutton(dlg,text="   OK   ",command=onOK)
              Cancel.but <-tkbutton(dlg,text=" Cancel ",command=onCancel)
              tkgrid(OK.but,Cancel.but)
              tkgrid(tklabel(dlg,text="    "))

              tkfocus(dlg)
              tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkfocus(task3)})
              tkbind(textEntryWidget, "<Return>", onOK)
              tkwait.window(dlg)

              return(ReturnVal)

            }#end <--*t2maxPrompt <- function(...)*


            ReturnVal<-t2maxPrompt("Second Time Scale will be used","Enter the maximum value of the second time scale","" )
            if(ReturnVal!="ID_CANCEL")
            {
              t2max<<-ReturnVal
              #Rescale t2
              t3<<-t2/t2max
            }

          }#end <--*if( as.character(tclvalue(response))=="yes" )*

          else if( as.character(tclvalue(response))=="no" )
          {
            ##Even if the 2nd time scale was on (0,1], if the
            ##maximum information value was not entered, set
            ##t3 to t, which causes t2 to be ignored!#
            t3<<-t

          }

        }#end <--*else ##Should we try to use 2nd scale?*

      }#end <--*if(readyForCalculate)*

    }#end <--*if(secondTimeScaleIsUsedInput)*
    else
    {
      ##set t3 to t, which causes t2 to be ignored!
      t3<<-t
    }

    if(readyForCalculate)
    {
      # second time scale is not used so far --> set t3=t2=t
      t2<-t; t3<-t
      calculateTask3(n,nMax,t,t2,t2max,t3,drift,equallySpacedTimesInput,secondTimeScaleIsUsedInput,
                     BoundsSymmetry, c(alpha1,alpha2), c(phi1,phi2), c(function1,function2),
                     TruncateBoundsInput,enterBoundsManually, upperBounds, lowerBounds, taskWindow )
    }
  }#end <--*OnCalculateInputTask1 <- function()*



#######################################################################################################
#################------------------------------------------------------------------####################
##################  FROM HERE ON LABELS AND FRAMES ARE CREATED AND PUT TOGETHER   #####################
#################------------------------------------------------------------------####################
#######################################################################################################

  #Set Toplevel
  task3 <- tktoplevel(taskWindow)
  tkwm.title(task3,"-3- Compute Probabilities given Bounds and Drift")

  #Define main Frame
  InputTask3 <- tkframe(task3, relief="groove",borderwidth=2)


  ##--------------------------------------------------------------------------------##
  ##------------------------  number of Interim Times  -----------------------------##
  ##--------------------------------------------------------------------------------##

  #create pull down menu to select interim analyses from n==1 to n==25(=nMax)
  topMenu <- tkmenu(task3)
  tkconfigure(task3,menu=topMenu)
  nMenu <- tkmenu(topMenu,tearoff=FALSE,background="grey",activebackground="red")
  tkadd(nMenu,"command",label="1",command=function() onChangeInterimAnalyses(1))
  tkadd(nMenu,"command",label="2",command=function() onChangeInterimAnalyses(2))
  tkadd(nMenu,"command",label="3",command=function() onChangeInterimAnalyses(3))
  tkadd(nMenu,"command",label="4",command=function() onChangeInterimAnalyses(4))
  tkadd(nMenu,"command",label="5",command=function() onChangeInterimAnalyses(5))
  tkadd(nMenu,"command",label="6",command=function() onChangeInterimAnalyses(6))
  tkadd(nMenu,"command",label="7",command=function() onChangeInterimAnalyses(7))
  tkadd(nMenu,"command",label="8",command=function() onChangeInterimAnalyses(8))
  tkadd(nMenu,"command",label="9",command=function() onChangeInterimAnalyses(9))
  tkadd(nMenu,"command",label="10",command=function() onChangeInterimAnalyses(10))
  tkadd(nMenu,"command",label="11",command=function() onChangeInterimAnalyses(11))
  tkadd(nMenu,"command",label="12",command=function() onChangeInterimAnalyses(12))
  tkadd(nMenu,"command",label="13",command=function() onChangeInterimAnalyses(13))
  tkadd(nMenu,"command",label="14",command=function() onChangeInterimAnalyses(14))
  tkadd(nMenu,"command",label="15",command=function() onChangeInterimAnalyses(15))
  tkadd(nMenu,"command",label="16",command=function() onChangeInterimAnalyses(16))
  tkadd(nMenu,"command",label="17",command=function() onChangeInterimAnalyses(17))
  tkadd(nMenu,"command",label="18",command=function() onChangeInterimAnalyses(18))
  tkadd(nMenu,"command",label="19",command=function() onChangeInterimAnalyses(19))
  tkadd(nMenu,"command",label="20",command=function() onChangeInterimAnalyses(20))
  tkadd(nMenu,"command",label="21",command=function() onChangeInterimAnalyses(21))
  tkadd(nMenu,"command",label="22",command=function() onChangeInterimAnalyses(22))
  tkadd(nMenu,"command",label="23",command=function() onChangeInterimAnalyses(23))
  tkadd(nMenu,"command",label="24",command=function() onChangeInterimAnalyses(24))
  tkadd(nMenu,"command",label="25",command=function() onChangeInterimAnalyses(25))
  tkadd(topMenu,"cascade",label=paste("#Interim Times: K= ",as.character(n)),menu=nMenu)

  tkgrid(tklabel(InputTask3,text="")) # Blank line


  ##--------------------------------------------------------------------------------##
  ##-------------  Interim Times equally or unequally spaced? ----------------------##
  ##-------------       Second Time Scale will be used?       ----------------------##
  ##--------------------------------------------------------------------------------##

  ## prepare Frames
  interimTimesFrame<- tkframe(InputTask3,relief="groove",borderwidth=0)
  equallySpacedTimesFrame <- tkframe(interimTimesFrame,relief="groove",borderwidth=0)
  secondTimesFrame <- tkframe(interimTimesFrame,relief="groove",borderwidth=0)

  #again we need a frame for dynamic working in it to not affect
  #the format of 'equallySpacedTimesFrame' respective 'secondTimesFrame'
  equallySpacedLabelFrame<-tkframe(equallySpacedTimesFrame,relief="groove",borderwidth=0)
  unEquallyDynamicFrame<-tkframe(equallySpacedTimesFrame,relief="groove",borderwidth=0)
  secondTimesLabelFrame<-tkframe(secondTimesFrame,relief="groove",borderwidth=0)
  secondTimesDynamicFrame<-tkframe(secondTimesFrame,relief="groove",borderwidth=0)

  #Default is Equally Spaced Times and no Second Time Scale
  #create Checkboxes
  #equally spaced
  equallySpacedTimesCheckBox<-tkcheckbutton(equallySpacedLabelFrame,command=onCheckBoxEquallySpacedTimes)
  equallySpacedTimesCheckBoxValue <- tclVar(as.character(as.numeric(equallySpacedTimesInput)))
  tkconfigure(equallySpacedTimesCheckBox,variable=equallySpacedTimesCheckBoxValue)
  #second time scale
  secondTimeScaleIsUsedCheckBox<-tkcheckbutton(secondTimesLabelFrame,command=onCheckBoxSecondTimeScale)
  secondTimeScaleIsUsedCheckBoxValue <- tclVar(as.character(as.numeric(secondTimeScaleIsUsedInput)))
  tkconfigure(secondTimeScaleIsUsedCheckBox,variable=secondTimeScaleIsUsedCheckBoxValue)

  #put checkbox and other frames together
  equallyTimesBoxLabel<-tklabel(equallySpacedLabelFrame,text="Equally Spaced Times")
  secondTimesBoxLabel<-tklabel(secondTimesLabelFrame,text="Use Second Time Scale")
  tkgrid(equallyTimesBoxLabel,equallySpacedTimesCheckBox)
  # tkgrid(secondTimesBoxLabel,secondTimeScaleIsUsedCheckBox) - this feature is currently removed
  tkgrid(equallySpacedTimesFrame,secondTimesFrame,sticky="n")
  tkgrid(equallySpacedLabelFrame,sticky="n")
  tkgrid(secondTimesLabelFrame,sticky="n")
  tkgrid(unEquallyDynamicFrame,sticky="nw")
  tkgrid(secondTimesDynamicFrame,sticky="n")
  tkgrid(interimTimesFrame,sticky="w")
  tkgrid(tklabel(InputTask3,text="")) # Blank line

  #Drift Parameter
  driftTclVar<-tclVar(as.character(drift))
  driftFrame1 <- tkframe(InputTask3,relief="groove",borderwidth=0)
  driftFrame2 <- tkframe(InputTask3,relief="groove",borderwidth=0)
  driftLabel1<-tklabel(driftFrame1,text="Enter Drift Parameter:")
  driftLabel2<-tklabel(driftFrame2,text="Drift is equal to the expectation of the Z statistic when time=1.")

  entry.drift <-tkentry(driftFrame1,width="6",textvariable=driftTclVar)
  #grid it
  tkgrid(driftLabel1,entry.drift, sticky="w")
  tkgrid(driftLabel2,sticky="w")
  tkgrid(tklabel(driftFrame2,text="")) # Blank line
  tkgrid(driftFrame1,sticky="w")
  tkgrid(driftFrame2,sticky="w")

  ###One- or Two-Sided Bounds or asymmetric Bounds###
  #create frames
  boundsLabelFrame <- tkframe(InputTask3,relief="groove",borderwidth=0)
  boundsRadioButtonFrame <- tkframe(InputTask3,relief="groove",borderwidth=0)

  #create radio buttons
  oneSided <- tkradiobutton(boundsRadioButtonFrame,command=onBoundsChosen)
  twoSided <- tkradiobutton(boundsRadioButtonFrame,command=onBoundsChosen)
  asymmetric <- tkradiobutton(boundsRadioButtonFrame,command=onBoundsChosen)
  SymmetryValue <- tclVar(as.character(BoundsSymmetry))
  tkconfigure(oneSided,variable=SymmetryValue,value="1")
  tkconfigure(twoSided,variable=SymmetryValue,value="2")
  tkconfigure(asymmetric,variable=SymmetryValue,value="3")

  #grid labels and buttons together
  tkgrid(tklabel(boundsLabelFrame,text="One-, Two-Sided-Symmetric or Asymmetric Bounds?"),sticky="w")
  tkgrid(tklabel(boundsRadioButtonFrame,text="One-Sided "),oneSided)
  tkgrid(tklabel(boundsRadioButtonFrame,text="Two-Sided "),twoSided)
  tkgrid(tklabel(boundsRadioButtonFrame,text="Asymmetric "),asymmetric)

  #put frames
  tkgrid(boundsLabelFrame,sticky="w")
  tkgrid(boundsRadioButtonFrame,sticky="w")
  tkgrid(tklabel(InputTask3,text="")) # Blank line

  ################################################################################
  ### User could enter bounds manually instead of let them be computed (default)##
  ################################################################################
  manualOrComputedBoundsFrame<-tkframe(InputTask3,relief="groove",borderwidth=0)
  LabelManualOrComputedBoundsFrame<-tkframe(manualOrComputedBoundsFrame,relief="groove",borderwidth=0)
  computedBoundsFrame<-tkframe(manualOrComputedBoundsFrame,relief="groove",borderwidth=0)
  manualBoundsFrame<-tkframe(manualOrComputedBoundsFrame,relief="groove",borderwidth=0)
  #upper bounds
  manualBoundsUPPERframe<-tkframe(manualBoundsFrame,relief="groove",borderwidth=0)
  manualBoundsUPPERframe.Label<-tkframe(manualBoundsUPPERframe,relief="groove",borderwidth=0)
  manualBoundsUPPERframe.InputFields<-tkframe(manualBoundsUPPERframe,relief="groove",borderwidth=0)
  #lower bounds
  manualBoundsLOWERframe<-tkframe(manualBoundsFrame,relief="groove",borderwidth=0)
  manualBoundsLOWERframe.Label<-tkframe(manualBoundsLOWERframe,relief="groove",borderwidth=0)
  manualBoundsLOWERframe.InputFields<-tkframe(manualBoundsLOWERframe,relief="groove",borderwidth=0)

  #create checkbox with label
  manualBoundsCheckbox <- tkcheckbutton(LabelManualOrComputedBoundsFrame,command=onManualBounds)
  manualBoundsCheckBoxValue <- tclVar(as.character(as.numeric(enterBoundsManually)))
  tkconfigure(manualBoundsCheckbox,variable=manualBoundsCheckBoxValue)
  manualBoundsCheckboxLabel<-tklabel(LabelManualOrComputedBoundsFrame,text="Enter Bounds Manually")
  tkgrid(manualBoundsCheckboxLabel,manualBoundsCheckbox)
  tkgrid(tklabel(LabelManualOrComputedBoundsFrame,text="")) # Blank line
  tkgrid.configure(manualBoundsCheckboxLabel,sticky="w")
  tkgrid.configure(manualBoundsCheckbox,sticky="w")

  #create labels for manual entering and grid alltogether
  manualBoundsUPPERlabel<-tklabel(manualBoundsUPPERframe.Label,text="Enter UPPER Bounds(standardized)     ")
  tkgrid(manualBoundsUPPERlabel,sticky="w")
  tkgrid(manualBoundsUPPERframe.Label,sticky="w")
  tkgrid(manualBoundsUPPERframe.InputFields,sticky="w")
  manualBoundsLOWERlabel<-tklabel(manualBoundsLOWERframe.Label,text="Enter LOWER Bounds (standardized)")
  tkgrid(manualBoundsLOWERlabel,sticky="w")
  tkgrid(manualBoundsLOWERframe.Label,sticky="w")
  tkgrid(manualBoundsLOWERframe.InputFields,sticky="w")

  ### Significance Level(s) alpha and function(s) to be used to calculate bounds###
  ## if user choses asymmetric bounds two different functions could be used ##
  ##create Frames
  alphaAndFunctionsFrame<-tkframe(computedBoundsFrame,relief="groove",borderwidth=0)
  symmetricBoundsFrame<-tkframe(alphaAndFunctionsFrame,relief="groove",borderwidth=0)
  nonSymmetricBoundsFrame<-tkframe(alphaAndFunctionsFrame,relief="groove",borderwidth=0)

  ##Default alpha1==0.05, alpha2==0
  alpha1of1 <- tclVar(as.character(alphaInput))
  alpha1of2 <- tclVar(as.character("0.025"))
  alpha2of2 <- tclVar(as.character("0.025"))

  #########################################################
  ### case symmetric bounds or one-sided test (default) ###
  #########################################################
  #frames
  alphaFrame1of1 <- tkframe(symmetricBoundsFrame,relief="groove",borderwidth=0)
  functionsFrame1of1 <- tkframe(symmetricBoundsFrame,relief="groove",borderwidth=0)
  additionalParametersFrame1of1<-tkframe(symmetricBoundsFrame,relief="groove",borderwidth=0)

  ##create Labels for alpha
  alphaLabel1of1<-tklabel(alphaFrame1of1,text="Significance Level: alpha=")
  entry.alpha1of1 <-tkentry(alphaFrame1of1,width="6",textvariable=alpha1of1)

  #create Listbox for function choice
  functionLabel1of1<-tklabel(functionsFrame1of1,text="What function should be used?")
  listBoxFunction1of1<-tklistbox(functionsFrame1of1,height=5,width=30,selectmode="single",background="grey")
  functionChoice1of1 <- c("(1) O'Brien-Fleming Type","(2) Pocock Type",
    "(3) Power Family: alpha* t^phi","(4) Hwang-Shih-DeCani Family","(5) Exact Pocock Bounds")
  for (i in (1:5))
  {
    tkinsert(listBoxFunction1of1,"end",functionChoice1of1[i])
  }
  tkselection.set(listBoxFunction1of1, functionInput-1)  # Default function is O'Brien-Fleming Type.  Indexing starts at zero.

  #create and put button to confirm a function because for example in case of 'Power family: alpha* t^phi'
  #user has to enter additional parameter 'phi'
  confirmFun.button1of1 <-tkbutton(functionsFrame1of1,text=" CONFIRM FUNCTION ",command=onConfirmFunction1)

  #create variable for edit box which we will need if additional parameters must be entered
  #edit box is unvisible at beginning since default function O'Brien-Fleming Type does not need any additional parameters
  phi1of1 <- tclVar(as.character(phi1))
  phiLabel1of1<-tklabel(additionalParametersFrame1of1,text="")
  entry.functionParameter1of1 <-tkentry(additionalParametersFrame1of1,width="3",textvariable=phi1of1)

  #grid together
  #alpha
  tkgrid(alphaLabel1of1,entry.alpha1of1)
  tkgrid.configure(alphaLabel1of1,sticky="w")
  tkgrid.configure(entry.alpha1of1,sticky="w")
  tkgrid(tklabel(alphaFrame1of1,text="")) # Blank line
  tkgrid(functionLabel1of1,sticky="w")
  tkgrid(listBoxFunction1of1)

  #put frames and button
  tkgrid(alphaFrame1of1,sticky="w")
  tkgrid(functionsFrame1of1,additionalParametersFrame1of1)
  tkgrid(confirmFun.button1of1)
  tkgrid.configure(functionsFrame1of1,sticky="w")
  tkgrid(tklabel(symmetricBoundsFrame,text="")) # Blank line

  #Finally grid frame for symmetric case as default
  tkgrid(LabelManualOrComputedBoundsFrame,sticky="w")
  tkgrid(symmetricBoundsFrame,sticky="nw")
  tkgrid(alphaAndFunctionsFrame,sticky="w")
  tkgrid(computedBoundsFrame,sticky="w")
  tkgrid(manualOrComputedBoundsFrame,sticky="w")


  ##############################
  ### case Asymmetric bounds ###
  ##############################
  #frames
  alphaFrame1of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  functionsFrame1of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  additionalParametersFrame1of2<-tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  alphaFrame2of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  functionsFrame2of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  additionalParametersFrame2of2<-tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)

  ##create Labels for alpha
  alphaLabel1of2<-tklabel(alphaFrame1of2,text="UPPER Bounds: alpha=")
  entry.alpha1of2 <-tkentry(alphaFrame1of2,width="6",textvariable=alpha1of2)

  alphaLabel2of2<-tklabel(alphaFrame2of2,text="LOWER Bounds: alpha=")
  entry.alpha2of2 <-tkentry(alphaFrame2of2,width="6",textvariable=alpha2of2)

  #create Listboxes for function choice
  #################
  # List Box 1of2 #
  #################
  functionLabel1of2<-tklabel(functionsFrame1of2,text="Choose Function for UPPER Bounds")
  listBoxFunction1of2<-tklistbox(functionsFrame1of2,height=5,width=30,selectmode="single",background="grey")
  functionChoice1of2 <- c("(1) O'Brien-Fleming Type","(2) Pocock Type",
    "(3) Power Family: alpha* t^phi","(4) Hwang-Shih-DeCani Family","(5) Exact Pocock Bounds")
  for (i in (1:5))
  {
    tkinsert(listBoxFunction1of2,"end",functionChoice1of2[i])
  }

  #create and put first Confirm button which "commands" same function as in symmetric case did
  confirmFun.button1of2 <-tkbutton(functionsFrame1of2,text=" CONFIRM FUNCTION ",command=onConfirmFunction1)
  #edit box for parameter phi[1]
  phi1of2 <- tclVar(as.character(phi1))
  phiLabel1of2<-tklabel(additionalParametersFrame1of2,text="")
  entry.functionParameter1of2 <-tkentry(additionalParametersFrame1of2,width="3",textvariable=phi1of2)

  #################
  # List Box 2of2 #
  #################
  functionLabel2of2<-tklabel(functionsFrame2of2,text="Choose Function for LOWER Bounds")
  listBoxFunction2of2<-tklistbox(functionsFrame2of2,height=5,width=30,selectmode="single",background="grey")
  functionChoice2of2 <- c("(1) O'Brien-Fleming Type","(2) Pocock Type",
    "(3) Power Family: alpha* t^phi","(4) Hwang-Shih-DeCani Family","(5) Exact Pocock Bounds")
  for (i in (1:5))
  {
    tkinsert(listBoxFunction2of2,"end",functionChoice2of2[i])
  }

  #create and put first Confirm button
  confirmFun.button2of2 <-tkbutton(functionsFrame2of2,text=" CONFIRM FUNCTION ",command=onConfirmFunction2)

  #edit box for parameter phi[2]
  phi2of2 <- tclVar(as.character(phi2))
  phiLabel2of2<-tklabel(additionalParametersFrame2of2,text="")
  entry.functionParameter2of2 <-tkentry(additionalParametersFrame2of2,width="3",textvariable=phi2of2)

  #grid together

  #1of2
  tkgrid(alphaLabel1of2,entry.alpha1of2)
  tkgrid.configure(alphaLabel1of2,sticky="w")
  tkgrid.configure(entry.alpha1of2,sticky="w")
  tkgrid(functionLabel1of2,sticky="w")
  tkgrid(listBoxFunction1of2)

  #put frames and button
  tkgrid(alphaFrame1of2,sticky="w")
  tkgrid(functionsFrame1of2,additionalParametersFrame1of2)
  tkgrid(confirmFun.button1of2)
  tkgrid.configure(functionsFrame1of2,sticky="w")
  tkgrid(tklabel(nonSymmetricBoundsFrame,text="")) # Blank line


  #2of2
  tkgrid(alphaLabel2of2,entry.alpha2of2)
  tkgrid.configure(alphaLabel2of2,sticky="w")
  tkgrid.configure(entry.alpha2of2,sticky="w")
  tkgrid(functionLabel2of2,sticky="w")
  tkgrid(listBoxFunction2of2)

  #put frames and button
  tkgrid(alphaFrame2of2,sticky="w")
  tkgrid(functionsFrame2of2,additionalParametersFrame2of2)
  tkgrid(confirmFun.button2of2)
  tkgrid.configure(functionsFrame2of2,sticky="w")
  tkgrid(tklabel(nonSymmetricBoundsFrame,text="")) # Blank line

  #################################################################

  ###Truncate Bounds?###
  PositionTruncateBoundsFrame<-tkframe(InputTask3,relief="groove",borderwidth=0)
  TruncateBoundsFrame <- tkframe(PositionTruncateBoundsFrame,relief="groove",borderwidth=0)
  TruncateLabelFrame <- tkframe(TruncateBoundsFrame,relief="groove",borderwidth=0)
  TruncateDynamicFrame <-tkframe(TruncateBoundsFrame,relief="groove",borderwidth=0)

  #create checkbox
  TruncateBoundsCheckBox<-tkcheckbutton(TruncateLabelFrame,command=onTruncateCheckbox)
  TruncateBoundsCheckBoxValue <- tclVar(as.character(as.numeric(truncateBoundsYesNo)))
  tkconfigure(TruncateBoundsCheckBox,variable=TruncateBoundsCheckBoxValue)

  #create variable for edit box which we will need if user wants truncation of bounds -
  boundsTruncation <- tclVar(as.character(TruncateBoundsInput))
  boundsTruncationLabel<-tklabel(TruncateDynamicFrame,text="Enter Truncation Point:")
  entry.truncationValue <-tkentry(TruncateDynamicFrame,width="3",textvariable=boundsTruncation)

  #put frames
  tkgrid(tklabel(TruncateLabelFrame,text="Truncate standardized Bounds?"),TruncateBoundsCheckBox)
  tkgrid(boundsTruncationLabel,entry.truncationValue,sticky="w")
  tkgrid(TruncateLabelFrame,TruncateDynamicFrame,sticky="w")
  tkgrid(TruncateBoundsFrame,sticky="w")
  tkgrid(PositionTruncateBoundsFrame,sticky="w")
  tkgrid.forget(TruncateDynamicFrame) #default is no Truncating
  tkgrid(tklabel(InputTask3,text="")) # Blank line

  ##put Overall Frame
  tkgrid(InputTask3)


  #frame for the buttons
  buttonFrame<-tkframe(task3,relief="groove",borderwidth=0)

  #create and put button for calculating
  calculate.button <-tkbutton(buttonFrame,text=" CALCULATE ",command=OnCalculateInputTask1)

  # function handles click onto button to Cancel i.e. close current window
  onCancel <- function()
  {
   tkdestroy(task3)
  }
  cancel.button <-tkbutton(buttonFrame,text=" Cancel ",command=onCancel)

  # grid buttons
  tkgrid( tklabel(buttonFrame, text=""))   #blank line
  tkgrid(calculate.button, tklabel(buttonFrame, text="            "),
         cancel.button, sticky="we" )
  tkgrid( tklabel(buttonFrame, text=""))   #blank line
  tkgrid(buttonFrame)

  tkfocus(task3)

}


"guiInputTask4" <-
function(taskWindow)
{

  ### Initialize Variables ###
  #default inputs
  nMax<-25 # Number of interim analyses is limited to 25
  n<-1 # number of interim analyses
  confidenceLevel<-0.95 # desired power respective confidence level
  alphaInput<-0.05 # 'alphaInput' is input of the desired overall size.
  t<-1  # vector containing interim analyses
  t2<-t # vector containing second time scale - by default t2==t
  t3<-t2 # t3 is t2 divided by the maximum information, if needed
  t2max<-0 # maximum value of t2
  upperBounds<-0 # vector containing upper bounds
  lowerBounds<-0 # vector containing lower bounds
  BoundsSymmetry<-1 # BoundsSymmetry==1 means one-sided bounds, BoundsSymmetry==2 means two-sided symmetric and BoundsSymmetry==3 means asymmetric bounds.
  functionInput<-1 # indicates type I error spending rate function e.g. the function(s) the user choosed
  Zvalue<-0 # standardized statistic (Z value) at the last analysis
  TruncateBoundsInput<-8 ## here 8 equals infinity

  #some status variables (names are self-explanatory)
  nInputEdited<-FALSE
  equallySpacedTimesCheckBoxClicked<-FALSE
  secondTimeScaleCheckBoxClicked<-FALSE
  equallySpacedTimesInput<-TRUE
  secondTimeScaleIsUsedInput<-FALSE
  enterBoundsManually<-FALSE
  truncateBoundsYesNo<-FALSE # default is no truncating of bounds

  #some lists (names are self-explanatory)
  listOfTimePointLabel.unequalTimes<-list() #
  listOfInputFields.unequalTimes<-list()
  listOfEntries.unequalTimes<-list()

  listOfTimePointLabel.secondTimes<-list()
  listOfInputFields.secondTimes<-list()
  listOfEntries.secondTimes<-list()

  listOfTimePointLabel.boundsUPPER<-list() #
  listOfInputFields.boundsUPPER<-list()
  listOfEntries.boundsUPPER<-list()

  listOfTimePointLabel.boundsLOWER<-list() #
  listOfInputFields.boundsLOWER<-list()
  listOfEntries.boundsLOWER<-list()

  #operating variables
  nBackup<-n # backup n
  boundBackup<-BoundsSymmetry # backup BoundsSymmetry
  alpha1<- alphaInput # set alpha
  alpha2<- 0 # alpha2 is need in case of asymmetric bounds
  function1<-functionInput # set function1
  function2<-functionInput # function2 is need in case of asymmetric bounds
  phi1<-1 # optional Parameter referring to Power Family
  phi2<-1 # phi2 is need in case of asymmetric bounds both using Power family

  #Define some Fonts
  fontItalic <- tkfont.create(family="times",size=10,slant="italic")
  fontBig <- tkfont.create(family="times",size=10,weight="bold")


####################################################################################################
#################------------------------------------------------------------#######################
##################  FUNCTIONS THAT HANDLE EVENTS TAKING PLACE IN THE WINDOW ########################
#################------------------------------------------------------------#######################
####################################################################################################

  #########################################################
  # function handles change on number of interim analyses #
  #########################################################
  onChangeInterimAnalyses<-function(nValue)
  {
    #First check whether n has changed
    if(n==nValue)
    {
      #nothing changed - so do nothing
    }
    else #interim times changed
    {
      #set new n
      n<<-nValue

      #we'll have to recompute the lists t,t2 new e.g
      #they got default values equally spaced and also set the bounds to some default values
      t<<-1
      for(i in 1:n)
      {
        t[i]<<-i/n
        upperBounds[i]<<-i
      }
      t2<<-t
      lowerBounds<<- -upperBounds


      #update n in menu bar
      tkdelete(topMenu,0,1)
      tkadd(topMenu,"cascade",label=paste("#Interim Times: K=",as.character(n)),menu=nMenu)

      ### equally or unequally spaced times? get it from the checkbox ###
      equallySpacedTimesInput <- as.logical(as.numeric(tclvalue(equallySpacedTimesCheckBoxValue)))

      # check case unequally checkboxes - grid input fields with number of interim analyses into the frames
      if(!equallySpacedTimesInput)
      {
        #first remove "old" labels and input fields - old n is stored in nBackup
        for(i in 1:nBackup)
        {
  #remove labels and input fields
          tkgrid.remove(listOfTimePointLabel.unequalTimes[[i]],listOfEntries.unequalTimes[[i]])
        }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.unequalTimes<<-list()
        listOfTimePointLabel.unequalTimes<<-list()
        listOfEntries.unequalTimes<<-list()

        #create new labels by calling function 'onCheckBoxEquallySpacedTimes()'
        onCheckBoxEquallySpacedTimes()

      }#end <--*if(!equallySpacedTimesInput)*


      ### Second Time scale - will it be used? ###
      secondTimeScaleIsUsedInput <- as.logical(as.numeric(tclvalue(secondTimeScaleIsUsedCheckBoxValue)))

      #check case second times scale checkbox is activated
      if(secondTimeScaleIsUsedInput)
      {
        #first remove "old" labels and input fields - old n is stored in nBackup
        for(i in 1:nBackup)
        {
  #remove labels and input fields
          tkgrid.remove(listOfTimePointLabel.secondTimes[[i]],listOfEntries.secondTimes[[i]])
        }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfTimePointLabel.secondTimes<<-list() #
        listOfInputFields.secondTimes<<-list()
        listOfEntries.secondTimes<<-list()

        #create new labels and input fields by calling function 'onCheckBoxSecondTimeScale()'
        onCheckBoxSecondTimeScale()

      }#end <--*if(secondTimeScaleIsUsedInput)*

      ### if user enters bounds manually we have to adapt appropriate input fields
      if(enterBoundsManually)
      {

        #check on symmetric or asymmetric bounds used
        if(!BoundsSymmetry==3)
        {
          #symmetric
          #first remove "old" labels and input fields - old n is stored in nBackup
          for(i in 1:nBackup)
          {
    #remove labels and input fields
            tkgrid.remove(listOfTimePointLabel.boundsUPPER[[i]],listOfEntries.boundsUPPER[[i]])
          }
          #set the lists to NULL otherwise we would duplicate entries in a next loop
          listOfInputFields.boundsUPPER<<-list()
          listOfTimePointLabel.boundsUPPER<<-list()
          listOfEntries.boundsUPPER<<-list()

          #create new labels and input fields by calling function 'onManualBounds()'
          onManualBounds()

        }#end <--*if(!BoundsSymmetry==3)*

        else #asymmetric bounds - do the same as with symmetric for both frames
        {
          for(i in 1:nBackup)
          {
    #remove labels and input fields
            tkgrid.remove(listOfTimePointLabel.boundsUPPER[[i]],listOfEntries.boundsUPPER[[i]])
            tkgrid.remove(listOfTimePointLabel.boundsLOWER[[i]],listOfEntries.boundsLOWER[[i]])
          }
          #set the lists to NULL otherwise we would duplicate entries in a next loop
          listOfInputFields.boundsUPPER<<-list()
          listOfTimePointLabel.boundsUPPER<<-list()
          listOfEntries.boundsUPPER<<-list()
          listOfInputFields.boundsLOWER<<-list()
          listOfTimePointLabel.boundsLOWER<<-list()
          listOfEntries.boundsLOWER<<-list()

          #create new labels and input fields by calling function 'onManualBounds()'
          onManualBounds()
        }#end <--*else #asymmetric bounds - do the same as with symmetric for both frames*

      }#end <--*if(enterBoundsManually)*


    }#end <--*else #interim times changed*

    #update nBackup
    nBackup<<-n
  }#end <--*onChangeInterimAnalyses<-function(nValue)*


  ###################################################################################
  # function handles a click on checkbox for equally/unequally spaced interim times #
  ###################################################################################
  onCheckBoxEquallySpacedTimes <- function()
  {
    #equally or unequally spaced times? get it from the checkbox
    equallySpacedTimesInput <- as.logical(as.numeric(tclvalue(equallySpacedTimesCheckBoxValue)))

    # case unequally checkboxes - grid input fields with number of interim analyses into the frames
    if(!equallySpacedTimesInput)
    {
      for(i in 1:n)
      {
        #create label in a list thus we can dynamically change number of input fields
        listOfTimePointLabel.unequalTimes<<-c(listOfTimePointLabel.unequalTimes,list(tklabel(unEquallyDynamicFrame, text=paste("time",as.character(i)))))

        #We need a list of Input Fields to be able to save the dynamic created tclVar's
        listOfInputFields.unequalTimes<<-c(listOfInputFields.unequalTimes,list(tclVar(as.character(t[i]))))
        listOfEntries.unequalTimes<<-c(listOfEntries.unequalTimes, list(tkentry(unEquallyDynamicFrame,width="11",textvariable=as.character(listOfInputFields.unequalTimes[[i]]))))

        #put label with Input field
        tkgrid(listOfTimePointLabel.unequalTimes[[i]],listOfEntries.unequalTimes[[i]])
        tkgrid.configure(listOfTimePointLabel.unequalTimes[[i]],sticky="nw")
        tkgrid.configure(listOfEntries.unequalTimes[[i]],sticky="nw")
      }#end <--*for*
    #put frame
    tkgrid(unEquallyDynamicFrame)
    }#end <--*if*

    else #equally spaced - remove all input fields cause they should disappear in the window
    {
      #fade out frame
      tkgrid.forget(unEquallyDynamicFrame)

      for(i in 1:n)
      {
        #remove labels and input fields
tkgrid.remove(listOfTimePointLabel.unequalTimes[[i]],listOfEntries.unequalTimes[[i]])
      }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.unequalTimes<<-list()
        listOfTimePointLabel.unequalTimes<<-list()
        listOfEntries.unequalTimes<<-list()
    }
  }#end <--*function()*

  ##############################################################
  # function handles a click on checkbox for second time scale
  #
  # ATTENTION: this feature of a second time scale is currently NOT used!
  #
  ##############################################################
  onCheckBoxSecondTimeScale <- function()
  {
    #second time scale used?
    secondTimeScaleIsUsedInput <- as.logical(as.numeric(tclvalue(secondTimeScaleIsUsedCheckBoxValue)))

    # case unequally checkboxes - grid input fields with number of interim analyses into the frames
    if(secondTimeScaleIsUsedInput)
    {
      for(i in 1:n)
      {
        #create label in a list thus we can dynamically change number of input fields
        listOfTimePointLabel.secondTimes<<-c(listOfTimePointLabel.secondTimes,list(tklabel(secondTimesDynamicFrame, text=paste("time",as.character(i)))))

        #We need a list of Input Fields to be able to save the dynamic created tclVar's
        listOfInputFields.secondTimes<<-c(listOfInputFields.secondTimes,list(tclVar(as.character(t2[i]))))
        listOfEntries.secondTimes<<-c(listOfEntries.secondTimes, list(tkentry(secondTimesDynamicFrame,width="11",textvariable=as.character(listOfInputFields.secondTimes[[i]]))))

        #put label with Input field
        tkgrid(listOfTimePointLabel.secondTimes[[i]],listOfEntries.secondTimes[[i]])
        tkgrid.configure(listOfTimePointLabel.secondTimes[[i]],sticky="nw")
        tkgrid.configure(listOfEntries.secondTimes[[i]],sticky="nw")
      }#end <--*for*
    #put frame
    tkgrid(secondTimesDynamicFrame)
    }#end <--*if*

    else #equally spaced - remove all input fields cause they should disappear in the window
    {
      #fade out frame
      tkgrid.forget(secondTimesDynamicFrame)

      for(i in 1:n)
      {
#remove labels and input fields
tkgrid.remove(listOfTimePointLabel.secondTimes[[i]],listOfEntries.secondTimes[[i]])
      }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.secondTimes<<-list()
        listOfTimePointLabel.secondTimes<<-list()
        listOfEntries.secondTimes<<-list()
    }

  }#end <--*onCheckBoxSecondTimeScale <- function()*




  #################################################################
  # function handles a click on checkbox to enter bounds manually #
  #################################################################
  onManualBounds <-function()
  {
    #get checkbox value
    enterBoundsManually<<-as.numeric(tclvalue(manualBoundsCheckBoxValue))


    ### user wants to enter bounds manually ###
    if(enterBoundsManually)
    {
      #fade out frame with choice of functions and truncating bounds checkbox
      tkgrid.forget(computedBoundsFrame)
      tkgrid.forget(TruncateBoundsFrame)

      #symmetric or asymmetric bounds? get checkbox value and check it out
      BoundsSymmetry <<- as.numeric(tclvalue(SymmetryValue))


      #at least we need one input field
      for(i in 1:n)
      {
        #create label in a list thus we can dynamically change number of input fields
        listOfTimePointLabel.boundsUPPER<<-c(listOfTimePointLabel.boundsUPPER,list(tklabel(manualBoundsUPPERframe.InputFields, text=paste("time",as.character(i)))))

        #We need a list of Input Fields to be able to save the dynamic created tclVar's
        listOfInputFields.boundsUPPER<<-c(listOfInputFields.boundsUPPER,list(tclVar(as.character(upperBounds[i]))))
        listOfEntries.boundsUPPER<<-c(listOfEntries.boundsUPPER, list(tkentry(manualBoundsUPPERframe.InputFields,width="11",textvariable=as.character(listOfInputFields.boundsUPPER[[i]]))))

        #put label with Input field
        tkgrid(listOfTimePointLabel.boundsUPPER[[i]],listOfEntries.boundsUPPER[[i]])
        tkgrid.configure(listOfTimePointLabel.boundsUPPER[[i]],sticky="nw")
        tkgrid.configure(listOfEntries.boundsUPPER[[i]],sticky="nw")
      }#end <--*for*

      #if asymmetric bounds we need an additional second input field
      if(BoundsSymmetry==3)
      {
        for(i in 1:n)
        {
          #create label in a list thus we can dynamically change number of input fields
          listOfTimePointLabel.boundsLOWER<<-c(listOfTimePointLabel.boundsLOWER,list(tklabel(manualBoundsLOWERframe.InputFields, text=paste("time",as.character(i)))))

          #We need a list of Input Fields to be able to save the dynamic created tclVar's
          listOfInputFields.boundsLOWER<<-c(listOfInputFields.boundsLOWER,list(tclVar(as.character(lowerBounds[i]))))
          listOfEntries.boundsLOWER<<-c(listOfEntries.boundsLOWER, list(tkentry(manualBoundsLOWERframe.InputFields,width="11",textvariable=as.character(listOfInputFields.boundsLOWER[[i]]))))

          #put label with Input field
          tkgrid(listOfTimePointLabel.boundsLOWER[[i]],listOfEntries.boundsLOWER[[i]])
          tkgrid.configure(listOfTimePointLabel.boundsLOWER[[i]],sticky="nw")
          tkgrid.configure(listOfEntries.boundsLOWER[[i]],sticky="nw")
        }#end <--*for*
        tkgrid(manualBoundsUPPERframe,manualBoundsLOWERframe,sticky="nw")
      }
      else
      {
        tkgrid(manualBoundsUPPERframe,sticky="nw")
      }

      #put whole frame
      tkgrid(manualBoundsFrame,sticky="w")

    }#end <--*if(enterBoundsManually)*

    else #user deactivated checkbox to enter bounds manual
    {
      #fade out frame containing input fields for manual bounds and fade in truncating bounds checkbox
      tkgrid.forget(manualBoundsUPPERframe)
      tkgrid.forget(manualBoundsLOWERframe)
      tkgrid.forget(manualBoundsFrame)
      tkgrid(TruncateBoundsFrame)

      #check on symmetric or asymmetric bounds used
      if(!BoundsSymmetry==3)
      {
        #symmetric
        #remove labels and input fields and clear lists
        for(i in 1:n)
        {
          #remove labels and input fields
          tkgrid.remove(listOfTimePointLabel.boundsUPPER[[i]],listOfEntries.boundsUPPER[[i]])
        }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.boundsUPPER<<-list()
        listOfTimePointLabel.boundsUPPER<<-list()
        listOfEntries.boundsUPPER<<-list()

      }#end <--*if(!BoundsSymmetry==3)*

      else #asymmetric bounds - do the same as with symmetric for both frames
      {
        for(i in 1:n)
        {
          #remove labels and input fields and clear lists
          tkgrid.remove(listOfTimePointLabel.boundsUPPER[[i]],listOfEntries.boundsUPPER[[i]])
          tkgrid.remove(listOfTimePointLabel.boundsLOWER[[i]],listOfEntries.boundsLOWER[[i]])
        }
        #set the lists to NULL otherwise we would duplicate entries in a next loop
        listOfInputFields.boundsUPPER<<-list()
        listOfTimePointLabel.boundsUPPER<<-list()
        listOfEntries.boundsUPPER<<-list()
        listOfInputFields.boundsLOWER<<-list()
        listOfTimePointLabel.boundsLOWER<<-list()
        listOfEntries.boundsLOWER<<-list()

      }#end <--*else #asymmetric bounds - do the same as with symmetric for both frames*

      #call onBoundsChosen which will fade in the needed frames
      onBoundsChosen()
      tkgrid(computedBoundsFrame)

    }#end <--*else #user deactivated checkbox to enter bounds manual*

  }#end <--*onManualBounds <-function()*


  ####################################################################
  # functions handle a click on CONFIRM-Buttons to choose a function #
  ####################################################################
  onConfirmFunction1 <-function()
  {
    #check whether we got symmetric or asymmetric bounds
    BoundsSymmetry <<- as.numeric(tclvalue(SymmetryValue))

    #get value from listbox and ask for additional parameters if necessary
    if( BoundsSymmetry==1 || BoundsSymmetry==2)
    {
      function1<<-as.numeric(tkcurselection(listBoxFunction1of1))+1
    }
    else
    {
      function1<<-as.numeric(tkcurselection(listBoxFunction1of2))+1
    }

    #check whether user selected a function
    if( length(function1)==0 )
    {
      tkmessageBox(message="You must select a function!",icon="error",type="ok")
    }

    else # handle select
    {
      ### ASYMMETRIC ###
      if(BoundsSymmetry==3)
      {
        #first of all remove earlier input field
        tkgrid.remove(phiLabel1of2,entry.functionParameter1of2)

        #case Power Family
        if (function1==3)
        {
          #set new label and input field
          phiLabel1of2<<-tklabel(additionalParametersFrame1of2,text="Enter Paramter phi>0:")
          entry.functionParameter1of2 <<-tkentry(additionalParametersFrame1of2,width="6",textvariable=phi1of2)
          tkgrid(phiLabel1of2,sticky="w")
          tkgrid(entry.functionParameter1of2,sticky="w")
        }

        #case Hwang-Shih-DeCani family
        else if (function1==4)
             {
               #set new label and input field
               phiLabel1of2<<-tklabel(additionalParametersFrame1of2,text="Enter Parameter phi=/=0:")
               entry.functionParameter1of2 <<-tkentry(additionalParametersFrame1of2,width="6",textvariable=phi1of2)
               tkgrid(phiLabel1of2,sticky="w")
               tkgrid(entry.functionParameter1of2,sticky="w")
             }
             #case no additional parameters needed
             else
             {
               #do nothing else
             }
      }#end <--*if(BoundsSymmetry==3)*

      else ### SYMMETRIC - do the same in other frame###
      {
        #first of all remove earlier input field
        tkgrid.remove(phiLabel1of1,entry.functionParameter1of1)

        #get value from listbox and ask for additional parameters if necessary
        functionInput <<- as.numeric(tkcurselection(listBoxFunction1of1))+1

        #case Power Family
        if (function1==3)
        {
          #set new label and input field
          phiLabel1of1<<-tklabel(additionalParametersFrame1of1,text="Enter Paramter phi>0:")
          entry.functionParameter1of1 <<-tkentry(additionalParametersFrame1of1,width="6",textvariable=phi1of1)
          tkgrid(phiLabel1of1,sticky="w")
          tkgrid(entry.functionParameter1of1,sticky="w")
        }

        #case Hwang-Shih-DeCani family
        else if (function1==4)
             {
               #set new label and input field
               phiLabel1of1<<-tklabel(additionalParametersFrame1of1,text="Enter Parameter phi=/=0:")
               entry.functionParameter1of1 <<-tkentry(additionalParametersFrame1of1,width="6",textvariable=phi1of1)
               tkgrid(phiLabel1of1,sticky="w")
               tkgrid(entry.functionParameter1of1,sticky="w")
             }
             #case no additional parameters needed
             else
             {
               #do nothing else
             }
      }#end <--*else ### SYMMETRIC - do the same in other frame###     *
    }#end <--*else # handle select*
  }#end <--*onConfirmFunction1 <-function()*




  onConfirmFunction2 <-function()
  {
    #get value from listbox and ask for additional parameters if necessary
    function2<<-as.numeric(tkcurselection(listBoxFunction2of2))+1

    #check whether user selected a function
    if( length(function2)==0 )
    {
      tkmessageBox(message="You must have select a function!",icon="error",type="ok")
    }

    else # handle select
    {
      #first of all remove earlier input field
      tkgrid.remove(phiLabel2of2,entry.functionParameter2of2)

      #case Power Family
      if (function2==3)
      {
        #set new label and input field
        phiLabel2of2<<-tklabel(additionalParametersFrame2of2,text="Enter Paramter phi>0:")
        entry.functionParameter2of2 <<-tkentry(additionalParametersFrame2of2,width="6",textvariable=phi2of2)
        tkgrid(phiLabel2of2,sticky="w")
        tkgrid(entry.functionParameter2of2,sticky="w")
      }

      #case Hwang-Shih-DeCani family
      else if (function2==4)
           {
             #set new label and input field
             phiLabel2of2<<-tklabel(additionalParametersFrame2of2,text="Enter Parameter phi=/=0:")
             entry.functionParameter2of2 <<-tkentry(additionalParametersFrame2of2,width="6",textvariable=phi2of2)
             tkgrid(phiLabel2of2,sticky="w")
             tkgrid(entry.functionParameter2of2,sticky="w")
           }
           #case no additional parameters needed
           else
           {
             #do nothing else
           }
    }#end <--*else # handle select*
  }#end <--*onConfirmFunction2 <-function()*



  ##################################################################
  # function handles a click on Radio Button for ASYMMETRIC BOUNDS #
  ##################################################################
  onBoundsChosen <- function()
  {
    #check whether we got asymmetric bounds
    BoundsSymmetry <<- as.numeric(tclvalue(SymmetryValue))

    #check whether bounds are computed or entered manually by user
    #get checkbox value
    enterBoundsManually<-as.numeric(tclvalue(manualBoundsCheckBoxValue))



    #Asymmetric Bounds!
    if( (BoundsSymmetry==1 || BoundsSymmetry==2) & boundBackup!=3)
    {
      #nothing to change
    }
    else if( (BoundsSymmetry==1 || BoundsSymmetry==2) & boundBackup==3)
         {
           #if users enters bounds manually update frames, if necessary
           if(enterBoundsManually)
           {
             tkgrid.forget(manualBoundsUPPERframe)
             tkgrid.forget(manualBoundsLOWERframe)
             tkgrid.forget(manualBoundsFrame)
             onManualBounds()
           }
           #exchange frames
           tkgrid.remove(nonSymmetricBoundsFrame)
           tkgrid(symmetricBoundsFrame,sticky="nw")
         }
         else if(BoundsSymmetry==3 & boundBackup!=3)
              {
                #if users enters bounds manually update frames, if necessary
                if(enterBoundsManually)
                {
                  tkgrid.forget(manualBoundsUPPERframe)
                  tkgrid.forget(manualBoundsLOWERframe)
                  tkgrid.forget(manualBoundsFrame)
                  onManualBounds()
                }
                #exchange frames
                tkgrid.remove(symmetricBoundsFrame)
                tkgrid(nonSymmetricBoundsFrame,sticky="nw")

              }
  #update boundBackup
  boundBackup<<-BoundsSymmetry
  }


  #######################################################
  # function handles a click on Trunate Bounds Checkbox #
  #######################################################
  onTruncateCheckbox <- function()
  {
    #checkbox activated?
    truncateBoundsYesNo <<- as.logical(as.numeric(tclvalue(TruncateBoundsCheckBoxValue)))

    if(truncateBoundsYesNo) #activated
    {
      ##grid edit box
      tkgrid(TruncateDynamicFrame)
    }
    else #deactivated
    {
      #ungrid edit box
      tkgrid.forget(TruncateDynamicFrame)
    }
  }


  ##################################################
  # function handles a click on 'CALCULATE'-Button #
  ##################################################
  OnCalculateInputTask1 <- function()
  {
    readyForCalculate <- TRUE

    #get values from checkboxes, listboxes and radiobuttons
    equallySpacedTimesInput <<- as.logical(as.numeric(tclvalue(equallySpacedTimesCheckBoxValue)))
    secondTimeScaleIsUsedInput <<- as.logical(as.numeric(tclvalue(secondTimeScaleIsUsedCheckBoxValue)))
    BoundsSymmetry <<- as.numeric(tclvalue(SymmetryValue))
    truncateBoundsYesNo <<- as.logical(as.numeric(tclvalue(TruncateBoundsCheckBoxValue)))
    Zvalue<-as.numeric(tclvalue(ZvalueTclVar))

    #get and check power input to be in (0,1]
    confidenceLevel<<-as.numeric(tclvalue(powerTclVar))
    if( !( confidenceLevel>0 & confidenceLevel<1) )
    {
      readyForCalculate<-FALSE
      tkmessageBox(message="Incorrect Power value entered - please correct!",icon="error",type="ok")
    }


    #truncation point set?
    if(truncateBoundsYesNo)
    {
      TruncateBoundsInput <<- abs(as.numeric(tclvalue(boundsTruncation)))
    }
    else
    {
      TruncateBoundsInput<-8
    }

    #evaluate whether function is used to compute bounds or user entered them manually
    if(enterBoundsManually)
    {
      #manually entered bounds
      for(i in 1:n)
      {
        upperBounds[i]<<- as.numeric(tclvalue(listOfInputFields.boundsUPPER[[i]]))
      }

      #elicit lower bounds
      if(BoundsSymmetry==1)
      {
        #one-sided => lower Bounds == -8 (that is -infinity)
        lowerBounds <<- seq(-8,-8,length=n)
      }
      else if(BoundsSymmetry==2)
           {
             #two-sided symmetric
             lowerBounds <<- -upperBounds
           }
           else
           {
             #asymmetric
             for(i in 1:n)
             {
               lowerBounds[i]<<- as.numeric(tclvalue(listOfInputFields.boundsLOWER[[i]]))
             }
           }
    }#end <--*if(enterBoundsManually)*

    else #bounds are computed
    {
      #get chosen function(s) and check alpha
      ###################
      # case asymmetric #
      ###################
      if(BoundsSymmetry==3)
      {
        alpha1 <<- as.numeric(tclvalue(alpha1of2))
        alpha2 <<- as.numeric(tclvalue(alpha2of2))

        #check alpha
        alphaAll<- alpha1 + alpha2
        if( !(alpha1>=0 & alpha1<=1 & alpha2>=0 & alpha2<=1 & alphaAll<=1) )
        {
          readyForCalculate<-FALSE
          tkmessageBox(message="Alpha out of range! Correct it and try again.",icon="error",type="ok")
        }

        #check phi if entered as parameter
        phi1 <<- as.numeric(tclvalue(phi1of2))
        phi2 <<- as.numeric(tclvalue(phi2of2))
        #function for UPPER bounds
        if(function1==3)
        {
          if( !(phi1>0) )
          {
            readyForCalculate<-FALSE
            tkmessageBox(message="Parameter phi in function for UPPER bounds must be >0 !",icon="error",type="ok")
          }
        }
        else if(function1==4)
             {
               if(phi1==0)
               {
                 readyForCalculate<-FALSE
                 tkmessageBox(message="Parameter phi in function for UPPER bounds may NOT be zero!",icon="error",type="ok")
               }
             }

        #same with function for LOWER bounds
        if(function2==3)
        {
          if( !(phi2>0) )
          {
            readyForCalculate<-FALSE
            tkmessageBox(message="Parameter phi in function for LOWER bounds must be >0 !",icon="error",type="ok")
          }
        }
        else if(function2==4)
             {
               if(phi2==0)
               {
                 readyForCalculate<-FALSE
                 tkmessageBox(message="Parameter phi in function for LOWER bounds may NOT be zero!",icon="error",type="ok")
               }
             }
      }#end <--*if(BoundsSymmetry==3)*

      ##################
      # case symmetric #
      ##################
      else #one function cause of symmetric bounds
      {
        alpha1 <<- as.numeric(tclvalue(alpha1of1))
        #check alpha
        if( !(alpha1>=0 & alpha1<=1) )
        {
          readyForCalculate<-FALSE
          tkmessageBox(message="Alpha out of range! Correct it and try again.",icon="error",type="ok")
        }

        #check phi if entered as parameter
        phi1 <<- as.numeric(tclvalue(phi1of1))
        #what function used?
        if(function1==3)
        {
          if( !(phi1>0) )
          {
            readyForCalculate<-FALSE
            tkmessageBox(message="Parameter phi in function must be >0 !",icon="error",type="ok")
          }
        }
        else if(function1==4)
             {
               if(phi1==0)
               {
                 readyForCalculate<-FALSE
                 tkmessageBox(message="Parameter phi in function may NOT be zero!",icon="error",type="ok")
                 }
             }
      }

    }#end <--*else #bounds are computed*
    ##if user typed in unequally spaced times - get them and
    ##check them to be in intervall(0,1] and in right order
    if(!equallySpacedTimesInput)
    {
      tempVal<-0
      interimTimesBad<-FALSE
      for(i in 1:n)
      {
        tempVal[i] <- as.numeric(tclvalue(listOfInputFields.unequalTimes[[i]]))

        if (tempVal[i]<=0) { interimTimesBad<-TRUE }
        if (tempVal[i]>1) { interimTimesBad<-TRUE }
        if (i>1)
        {
          if (tempVal[i]<=tempVal[i-1]) { interimTimesBad<-TRUE }
        }
      }#end <--*for(i in 1:n)*

      ##if times are not good => error and keep old times
        if(interimTimesBad)
        {
          readyForCalculate <- FALSE
          tkmessageBox(message="Bad Interim Times entered - old Times are kept so far! ",icon="error",type="ok")
        }
        ##else take new times
        else
        {
          t<<-tempVal
        }

    }#end <--*if(equallySpacedTimesInput)*
    else
    {
     for(i in 1:n)
     {
       t[i]<<-i/n
     }
    }

    ##if user typed in second time scales - get them and
    ##check them to be in intervall(0,1] and in right order
    if(secondTimeScaleIsUsedInput)
    {
      tempVal<-0
      secondTimesBad<-FALSE
      for(i in 1:n)
      {
        tempVal[i] <- as.numeric(tclvalue(listOfInputFields.secondTimes[[i]]))

        if (tempVal[i]<=0) { secondTimesBad<-TRUE }
        if (tempVal[i]>1) { secondTimesBad<-TRUE }
        if (i>1)
        {
          if (tempVal[i]<=tempVal[i-1]) { secondTimesBad<-TRUE }
        }
      }#end <--*for(i in 1:n)*

      ##if times are not good => error and keep old times
        if(secondTimesBad)
        {
          readyForCalculate <- FALSE
          tkmessageBox(message="Bad Second Time Scale entered - old Times are kept so far! ",icon="error",type="ok")
        }
        ##else take new times
        else
        {
          t2<<-tempVal
        }

      if(readyForCalculate)
      {

        ###########################################################################
        ############### RESCALE SECOND TIME SCALE IF NECESSARY ####################
        ###########################################################################
        # When second time scale is not on (0,1], computation with
        # non-zero drift parameters are incorrect, since the drift
        # always scaled to (0,1].  If the trial is complete (t[n]=1)
        # then t2 can be rescaled as t3 = t2/t2[n].  Otherwise, if
        # the second time scale is to be used for covariances, the
        # user must enter a maximum value.
        #
        # drift[ t[i] ] = drift*t[i]
        #
        # Start with t3 = t2.
        # (t2=t by default e.g. if user did not enter second time scale.)
        t3<<-t2
        t2max<<-0

        ##If t[n]=1, t2[n] is maximum of t2.
        if(t[n]==1)
        {
         tkmessageBox(title="-4- Compute Confidence Interval",message="Second Time scale will be used to determine covariances.",icon="info",type="ok")
         t2max<<-t2[n]
         t3<<-t2/t2max
        }
        else ##Should we try to use 2nd scale?
        {
          response<-tkmessageBox(message="Do you wish to use the 2nd time scale to determine covariances?",
                                 icon="question",type="yesno",default="yes")


          ##--If yes, prompt for maximum of 2nd time scale.--##
          if( as.character(tclvalue(response))=="yes" )
          {
            ##################################################
            # function handles prompting for t2max if needed #
            ##################################################
            t2maxPrompt <- function(title,question,entryInit,entryWidth=4,returnValOnCancel="ID_CANCEL")
            {
          dlg <- tktoplevel(taskWindow)
              tkwm.deiconify(dlg)
              tkgrab.set(dlg)
              tkfocus(dlg)
              tkwm.title(dlg,title)
              textEntryVarTcl <- tclVar(paste(entryInit))
              textEntryWidget <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl)
              tkgrid(tklabel(dlg,text="       "))
              tkgrid(tklabel(dlg,text=question),textEntryWidget)
              tkgrid(tklabel(dlg,text="       "))
              ReturnVal <- returnValOnCancel
              onOK <- function()
              {
                ReturnVal <<- as.numeric(tclvalue(textEntryVarTcl))
                #check whether numeric was entered
                if(is.na(ReturnVal))
                {
                  tkmessageBox(title="ERROR",message="You did not enter a valid numeric value!",icon="error",type="ok")
                }
                #if input numeric check whether the numeric is a valid entry
                else
                {
                  if(ReturnVal<=0)
                  {
                    tkmessageBox(title="ERROR",message="Maximum must be positive! Please try again!",icon="error",type="ok")
                  }
                  else if(ReturnVal<t2[n])
                       {
                         tkmessageBox(title="ERROR",message="The Maximum cannot be smaller than your last seond time scale value!",icon="error",type="ok")
                       }

                       #input ok - go back to main window
                       else
                       {
                         tkgrab.release(dlg)
                         tkdestroy(dlg)
                         tkfocus(task4)
                       }
                }

              }
              onCancel <- function()
              {
                readyForCalculate<<-FALSE
                ReturnVal <<- returnValOnCancel
                tkgrab.release(dlg)
                tkdestroy(dlg)
                tkfocus(task4)
              }
              OK.but     <-tkbutton(dlg,text="   OK   ",command=onOK)
              Cancel.but <-tkbutton(dlg,text=" Cancel ",command=onCancel)
              tkgrid(OK.but,Cancel.but)
              tkgrid(tklabel(dlg,text="    "))

              tkfocus(dlg)
              tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkfocus(task4)})
              tkbind(textEntryWidget, "<Return>", onOK)
              tkwait.window(dlg)

              return(ReturnVal)

            }#end <--*t2maxPrompt <- function(...)*


            ReturnVal<-t2maxPrompt("Second Time Scale will be used","Enter the maximum value of the second time scale","" )
            if(ReturnVal!="ID_CANCEL")
            {
              t2max<<-ReturnVal
              #Rescale t2
              t3<<-t2/t2max
            }

          }#end <--*if( as.character(tclvalue(response))=="yes" )*

          else if( as.character(tclvalue(response))=="no" )
          {
            ##Even if the 2nd time scale was on (0,1], if the
            ##maximum information value was not entered, set
            ##t3 to t, which causes t2 to be ignored!#
            t3<<-t

          }

        }#end <--*else ##Should we try to use 2nd scale?*

      }#end <--*if(readyForCalculate)*

    }#end <--*if(secondTimeScaleIsUsedInput)*
    else
    {
      ##set t3 to t, which causes t2 to be ignored!
      t3<<-t
    }

    if(readyForCalculate)
    {
      # second time scale is not used so far --> set t3=t2=t
      t2<-t; t3<-t
      calculateTask4(n,nMax,t,t2,t2max,t3,confidenceLevel,equallySpacedTimesInput,secondTimeScaleIsUsedInput,
                     BoundsSymmetry, c(alpha1,alpha2), c(phi1,phi2), c(function1,function2),TruncateBoundsInput,
                     enterBoundsManually, upperBounds, lowerBounds, Zvalue, taskWindow)
    }
  }#end <--*OnCalculateInputTask1 <- function()*



#######################################################################################################
#################------------------------------------------------------------------####################
##################  FROM HERE ON LABELS AND FRAMES ARE CREATED AND PUT TOGETHER   #####################
#################------------------------------------------------------------------####################
#######################################################################################################

  #Set Toplevel
  task4 <- tktoplevel(taskWindow)
  tkwm.title(task4,"-4- Compute Confidence Interval")

  #Define main Frame
  InputTask4 <- tkframe(task4, relief="groove",borderwidth=2)


  ##--------------------------------------------------------------------------------##
  ##------------------------  number of Interim Times  -----------------------------##
  ##--------------------------------------------------------------------------------##

  #create pull down menu to select interim analyses from n==1 to n==25(=nMax)
  topMenu <- tkmenu(task4)
  tkconfigure(task4,menu=topMenu)
  nMenu <- tkmenu(topMenu,tearoff=FALSE,background="grey",activebackground="red")
  tkadd(nMenu,"command",label="1",command=function() onChangeInterimAnalyses(1))
  tkadd(nMenu,"command",label="2",command=function() onChangeInterimAnalyses(2))
  tkadd(nMenu,"command",label="3",command=function() onChangeInterimAnalyses(3))
  tkadd(nMenu,"command",label="4",command=function() onChangeInterimAnalyses(4))
  tkadd(nMenu,"command",label="5",command=function() onChangeInterimAnalyses(5))
  tkadd(nMenu,"command",label="6",command=function() onChangeInterimAnalyses(6))
  tkadd(nMenu,"command",label="7",command=function() onChangeInterimAnalyses(7))
  tkadd(nMenu,"command",label="8",command=function() onChangeInterimAnalyses(8))
  tkadd(nMenu,"command",label="9",command=function() onChangeInterimAnalyses(9))
  tkadd(nMenu,"command",label="10",command=function() onChangeInterimAnalyses(10))
  tkadd(nMenu,"command",label="11",command=function() onChangeInterimAnalyses(11))
  tkadd(nMenu,"command",label="12",command=function() onChangeInterimAnalyses(12))
  tkadd(nMenu,"command",label="13",command=function() onChangeInterimAnalyses(13))
  tkadd(nMenu,"command",label="14",command=function() onChangeInterimAnalyses(14))
  tkadd(nMenu,"command",label="15",command=function() onChangeInterimAnalyses(15))
  tkadd(nMenu,"command",label="16",command=function() onChangeInterimAnalyses(16))
  tkadd(nMenu,"command",label="17",command=function() onChangeInterimAnalyses(17))
  tkadd(nMenu,"command",label="18",command=function() onChangeInterimAnalyses(18))
  tkadd(nMenu,"command",label="19",command=function() onChangeInterimAnalyses(19))
  tkadd(nMenu,"command",label="20",command=function() onChangeInterimAnalyses(20))
  tkadd(nMenu,"command",label="21",command=function() onChangeInterimAnalyses(21))
  tkadd(nMenu,"command",label="22",command=function() onChangeInterimAnalyses(22))
  tkadd(nMenu,"command",label="23",command=function() onChangeInterimAnalyses(23))
  tkadd(nMenu,"command",label="24",command=function() onChangeInterimAnalyses(24))
  tkadd(nMenu,"command",label="25",command=function() onChangeInterimAnalyses(25))
  tkadd(topMenu,"cascade",label=paste("#Interim Times: K= ",as.character(n)),menu=nMenu)

  tkgrid(tklabel(InputTask4,text="")) # Blank line


  ##--------------------------------------------------------------------------------##
  ##-------------  Interim Times equally or unequally spaced? ----------------------##
  ##-------------       Second Time Scale will be used?       ----------------------##
  ##--------------------------------------------------------------------------------##

  ## prepare Frames
  interimTimesFrame<- tkframe(InputTask4,relief="groove",borderwidth=0)
  equallySpacedTimesFrame <- tkframe(interimTimesFrame,relief="groove",borderwidth=0)
  secondTimesFrame <- tkframe(interimTimesFrame,relief="groove",borderwidth=0)

  #again we need a frame for dynamic working in it to not affect
  #the format of 'equallySpacedTimesFrame' respective 'secondTimesFrame'
  equallySpacedLabelFrame<-tkframe(equallySpacedTimesFrame,relief="groove",borderwidth=0)
  unEquallyDynamicFrame<-tkframe(equallySpacedTimesFrame,relief="groove",borderwidth=0)
  secondTimesLabelFrame<-tkframe(secondTimesFrame,relief="groove",borderwidth=0)
  secondTimesDynamicFrame<-tkframe(secondTimesFrame,relief="groove",borderwidth=0)

  #Default is Equally Spaced Times and no Second Time Scale
  #create Checkboxes
  #equally spaced
  equallySpacedTimesCheckBox<-tkcheckbutton(equallySpacedLabelFrame,command=onCheckBoxEquallySpacedTimes)
  equallySpacedTimesCheckBoxValue <- tclVar(as.character(as.numeric(equallySpacedTimesInput)))
  tkconfigure(equallySpacedTimesCheckBox,variable=equallySpacedTimesCheckBoxValue)
  #second time scale
  secondTimeScaleIsUsedCheckBox<-tkcheckbutton(secondTimesLabelFrame,command=onCheckBoxSecondTimeScale)
  secondTimeScaleIsUsedCheckBoxValue <- tclVar(as.character(as.numeric(secondTimeScaleIsUsedInput)))
  tkconfigure(secondTimeScaleIsUsedCheckBox,variable=secondTimeScaleIsUsedCheckBoxValue)

  #put checkbox and other frames together
  equallyTimesBoxLabel<-tklabel(equallySpacedLabelFrame,text="Equally Spaced Times")
  secondTimesBoxLabel<-tklabel(secondTimesLabelFrame,text="Use Second Time Scale")
  tkgrid(equallyTimesBoxLabel,equallySpacedTimesCheckBox)
  # tkgrid(secondTimesBoxLabel,secondTimeScaleIsUsedCheckBox) - this feature is currently removed
  tkgrid(equallySpacedTimesFrame,secondTimesFrame,sticky="n")
  tkgrid(equallySpacedLabelFrame,sticky="n")
  tkgrid(secondTimesLabelFrame,sticky="n")
  tkgrid(unEquallyDynamicFrame,sticky="nw")
  tkgrid(secondTimesDynamicFrame,sticky="n")
  tkgrid(interimTimesFrame,sticky="w")
  tkgrid(tklabel(InputTask4,text="")) # Blank line


  ##-- Level for Confidence Intervall --##
  ## confidence Intervall replaces last bound with Zvalue
  ZvalueTclVar<-tclVar(as.character(Zvalue))
  ZvalueFrame <- tkframe(InputTask4,relief="groove",borderwidth=0)
  ZvalueLabel<-tklabel(ZvalueFrame,text="Enter the standardized statistic (Z value) at the last analysis:")
  entry.Zvalue <-tkentry(ZvalueFrame,width="6",textvariable=ZvalueTclVar)
  #grid it
  tkgrid(ZvalueLabel,entry.Zvalue, sticky="w")
  tkgrid(ZvalueFrame,sticky="w")


  #Desired Power
  powerTclVar<-tclVar(as.character(confidenceLevel))
  powerFrame <- tkframe(InputTask4,relief="groove",borderwidth=0)
  powerLabel<-tklabel(powerFrame,text="Enter Confidence Level - it must be in (0,1) :")
  entry.power <-tkentry(powerFrame,width="6",textvariable=powerTclVar)
  #grid it
  tkgrid(powerLabel,entry.power, sticky="w")
  tkgrid(tklabel(powerFrame,text="")) # Blank line
  tkgrid(powerFrame,sticky="w")

  ###One- or Two-Sided Bounds or asymmetric Bounds###
  #create frames
  boundsLabelFrame <- tkframe(InputTask4,relief="groove",borderwidth=0)
  boundsRadioButtonFrame <- tkframe(InputTask4,relief="groove",borderwidth=0)

  #create radio buttons
  oneSided <- tkradiobutton(boundsRadioButtonFrame,command=onBoundsChosen)
  twoSided <- tkradiobutton(boundsRadioButtonFrame,command=onBoundsChosen)
  asymmetric <- tkradiobutton(boundsRadioButtonFrame,command=onBoundsChosen)
  SymmetryValue <- tclVar(as.character(BoundsSymmetry))
  tkconfigure(oneSided,variable=SymmetryValue,value="1")
  tkconfigure(twoSided,variable=SymmetryValue,value="2")
  tkconfigure(asymmetric,variable=SymmetryValue,value="3")

  #grid labels and buttons together
  tkgrid(tklabel(boundsLabelFrame,text="One-, Two-sided symmetric or asymmetric bounds?"),sticky="w")
  tkgrid(tklabel(boundsRadioButtonFrame,text="One-Sided "),oneSided)
  tkgrid(tklabel(boundsRadioButtonFrame,text="Two-Sided "),twoSided)
  tkgrid(tklabel(boundsRadioButtonFrame,text="Asymmetric "),asymmetric)

  #put frames
  tkgrid(boundsLabelFrame,sticky="w")
  tkgrid(boundsRadioButtonFrame,sticky="w")
  tkgrid(tklabel(InputTask4,text="")) # Blank line

  ################################################################################
  ### User could enter bounds manually instead of let them be computed (default)##
  ################################################################################
  manualOrComputedBoundsFrame<-tkframe(InputTask4,relief="groove",borderwidth=0)
  LabelManualOrComputedBoundsFrame<-tkframe(manualOrComputedBoundsFrame,relief="groove",borderwidth=0)
  computedBoundsFrame<-tkframe(manualOrComputedBoundsFrame,relief="groove",borderwidth=0)
  manualBoundsFrame<-tkframe(manualOrComputedBoundsFrame,relief="groove",borderwidth=0)
  #upper bounds
  manualBoundsUPPERframe<-tkframe(manualBoundsFrame,relief="groove",borderwidth=0)
  manualBoundsUPPERframe.Label<-tkframe(manualBoundsUPPERframe,relief="groove",borderwidth=0)
  manualBoundsUPPERframe.InputFields<-tkframe(manualBoundsUPPERframe,relief="groove",borderwidth=0)
  #lower bounds
  manualBoundsLOWERframe<-tkframe(manualBoundsFrame,relief="groove",borderwidth=0)
  manualBoundsLOWERframe.Label<-tkframe(manualBoundsLOWERframe,relief="groove",borderwidth=0)
  manualBoundsLOWERframe.InputFields<-tkframe(manualBoundsLOWERframe,relief="groove",borderwidth=0)

  #create checkbox with label
  manualBoundsCheckbox <- tkcheckbutton(LabelManualOrComputedBoundsFrame,command=onManualBounds)
  manualBoundsCheckBoxValue <- tclVar(as.character(as.numeric(enterBoundsManually)))
  tkconfigure(manualBoundsCheckbox,variable=manualBoundsCheckBoxValue)
  manualBoundsCheckboxLabel<-tklabel(LabelManualOrComputedBoundsFrame,text="Enter Bounds Manually")
  tkgrid(manualBoundsCheckboxLabel,manualBoundsCheckbox)
  tkgrid(tklabel(LabelManualOrComputedBoundsFrame,text="")) # Blank line
  tkgrid.configure(manualBoundsCheckboxLabel,sticky="w")
  tkgrid.configure(manualBoundsCheckbox,sticky="w")

  #create labels for manual entering and grid alltogether
  manualBoundsUPPERlabel<-tklabel(manualBoundsUPPERframe.Label,text="Enter UPPER Bounds(standardized)     ")
  tkgrid(manualBoundsUPPERlabel,sticky="w")
  tkgrid(manualBoundsUPPERframe.Label,sticky="w")
  tkgrid(manualBoundsUPPERframe.InputFields,sticky="w")
  manualBoundsLOWERlabel<-tklabel(manualBoundsLOWERframe.Label,text="Enter LOWER Bounds (standardized)")
  tkgrid(manualBoundsLOWERlabel,sticky="w")
  tkgrid(manualBoundsLOWERframe.Label,sticky="w")
  tkgrid(manualBoundsLOWERframe.InputFields,sticky="w")

  ### Significance Level(s) alpha and function(s) to be used to calculate bounds###
  ## if user choses asymmetric bounds two different functions could be used ##
  ##create Frames
  alphaAndFunctionsFrame<-tkframe(computedBoundsFrame,relief="groove",borderwidth=0)
  symmetricBoundsFrame<-tkframe(alphaAndFunctionsFrame,relief="groove",borderwidth=0)
  nonSymmetricBoundsFrame<-tkframe(alphaAndFunctionsFrame,relief="groove",borderwidth=0)

  ##Default alpha1==0.05, alpha2==0
  alpha1of1 <- tclVar(as.character(alphaInput))
  alpha1of2 <- tclVar(as.character("0.025"))
  alpha2of2 <- tclVar(as.character("0.025"))

  #########################################################
  ### case symmetric bounds or one-sided test (default) ###
  #########################################################
  #frames
  alphaFrame1of1 <- tkframe(symmetricBoundsFrame,relief="groove",borderwidth=0)
  functionsFrame1of1 <- tkframe(symmetricBoundsFrame,relief="groove",borderwidth=0)
  additionalParametersFrame1of1<-tkframe(symmetricBoundsFrame,relief="groove",borderwidth=0)

  ##create Labels for alpha
  alphaLabel1of1<-tklabel(alphaFrame1of1,text="Significance Level: alpha=")
  entry.alpha1of1 <-tkentry(alphaFrame1of1,width="6",textvariable=alpha1of1)

  #create Listbox for function choice
  functionLabel1of1<-tklabel(functionsFrame1of1,text="What function should be used?")
  listBoxFunction1of1<-tklistbox(functionsFrame1of1,height=5,width=30,selectmode="single",background="grey")
  functionChoice1of1 <- c("(1) O'Brien-Fleming Type","(2) Pocock Type",
    "(3) Power Family: alpha* t^phi","(4) Hwang-Shih-DeCani Family","(5) Exact Pocock Bounds")
  for (i in (1:5))
  {
    tkinsert(listBoxFunction1of1,"end",functionChoice1of1[i])
  }
  tkselection.set(listBoxFunction1of1, functionInput-1)  # Default function is O'Brien-Fleming Type.  Indexing starts at zero.

  #create and put button to confirm a function because for example in case of 'Power family: alpha* t^phi'
  #user has to enter additional parameter 'phi'
  confirmFun.button1of1 <-tkbutton(functionsFrame1of1,text=" CONFIRM FUNCTION ",command=onConfirmFunction1)

  #create variable for edit box which we will need if additional parameters must be entered
  #edit box is unvisible at beginning since default function O'Brien-Fleming Type does not need any additional parameters
  phi1of1 <- tclVar(as.character(phi1))
  phiLabel1of1<-tklabel(additionalParametersFrame1of1,text="")
  entry.functionParameter1of1 <-tkentry(additionalParametersFrame1of1,width="3",textvariable=phi1of1)

  #grid together
  #alpha
  tkgrid(alphaLabel1of1,entry.alpha1of1)
  tkgrid.configure(alphaLabel1of1,sticky="w")
  tkgrid.configure(entry.alpha1of1,sticky="w")
  tkgrid(tklabel(alphaFrame1of1,text="")) # Blank line
  tkgrid(functionLabel1of1,sticky="w")
  tkgrid(listBoxFunction1of1)

  #put frames and button
  tkgrid(alphaFrame1of1,sticky="w")
  tkgrid(functionsFrame1of1,additionalParametersFrame1of1)
  tkgrid(confirmFun.button1of1)
  tkgrid.configure(functionsFrame1of1,sticky="w")
  tkgrid(tklabel(symmetricBoundsFrame,text="")) # Blank line

  #Finally grid frame for symmetric case as default
  tkgrid(LabelManualOrComputedBoundsFrame,sticky="w")
  tkgrid(symmetricBoundsFrame,sticky="nw")
  tkgrid(alphaAndFunctionsFrame,sticky="w")
  tkgrid(computedBoundsFrame,sticky="w")
  tkgrid(manualOrComputedBoundsFrame,sticky="w")


  ##############################
  ### case Asymmetric bounds ###
  ##############################
  #frames
  alphaFrame1of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  functionsFrame1of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  additionalParametersFrame1of2<-tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  alphaFrame2of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  functionsFrame2of2 <- tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)
  additionalParametersFrame2of2<-tkframe(nonSymmetricBoundsFrame,relief="groove",borderwidth=0)

  ##create Labels for alpha
  alphaLabel1of2<-tklabel(alphaFrame1of2,text="UPPER Bounds: alpha=")
  entry.alpha1of2 <-tkentry(alphaFrame1of2,width="6",textvariable=alpha1of2)

  alphaLabel2of2<-tklabel(alphaFrame2of2,text="LOWER Bounds: alpha=")
  entry.alpha2of2 <-tkentry(alphaFrame2of2,width="6",textvariable=alpha2of2)

  #create Listboxes for function choice
  #################
  # List Box 1of2 #
  #################
  functionLabel1of2<-tklabel(functionsFrame1of2,text="Choose Function for UPPER Bounds")
  listBoxFunction1of2<-tklistbox(functionsFrame1of2,height=5,width=30,selectmode="single",background="grey")
  functionChoice1of2 <- c("(1) O'Brien-Fleming Type","(2) Pocock Type",
    "(3) Power Family: alpha* t^phi","(4) Hwang-Shih-DeCani Family","(5) Exact Pocock Bounds")
  for (i in (1:5))
  {
    tkinsert(listBoxFunction1of2,"end",functionChoice1of2[i])
  }

  #create and put first Confirm button which "commands" same function as in symmetric case did
  confirmFun.button1of2 <-tkbutton(functionsFrame1of2,text=" CONFIRM FUNCTION ",command=onConfirmFunction1)
  #edit box for parameter phi[1]
  phi1of2 <- tclVar(as.character(phi1))
  phiLabel1of2<-tklabel(additionalParametersFrame1of2,text="")
  entry.functionParameter1of2 <-tkentry(additionalParametersFrame1of2,width="3",textvariable=phi1of2)

  #################
  # List Box 2of2 #
  #################
  functionLabel2of2<-tklabel(functionsFrame2of2,text="Choose Function for LOWER Bounds")
  listBoxFunction2of2<-tklistbox(functionsFrame2of2,height=5,width=30,selectmode="single",background="grey")
  functionChoice2of2 <- c("(1) O'Brien-Fleming Type","(2) Pocock Type",
    "(3) Power Family: alpha* t^phi","(4) Hwang-Shih-DeCani Family","(5) Exact Pocock Bounds")
  for (i in (1:5))
  {
    tkinsert(listBoxFunction2of2,"end",functionChoice2of2[i])
  }

  #create and put first Confirm button
  confirmFun.button2of2 <-tkbutton(functionsFrame2of2,text=" CONFIRM FUNCTION ",command=onConfirmFunction2)

  #edit box for parameter phi[2]
  phi2of2 <- tclVar(as.character(phi2))
  phiLabel2of2<-tklabel(additionalParametersFrame2of2,text="")
  entry.functionParameter2of2 <-tkentry(additionalParametersFrame2of2,width="3",textvariable=phi2of2)

  #grid together

  #1of2
  tkgrid(alphaLabel1of2,entry.alpha1of2)
  tkgrid.configure(alphaLabel1of2,sticky="w")
  tkgrid.configure(entry.alpha1of2,sticky="w")
  tkgrid(functionLabel1of2,sticky="w")
  tkgrid(listBoxFunction1of2)

  #put frames and button
  tkgrid(alphaFrame1of2,sticky="w")
  tkgrid(functionsFrame1of2,additionalParametersFrame1of2)
  tkgrid(confirmFun.button1of2)
  tkgrid.configure(functionsFrame1of2,sticky="w")
  tkgrid(tklabel(nonSymmetricBoundsFrame,text="")) # Blank line


  #2of2
  tkgrid(alphaLabel2of2,entry.alpha2of2)
  tkgrid.configure(alphaLabel2of2,sticky="w")
  tkgrid.configure(entry.alpha2of2,sticky="w")
  tkgrid(functionLabel2of2,sticky="w")
  tkgrid(listBoxFunction2of2)

  #put frames and button
  tkgrid(alphaFrame2of2,sticky="w")
  tkgrid(functionsFrame2of2,additionalParametersFrame2of2)
  tkgrid(confirmFun.button2of2)
  tkgrid.configure(functionsFrame2of2,sticky="w")
  tkgrid(tklabel(nonSymmetricBoundsFrame,text="")) # Blank line

  #################################################################

  ###Truncate Bounds?###
  PositionTruncateBoundsFrame<-tkframe(InputTask4,relief="groove",borderwidth=0)
  TruncateBoundsFrame <- tkframe(PositionTruncateBoundsFrame,relief="groove",borderwidth=0)
  TruncateLabelFrame <- tkframe(TruncateBoundsFrame,relief="groove",borderwidth=0)
  TruncateDynamicFrame <-tkframe(TruncateBoundsFrame,relief="groove",borderwidth=0)

  #create checkbox
  TruncateBoundsCheckBox<-tkcheckbutton(TruncateLabelFrame,command=onTruncateCheckbox)
  TruncateBoundsCheckBoxValue <- tclVar(as.character(as.numeric(truncateBoundsYesNo)))
  tkconfigure(TruncateBoundsCheckBox,variable=TruncateBoundsCheckBoxValue)

  #create variable for edit box which we will need if user wants truncation of bounds -
  boundsTruncation <- tclVar(as.character(TruncateBoundsInput))
  boundsTruncationLabel<-tklabel(TruncateDynamicFrame,text="Enter Truncation Point:")
  entry.truncationValue <-tkentry(TruncateDynamicFrame,width="3",textvariable=boundsTruncation)

  #put frames
  tkgrid(tklabel(TruncateLabelFrame,text="Truncate standardized Bounds?"),TruncateBoundsCheckBox)
  tkgrid(boundsTruncationLabel,entry.truncationValue,sticky="w")
  tkgrid(TruncateLabelFrame,TruncateDynamicFrame,sticky="w")
  tkgrid(TruncateBoundsFrame,sticky="w")
  tkgrid(PositionTruncateBoundsFrame,sticky="w")
  tkgrid.forget(TruncateDynamicFrame) #default is no Truncating
  tkgrid(tklabel(InputTask4,text="")) # Blank line

  ##put Overall Frame
  tkgrid(InputTask4)


  #frame for the buttons
  buttonFrame<-tkframe(task4,relief="groove",borderwidth=0)

  #create and put button for calculating
  calculate.button <-tkbutton(buttonFrame,text=" CALCULATE ",command=OnCalculateInputTask1)

  # function handles click onto button to Cancel i.e. close current window
  onCancel <- function()
  {
   tkdestroy(task4)
  }
  cancel.button <-tkbutton(buttonFrame,text=" Cancel ",command=onCancel)

  # grid buttons
  tkgrid( tklabel(buttonFrame, text=""))   #blank line
  tkgrid(calculate.button, tklabel(buttonFrame, text="            "),
         cancel.button, sticky="we" )
  tkgrid( tklabel(buttonFrame, text=""))   #blank line
  tkgrid(buttonFrame)

  tkfocus(task4)

}


guiMode <- function()
{
  pkg.env$taskWindow <- tcltk::tktoplevel()
  taskWindow <- pkg.env$taskWindow

  logofile <- system.file("img", "logo.gif", package = "GroupSeq")
  tkimage.create("photo", "::image::logoIcon", file = logofile)
  tcl("wm", "iconphoto", taskWindow, "-default", "::image::logoIcon")

  tkwm.title(taskWindow,"Choose a Task")
  listBoxTasks<-tklistbox(taskWindow,height=4,width=40,selectmode="single",background="white")
  tkgrid(tklabel(taskWindow,text="Select a Task!"))
  tkgrid(listBoxTasks)
  tasks <- c("-1- Compute Bounds.","-2- Compute Drift given Power and Bounds.",
    "-3- Compute Probabilities given Bounds and Drift.","-4- Compute Confidence Interval.")
  for (i in (1:4))
  {
    tkinsert(listBoxTasks,"end",tasks[i])
  }
  tkselection.set(listBoxTasks,0)  # Default task is Task -1-.  Indexing starts at zero.

  OnOKtaskWindow <- function()
  {
    taskChoice <- as.numeric(tkcurselection(listBoxTasks))+1
    if(length(taskChoice)<1)
    {
     tkmessageBox(message="You must select a task!",icon="error",type="ok")
    }
    else
    {
     #call according function
     switch(taskChoice, guiInputTask1(taskWindow), guiInputTask2(taskWindow),
                       guiInputTask3(taskWindow), guiInputTask4(taskWindow) )
    }
  }

  OK.button <-tkbutton(taskWindow,text="   Perform Selected Task   ",command=OnOKtaskWindow)
  Quit.buttton <- tkbutton(taskWindow,text="  QUIT GroupSeq  ",command=onQuit)

  # place buttons
  tkgrid(OK.button)
  tkgrid(tklabel(taskWindow,text="")) # Blank line
  tkgrid(Quit.buttton)
  tkgrid(tklabel(taskWindow,text="")) # Blank line

  tkfocus(taskWindow)
}


"guiOutputTask1" <-
function(K,alpha,phi,t,lowerBounds,upperBounds,probDifference,probExit,
         BoundsSymmetry,spendingFunctionUsed, taskWindow)
{
  # Initializing
  FunctionNames=NULL;
  FunctionNamesUpper=NULL;
  FunctionNamesLower=NULL;

  #Set Toplevel
  outTask1Toplevel <- tktoplevel(taskWindow)

  if(!BoundsSymmetry==3)
  {
    tkwm.title(outTask1Toplevel,paste("-1-    K=",K,  ", alpha=",alpha[1]))
  }
  else
  {
    tkwm.title(outTask1Toplevel,paste("-1-    ,K=",K,  ", alphaUPPER=",alpha[1],", alphaLOWER=",alpha[2]))
  }

  #Define main Frame
  OutputTask1 <- tkframe(outTask1Toplevel)

  #Define subframes
  staticFrame <- tkframe(OutputTask1,relief="groove",borderwidth=0)
  dynamicFrame <- tkframe(OutputTask1,relief="groove",borderwidth=0)
  parametersFrame <- tkframe(staticFrame,relief="groove",borderwidth=0)
  numbersFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  timesFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  lowerBoundsFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  upperBoundsFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  probDiffFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  probExitFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)

  #create label with parameter values:
  tkgrid( tklabel(parametersFrame, text=paste("K =",K)),sticky="w")
  if(!BoundsSymmetry==3)
  {
    tkgrid( tklabel(parametersFrame, text=paste("alpha =",alpha[1])),sticky="w")
  }
  else
  {
    tkgrid( tklabel(parametersFrame, text=paste("alpha - Upper Bounds =",alpha[1])),sticky="w")
    tkgrid( tklabel(parametersFrame, text=paste("alpha - Lower Bounds =",alpha[2])),sticky="w")

  }

  if(!BoundsSymmetry==3)
  {
    ##names of spending functions that could have been used
    FunctionNames <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: alpha*t^",phi[1],sep=""),
                       paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")

    # substitute according funtion in output
    tkgrid( tklabel(parametersFrame, text=paste("Function: ",FunctionNames[[spendingFunctionUsed[1]]])),sticky="w")
  }
  else
  {
    ##names of spending functions that could have been used
    FunctionNamesUpper <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: alpha*t^",phi[1],sep=""),
                            paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")
    FunctionNamesLower <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: alpha*t^",phi[2],sep=""),
                            paste("Hwang-Shih-DeCani Family ( phi =",phi[2],")"),"Exact Pocock Bounds")

    # substitute according funtion in output
    tkgrid( tklabel(parametersFrame, text=paste("Function - Upper Bounds: ",FunctionNamesUpper[spendingFunctionUsed[1]])),sticky="w")
    tkgrid( tklabel(parametersFrame, text=paste("Function - Lower Bounds: ",FunctionNamesLower[spendingFunctionUsed[2]])),sticky="w")
  }

  #create head labels
  tkgrid( tklabel(numbersFrame, text="k    "),sticky="w")
  tkgrid( tklabel(timesFrame, text="Times   "),sticky="w")
  tkgrid( tklabel(lowerBoundsFrame, text="Lower Bounds  "),sticky="w")
  tkgrid( tklabel(upperBoundsFrame, text="Upper Bounds  "),sticky="w")
  tkgrid( tklabel(probDiffFrame, text="alpha[i]-alpha[i-1]  "),sticky="w")
  tkgrid( tklabel(probExitFrame, text="cumulative alpha  "),sticky="w")

  #create labels with results
  for(i in 1:K)
  {
    tkgrid( tklabel(numbersFrame, text=as.character(i)),sticky="w")
    tkgrid( tklabel(timesFrame, text=as.character(round(t[i],digits=3))),sticky="w")
    tkgrid( tklabel(lowerBoundsFrame, text=as.character(round(lowerBounds[i],digits=4))),sticky="w")
    tkgrid( tklabel(upperBoundsFrame, text=as.character(round(upperBounds[i],digits=4))),sticky="w")
    tkgrid( tklabel(probDiffFrame, text=as.character(round(probDifference[i],digits=10))),sticky="w")
    tkgrid( tklabel(probExitFrame, text=as.character(round(probExit[i],digits=10))),sticky="w")
  }
  tkgrid( tklabel(dynamicFrame, text=""))   #blank line

  #put frames together
  tkgrid(parametersFrame,sticky="w")
  tkgrid(numbersFrame, timesFrame, lowerBoundsFrame, upperBoundsFrame, probDiffFrame, probExitFrame, sticky="w")
  tkgrid(staticFrame,sticky="w")
  tkgrid(dynamicFrame,sticky="w")


  ###########################################################################
  ##function handles click onto button to show results of bounds in a graph##
  ###########################################################################
  onShowGraph <- function()
  {
    lines = graphics::lines

    ## if one-Sided-Test we won't see negative Z-Values
      if(BoundsSymmetry==1)
      {
        xCoordinate<-t
        yCoordinate<-upperBounds

        ## first plotting bounds as points...
        plot(xCoordinate,yCoordinate,main=paste("-1-  K=",K,
             "\n Function:", FunctionNames[spendingFunctionUsed[1]],", alpha=",round(alpha[1],digits=5), sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(0,4))

        ##...then add lines between them
        lines(t,upperBounds,col="blue")
      }

      else
      {
        xCoordinate<-c(t,t)
        yCoordinate<-c(lowerBounds,upperBounds)

        if(BoundsSymmetry==2)
        {
          ## first plotting bounds as points...
          plot(xCoordinate,yCoordinate,main=paste("-1-  K=",K,
             "\n Function:", FunctionNames[spendingFunctionUsed[1]],", alpha=",round(alpha[1],digits=5), sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(-4,4))
        }
        else
        {
        ## first plotting bounds as points...
        plot(xCoordinate,yCoordinate,main=paste("-1-  K=",K,
             "\n upper Function:", FunctionNamesUpper[spendingFunctionUsed[1]],", alpha=",round(alpha[1],digits=5),
             "\n lower Function:", FunctionNamesLower[spendingFunctionUsed[2]],", alpha=",round(alpha[2],digits=5),sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(-4,4))
        }

        ##...then add lines between them
        lines(t,lowerBounds,col="blue")
        lines(t,upperBounds,col="blue")
      }
  }

  ##################################################################
  ## function handles click onto button to save results in a file ##
  ##################################################################
  onSave <- function()
  {
     #create file variable
     fileName <- tclvalue(tkgetSaveFile(initialfile=".html",filetypes="{{html Files} {.html}} {{All files} *}"))
     if (fileName=="") return()

     #open file
     zz <- file(fileName,"w")

     #output will be written in HTML
     cat("<html> <body> \n",file = zz)

     #output K
     cat("K=",K,"<br> \n",file = zz)

     ##ouput alpha
     if(!BoundsSymmetry==3)
     {
       cat("&alpha; =",alpha[1],"<br>\n",file = zz)
     }
     else
     {
       cat("Upper &alpha; = ",alpha[1],"<br>\n",file = zz)
       cat("Lower &alpha; = ",alpha[2],"<br>\n",file = zz)
     }

     if(BoundsSymmetry!=3)
     {
       ##output names of spending functions that were used
       FunctionNames <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: &alpha;&sdot;t<sup>",phi[1],"</sup>"),
                        paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")

       # substitute according funtion in output
       cat("<b>",FunctionNames[spendingFunctionUsed[1]],"</b>"," was used as spending Function.","<br>\n",file = zz)
     }
     else
     {
       ##names of spending functions that could have been used
       FunctionNamesUpper <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: &alpha;&sdot;t<sup>",phi[1],"</sup>"),
                        paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")
       FunctionNamesLower <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: &alpha;&sdot;t<sup>",phi[2],"</sup>"),
                        paste("Hwang-Shih-DeCani Family ( phi =",phi[2],")"),"Exact Pocock Bounds")

       # substitute according funtion in output
       cat("Spending Function for UPPER Bound:","<b>",FunctionNamesUpper[spendingFunctionUsed[1]],"</b>","<br>\n",file = zz)
       cat("Spending Function for LOWER Bound:","<b>",FunctionNamesLower[spendingFunctionUsed[2]],"</b>","<br>\n",file = zz)
     }

     ##output the bounds
     cat("<br>\n",file = zz)
     cat("<table border=\"3\"> \n",file = zz)
     cat("<tr> \n",file = zz)
     cat("<td>Times &#160</td>  <td>Lower Bounds &#160</td>  <td>Upper Bounds &#160</td> \n",file = zz)
     cat("<td>&alpha;[i]-&alpha;[i-1] &#160</td>  <td>cumulative &alpha; &#160</td> \n",file = zz)
     cat("</tr> \n",file = zz)

     for(i in 1:K)
     {
       cat("<tr> \n",file = zz)
       cat("<td>",round(t[i],digits=3),"</td>",   "<td>",round(lowerBounds[i],digits=4),"</td>",   "<td>",round(upperBounds[i],digits=4),"</td>",
           "<td>",round(probDifference[i],digits=10),"</td>",   "<td>",round(probExit[i],digits=10),"</td> \n",file = zz )
       cat("</tr> \n",file = zz)
     }

     cat("</table> \n",file = zz)
     cat("</body> </html> \n",file = zz)
     close(zz)
  }

  ###########################################################################
  ##function handles click onto button to Cancel i.e. close current window ##
  ###########################################################################
  onCancel <- function()
  {
   tkdestroy(outTask1Toplevel)
  }

  #frame for the buttons
  buttonFrame<-tkframe(OutputTask1,relief="groove",borderwidth=0)

  #button to show graphic
  showGraph.button <-tkbutton(buttonFrame,text="  Show Graph  ",command=onShowGraph)

  #button to save in file
  save.button <-tkbutton(buttonFrame,text="  Save to File  ",command=onSave)

  #button to cancel i.e. close current window
  cancel.button <-tkbutton(buttonFrame,text="  Cancel   ",command=onCancel)

  #grid buttons
  tkgrid( tklabel(buttonFrame, text=""))   #blank line
  tkgrid(showGraph.button,tklabel(buttonFrame, text="            "),
         save.button, tklabel(buttonFrame, text="            "),
         cancel.button, sticky="we")
  tkgrid(buttonFrame)
  tkgrid( tklabel(buttonFrame, text=""))   #blank line

  #grid allover frame and focus
  tkgrid(OutputTask1,sticky="w")
  tkfocus(outTask1Toplevel)

}#end <--*function(...)*


"guiOutputTask2" <-
function(K,probTotal,drift,expectedStoppingTime,secondTimeScaleIsUsed,t,t2,t2max,
         lowerBounds,upperBounds,probStopping,probExceedingUpper,probExceedingLower, confidenceLevel,
         BoundsSymmetry,enterBoundsManually,alpha,phi,spendingFunctionUsed, taskWindow)

{
  ###INITIALiZE VARIABLES###
  resultExitProb<-0 #exit probability
  cumulativeExitProb<-0 #cumulative exit probability
  FunctionNames=NULL;
  FunctionNamesUpper=NULL;
  FunctionNamesLower=NULL;

  ##compute exit probability and cumulative exit probability
  for(i in 1:K)
  {
    resultExitProb[i] <- probExceedingUpper[i]+probExceedingLower[i]

    if(i==1)
    {
      cumulativeExitProb[i] <- resultExitProb[i]
    }
    else
    {
      cumulativeExitProb[i] <- cumulativeExitProb[i-1] + resultExitProb[i]
    }
  }


  #Set Toplevel
  outTask2Toplevel <- tktoplevel(taskWindow)
  tkwm.title(outTask2Toplevel,paste("-2-   K =",K,", drift =",round(drift,digits=5),", Power=",confidenceLevel))

  #Define main Frame
  OutputTask2 <- tkframe(outTask2Toplevel)

  #Define subframes
  staticFrame <- tkframe(OutputTask2,relief="groove",borderwidth=0)
  dynamicFrame <- tkframe(OutputTask2,relief="groove",borderwidth=0)
  parametersFrame <- tkframe(staticFrame,relief="groove",borderwidth=0)
  numbersFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  timesFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  secondTimesFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  lowerBoundsFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  upperBoundsFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  exitProbabilityFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  cumulativeExitProbFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)


  #create label with parameter values:
  tkgrid( tklabel(parametersFrame, text=paste("K =",K)),sticky="w")
  if(!enterBoundsManually)
  {
    if(!BoundsSymmetry==3)
    {
      tkgrid( tklabel(parametersFrame, text=paste("alpha =",alpha[1])),sticky="w")
    }
    else
    {
      tkgrid( tklabel(parametersFrame, text=paste("alpha - Upper Bounds =",alpha[1])),sticky="w")
      tkgrid( tklabel(parametersFrame, text=paste("alpha - Lower Bounds =",alpha[2])),sticky="w")

    }
  }

  if(!BoundsSymmetry==3)
  {
    if(enterBoundsManually)
    {
      tkgrid( tklabel(parametersFrame, text="manually entered Bounds"),sticky="w")
    }
    else
    {
      ##names of spending functions that could have been used
      FunctionNames <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: alpha*t^",phi[1],sep=""),
                       paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")

      # substitute according funtion in output
      tkgrid( tklabel(parametersFrame, text=paste("Function: ",FunctionNames[spendingFunctionUsed[1]])),sticky="w")
    }
  }
  else
  {
    if(enterBoundsManually)
    {
      tkgrid( tklabel(parametersFrame, text="Upper Bounds: manually entered "),sticky="w")
      tkgrid( tklabel(parametersFrame, text="Lower Bounds: manually entered "),sticky="w")
    }
    else
    {
      ## names of spending functions that could have been used
      FunctionNamesUpper <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: alpha*t^",phi[1],sep=""),
                              paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")
      FunctionNamesLower <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: alpha*t^",phi[2],sep=""),
                              paste("Hwang-Shih-DeCani Family ( phi =",phi[2],")"),"Exact Pocock Bounds")

      # substitute according funtion in output
      tkgrid( tklabel(parametersFrame, text=paste("Function - Upper Bounds: ",FunctionNamesUpper[spendingFunctionUsed[1]])),sticky="w")
      tkgrid( tklabel(parametersFrame, text=paste("Function - Lower Bounds: ",FunctionNamesLower[spendingFunctionUsed[2]])),sticky="w")

    }
  }

  #drift parameters
  tkgrid( tklabel(parametersFrame, text=""),sticky="w") #blank line
  tkgrid( tklabel(parametersFrame, text=paste("drift=",round(drift,digits=5))),sticky="w")
  if(t2max!=0)
  {
    tkgrid( tklabel(parametersFrame, text=paste("Maximum Information =",t2max)),sticky="w")
  }
  tkgrid( tklabel(parametersFrame, text=paste("Power =",confidenceLevel)),sticky="w")
  tkgrid( tklabel(parametersFrame, text=""),sticky="w") #blank line


  #create head labels
  tkgrid( tklabel(numbersFrame, text="k    "),sticky="w")
  tkgrid( tklabel(timesFrame, text="Times   "),sticky="w")
  tkgrid( tklabel(secondTimesFrame, text="2nd Time Scale   "),sticky="w")
  tkgrid( tklabel(lowerBoundsFrame, text="Lower Bounds  "),sticky="w")
  tkgrid( tklabel(upperBoundsFrame, text="Upper Bounds  "),sticky="w")
  tkgrid( tklabel(exitProbabilityFrame, text="Exit Probability  "),sticky="w")
  tkgrid( tklabel(cumulativeExitProbFrame, text="Cumulative Exit Prob.  "),sticky="w")

  #create labels with results
  for(i in 1:K)
  {
    tkgrid( tklabel(numbersFrame, text=as.character(i)),sticky="w")
    tkgrid( tklabel(timesFrame, text=as.character(round(t[i],digits=3))),sticky="w")
    tkgrid( tklabel(secondTimesFrame, text=as.character(round(t2[i],digits=3))),sticky="w")
    tkgrid( tklabel(lowerBoundsFrame, text=as.character(round(lowerBounds[i],digits=4))),sticky="w")
    tkgrid( tklabel(upperBoundsFrame, text=as.character(round(upperBounds[i],digits=4))),sticky="w")
    tkgrid( tklabel(exitProbabilityFrame, text=as.character(round(resultExitProb[i],digits=10))),sticky="w")
    tkgrid( tklabel(cumulativeExitProbFrame, text=as.character(round(cumulativeExitProb[i],digits=10))),sticky="w")
  }

  #put frames together
  tkgrid(parametersFrame,sticky="w")
  ##if second information time is used output with information time
  if(secondTimeScaleIsUsed)
  {
    tkgrid(numbersFrame, timesFrame,secondTimesFrame, lowerBoundsFrame, upperBoundsFrame, exitProbabilityFrame, cumulativeExitProbFrame, sticky="w")
  }
  else
  {
    tkgrid(numbersFrame, timesFrame,                  lowerBoundsFrame, upperBoundsFrame, exitProbabilityFrame, cumulativeExitProbFrame, sticky="w")
  }
  tkgrid(staticFrame,sticky="w")
  tkgrid(dynamicFrame,sticky="w")


  ###########################################################################
 ## function handles click onto button to show results of bounds in a graph ##
  ###########################################################################
  onShowGraph <- function()
  {
    lines = graphics::lines

    if(enterBoundsManually)
    {
      ## if one-Sided-Test we won't see negative Z-Values
      if(BoundsSymmetry==1)
      {
        xCoordinate<-t
        yCoordinate<-upperBounds

        ## first plotting bounds as points...
        plot(xCoordinate,yCoordinate,main=paste("-2-  K=",K,", drift=",round(drift,digits=5),
             "\n Bounds manually entered", sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(0,4))

        ##...then add lines between them
        lines(t,upperBounds,col="blue")
      }

      else
      {
        xCoordinate<-c(t,t)
        yCoordinate<-c(lowerBounds,upperBounds)

        ## first plotting bounds as points...
        plot(xCoordinate,yCoordinate,main=paste("-2-  K=",K,", drift=",round(drift,digits=5),
             "\n Bounds manually entered", sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(-4,4))

        ##...then add lines between them
        lines(t,lowerBounds,col="blue")
        lines(t,upperBounds,col="blue")
      }
    }# endif 'if(enterBoundsManually)'

    else # spending function was used
    {
      ## if one-Sided-Test we won't see negative Z-Values
      if(BoundsSymmetry==1)
      {
        xCoordinate<-t
        yCoordinate<-upperBounds

        ## first plotting bounds as points...
        plot(xCoordinate,yCoordinate,main=paste("-2-  K=",K,", drift=",round(drift,digits=5),
             "\n Function:", FunctionNames[spendingFunctionUsed[1]],", alpha=",round(alpha[1],digits=5), sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(0,4))

        ##...then add lines between them
        lines(t,upperBounds,col="blue")
      }

      else
      {
        xCoordinate<-c(t,t)
        yCoordinate<-c(lowerBounds,upperBounds)

        if(BoundsSymmetry==2)
        {
           ## first plotting bounds as points...
           plot(xCoordinate,yCoordinate,main=paste("-2-  K=",K,", drift=",round(drift,digits=5),
           "\n Function:", FunctionNames[spendingFunctionUsed[1]],", alpha=",round(alpha[1],digits=5), sep=""),
           pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
           xlab="Times",ylab="Standardized Z-Value",ylim=c(-4,4))
         }
         else
         {
           ## first plotting bounds as points...
           plot(xCoordinate,yCoordinate,main=paste("-2-  K=",K,", drift=",round(drift,digits=5),
             "\n upper Function:", FunctionNamesUpper[spendingFunctionUsed[1]],", alpha=",round(alpha[1],digits=5),
             "\n lower Function:", FunctionNamesLower[spendingFunctionUsed[2]],", alpha=",round(alpha[2],digits=5),sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(-4,4))
         }

         ##...then add lines between them
         lines(t,lowerBounds,col="blue")
         lines(t,upperBounds,col="blue")

      }
    }
  }

  ##################################################################
  ## function handles click onto button to save results in a file ##
  ##################################################################
  onSave <- function()
  {
     #create file variable
     fileName <- tclvalue(tkgetSaveFile(initialfile=".html",filetypes="{{html Files} {.html}} {{All files} *}"))
     if (fileName=="") return()

     #open file
     zz <- file(fileName,"w")

     #output will be writed in HTML
     cat("<html> <body> \n",file = zz)

     #output K
     cat("K=",K,"<br> \n",file = zz)

     ##ouput alpha
     if(!enterBoundsManually)
     {
       if(!BoundsSymmetry==3)
       {
         cat("&alpha; =",alpha[1],"<br>\n",file = zz)
       }
       else
       {
         cat("Upper &alpha; = ",alpha[1],"<br>\n",file = zz)
         cat("Lower &alpha; = ",alpha[2],"<br>\n",file = zz)
       }
     }

     if(!BoundsSymmetry==3)
     {
       if(enterBoundsManually)
       {
         cat("<b>manually entered</b> Bounds <br>\n",file = zz)
       }
       else
       {
         ##output names of spending functions that were used
         FunctionNames <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: &alpha;&sdot;t<sup>",phi[1],"</sup>"),
                            paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")

         # substitute according funtion in output
         cat("<b>",FunctionNames[spendingFunctionUsed[1]],"</b>"," was used as spending Function.","<br>\n",file = zz)
       }
     }
     else
     {
       if(enterBoundsManually)
       {
         cat("UPPER Bounds: <b>manually entered</b> <br>\n",file = zz)
         cat("LOWER Bounds: <b>manually entered</b> <br>\n",file = zz)
       }
       else
       {
         ##names of spending functions that could have been used
         FunctionNamesUpper <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: &alpha;&sdot;t<sup>",phi[1],"</sup>"),
                           paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")
         FunctionNamesLower <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: &alpha;&sdot;t<sup>",phi[2],"</sup>"),
                           paste("Hwang-Shih-DeCani Family ( phi =",phi[2],")"),"Exact Pocock Bounds")

         # substitute according funtion in output
         cat("Spending Function for UPPER Bound:","<b>",FunctionNamesUpper[spendingFunctionUsed[1]],"</b>","<br>\n",file = zz)
         cat("Spending Function for LOWER Bound:","<b>",FunctionNamesLower[spendingFunctionUsed[2]],"</b>","<br>\n",file = zz)
       }
     }

     #drift parameters
     cat("drift=",round(drift,digits=5),"<br> \n",file=zz)
     if(t2max!=0)
     {
       cat("Maximum Information=",t2max,"<br> \n",file=zz)
     }
     cat("Power=",confidenceLevel,"<br><br> \n",file=zz)


     ##output the bounds
     #labels
     cat("<br>\n",file = zz)
     cat("<table border=\"3\"> \n",file = zz)
     cat("<tr> \n",file = zz)
     cat("<td>Times &#160</td> \n" ,file=zz)
     if(secondTimeScaleIsUsed) { cat("<td>2nd Time Scale &#160</td> \n" ,file=zz) }
     cat("<td>Lower Bounds &#160</td>  <td>Upper Bounds &#160</td> \n",file = zz)
     cat("<td>Exit Probability &#160</td>  <td>Cumulative Exit Prob. &#160</td> \n",file = zz)
     cat("</tr> \n",file = zz)

     #values
     for(i in 1:K)
     {
       cat("<tr> \n",file = zz)
       cat("<td>",round(t[i],digits=3),"</td> \n",file = zz )
       if(secondTimeScaleIsUsed) { cat("<td>",round(t2[i],digits=3),"</td> \n",file = zz ) }
       cat("<td>",round(lowerBounds[i],digits=4),"</td>",   "<td>",round(upperBounds[i],digits=4),"</td>",
           "<td>",round(resultExitProb[i],digits=10),"</td>",   "<td>",round(cumulativeExitProb[i],digits=10),"</td> \n",file = zz )
       cat("</tr> \n",file = zz)
     }

     cat("</table> \n",file = zz)
     cat("</body> </html> \n",file = zz)
     close(zz)
  }

  ###########################################################################
  ##function handles click onto button to Cancel i.e. close current window ##
  ###########################################################################
  onCancel <- function()
  {
   tkdestroy(outTask2Toplevel)
  }

  #frame for the buttons
  buttonFrame<-tkframe(OutputTask2,relief="groove",borderwidth=0)

  #button to show graphic
  showGraph.button <-tkbutton(buttonFrame,text="  Show Graph  ",command=onShowGraph)

  #button to save in file
  save.button <-tkbutton(buttonFrame,text="  Save to File  ",command=onSave)

  #button to cancel i.e. close current window
  cancel.button <-tkbutton(buttonFrame,text="  Cancel   ",command=onCancel)

  #grid buttons
  tkgrid( tklabel(buttonFrame, text=""))   #blank line
  tkgrid(showGraph.button,tklabel(buttonFrame, text="            "),
         save.button, tklabel(buttonFrame, text="            "),
         cancel.button, sticky="we")
  tkgrid(buttonFrame)
  tkgrid( tklabel(buttonFrame, text=""))   #blank line

  #grid allover frame and focus
  tkgrid(OutputTask2,sticky="w")
  tkfocus(outTask2Toplevel)


}#end <--*function(...)*


"guiOutputTask3" <-
function(K,probTotal,drift,expectedStoppingTime,secondTimeScaleIsUsed,t,t2,t2max,
         lowerBounds,upperBounds,probStopping,probExceedingUpper,probExceedingLower,
         BoundsSymmetry,enterBoundsManually,alpha,phi,spendingFunctionUsed, taskWindow)
{
  ###INITIALiZE VARIABLES###
  cumulativeExitProb<-0 #cumulative exit probability
  resultExitProb<-0 #exit probability
  FunctionNames=NULL;
  FunctionNamesUpper=NULL;
  FunctionNamesLower=NULL;

  ##compute exit probability and cumulative exit probability
  for(i in 1:K)
  {
    resultExitProb[i] <- probExceedingUpper[i]+probExceedingLower[i]

    if(i==1)
    {
      cumulativeExitProb[i] <- resultExitProb[i]
    }
    else
    {
      cumulativeExitProb[i] <- cumulativeExitProb[i-1] + resultExitProb[i]
    }
  }


  #Set Toplevel
  outTask3Toplevel <- tktoplevel(taskWindow)
  tkwm.title(outTask3Toplevel,paste("-3-  K=",K,", drift parameter=",round(drift,digits=5)))

  #Define main Frame
  OutputTask3 <- tkframe(outTask3Toplevel)

  #Define subframes
  staticFrame <- tkframe(OutputTask3,relief="groove",borderwidth=0)
  dynamicFrame <- tkframe(OutputTask3,relief="groove",borderwidth=0)
  parametersFrame <- tkframe(staticFrame,relief="groove",borderwidth=0)
  numbersFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  timesFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  secondTimesFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  lowerBoundsFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  upperBoundsFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  exitProbabilityFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  cumulativeExitProbFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)

  #create label with parameter values:
  tkgrid( tklabel(parametersFrame, text=paste("K =",K)),sticky="w")

  if(!enterBoundsManually)
  {
    if(!BoundsSymmetry==3)
    {
      tkgrid( tklabel(parametersFrame, text=paste("alpha =",alpha[1])),sticky="w")
    }
    else
    {
      tkgrid( tklabel(parametersFrame, text=paste("alpha - Upper Bounds =",alpha[1])),sticky="w")
      tkgrid( tklabel(parametersFrame, text=paste("alpha - Lower Bounds =",alpha[2])),sticky="w")

    }
  }

  if(!BoundsSymmetry==3)
  {
    if(enterBoundsManually)
    {
      tkgrid( tklabel(parametersFrame, text="manually entered Bounds"),sticky="w")
    }
    else
    {
      ##names of spending functions that could have been used
      FunctionNames <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: alpha*t^",phi[1],sep=""),
                         paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")

      # substitute according funtion in output
      tkgrid( tklabel(parametersFrame, text=paste("Function: ",FunctionNames[[spendingFunctionUsed[1]]])),sticky="w")
    }
  }
  else
  {
    if(enterBoundsManually)
    {
      tkgrid( tklabel(parametersFrame, text="Upper Bounds: manually entered "),sticky="w")
      tkgrid( tklabel(parametersFrame, text="Lower Bounds: manually entered "),sticky="w")
    }
    else
    {
      ##names of spending functions that could have been used
      FunctionNamesUpper <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: alpha*t^",phi[1],sep=""),
                              paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")
      FunctionNamesLower <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: alpha*t^",phi[2],sep=""),
                              paste("Hwang-Shih-DeCani Family ( phi =",phi[2],")"),"Exact Pocock Bounds")

      # substitute according funtion in output
      tkgrid( tklabel(parametersFrame, text=paste("Function - Upper Bounds: ",FunctionNamesUpper[spendingFunctionUsed[1]])),sticky="w")
      tkgrid( tklabel(parametersFrame, text=paste("Function - Lower Bounds: ",FunctionNamesLower[spendingFunctionUsed[2]])),sticky="w")
    }
  }


  #drift parameters
  tkgrid( tklabel(parametersFrame, text=""),sticky="w") #blank line
  tkgrid( tklabel(parametersFrame, text=paste("Drift =",round(drift,digits=5))),sticky="w")
  tkgrid( tklabel(parametersFrame, text="Drift is equal to the expectation of the Z statistic when time=1."),sticky="w")
  if(t2max!=0)
  {
    tkgrid( tklabel(parametersFrame, text=paste("Maximum Information =",t2max)),sticky="w")
  }

  tkgrid( tklabel(parametersFrame, text=""),sticky="w") #blank line


  #create head labels
  tkgrid( tklabel(numbersFrame, text="k    "),sticky="w")
  tkgrid( tklabel(timesFrame, text="Times   "),sticky="w")
  tkgrid( tklabel(secondTimesFrame, text="2nd Time Scale   "),sticky="w")
  tkgrid( tklabel(lowerBoundsFrame, text="Lower Bounds  "),sticky="w")
  tkgrid( tklabel(upperBoundsFrame, text="Upper Bounds  "),sticky="w")
  tkgrid( tklabel(exitProbabilityFrame, text="Exit Probability  "),sticky="w")
  tkgrid( tklabel(cumulativeExitProbFrame, text="Cumulative Exit Prob.  "),sticky="w")

  #create labels with results
  for(i in 1:K)
  {
    tkgrid( tklabel(numbersFrame, text=as.character(i)),sticky="w")
    tkgrid( tklabel(timesFrame, text=as.character(round(t[i],digits=3))),sticky="w")
    tkgrid( tklabel(secondTimesFrame, text=as.character(round(t2[i],digits=3))),sticky="w")
    tkgrid( tklabel(lowerBoundsFrame, text=as.character(round(lowerBounds[i],digits=4))),sticky="w")
    tkgrid( tklabel(upperBoundsFrame, text=as.character(round(upperBounds[i],digits=4))),sticky="w")
    tkgrid( tklabel(exitProbabilityFrame, text=as.character(round(resultExitProb[i],digits=10))),sticky="w")
    tkgrid( tklabel(cumulativeExitProbFrame, text=as.character(round(cumulativeExitProb[i],digits=10))),sticky="w")
  }

  #put frames together
  tkgrid(parametersFrame,sticky="w")
  ##if second information time is used output with information time
  if(secondTimeScaleIsUsed)
  {
    tkgrid(numbersFrame, timesFrame,secondTimesFrame, lowerBoundsFrame, upperBoundsFrame, exitProbabilityFrame, cumulativeExitProbFrame, sticky="w")
  }
  else
  {
    tkgrid(numbersFrame, timesFrame,                  lowerBoundsFrame, upperBoundsFrame, exitProbabilityFrame, cumulativeExitProbFrame, sticky="w")
  }
  tkgrid(staticFrame,sticky="w")
  tkgrid(dynamicFrame,sticky="w")


  ###########################################################################
  ##function handles click onto button to show results of bounds in a graph##
  ###########################################################################
  onShowGraph <- function()
  {
    lines = graphics::lines

    if(enterBoundsManually)
    {
      ## if one-Sided-Test we won't see negative Z-Values
      if(BoundsSymmetry==1)
      {
        xCoordinate<-t
        yCoordinate<-upperBounds

        ## first plotting bounds as points...
        plot(xCoordinate,yCoordinate,main=paste("-3-  K=",K,", drift=",round(drift,digits=5),
             "\n Bounds manually entered", sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(0,4))

        ##...then add lines between them
        lines(t,upperBounds,col="blue")
      }

      else
      {
        xCoordinate<-c(t,t)
        yCoordinate<-c(lowerBounds,upperBounds)

        ## first plotting bounds as points...
        plot(xCoordinate,yCoordinate,main=paste("-3-  K=",K,", drift=",round(drift,digits=5),
             "\n Bounds manually entered", sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(-4,4))

        ##...then add lines between them
        lines(t,lowerBounds,col="blue")
        lines(t,upperBounds,col="blue")
      }
    }# endif 'if(enterBoundsManually)'

    else # spending function was used
    {
    ## if one-Sided-Test we won't see negative Z-Values
      if(BoundsSymmetry==1)
      {
        xCoordinate<-t
        yCoordinate<-upperBounds

        ## first plotting bounds as points...
        plot(xCoordinate,yCoordinate,main=paste("-3-  K=",K,", drift=",round(drift,digits=5),
             "\n Function:", FunctionNames[spendingFunctionUsed[1]],", alpha=",round(alpha[1],digits=5), sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(0,4))

        ##...then add lines between them
        lines(t,upperBounds,col="blue")
      }

      else
      {
        xCoordinate<-c(t,t)
        yCoordinate<-c(lowerBounds,upperBounds)

        if(BoundsSymmetry==2)
        {
          ## first plotting bounds as points...
          plot(xCoordinate,yCoordinate,main=paste("-3-  K=",K,", drift=",round(drift,digits=5),
             "\n Function:", FunctionNames[spendingFunctionUsed[1]],", alpha=",round(alpha[1],digits=5), sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(-4,4))
        }
        else
        {
          ## first plotting bounds as points...
          plot(xCoordinate,yCoordinate,main=paste("-3-  K=",K,", drift=",round(drift,digits=5),
             "\n upper Function:", FunctionNamesUpper[spendingFunctionUsed[1]],", alpha=",round(alpha[1],digits=5),
             "\n lower Function:", FunctionNamesLower[spendingFunctionUsed[2]],", alpha=",round(alpha[2],digits=5),sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(-4,4))
        }

        ##...then add lines between them
        lines(t,lowerBounds,col="blue")
        lines(t,upperBounds,col="blue")
      }
    }
  }

##################################################################
  ## function handles click onto button to save results in a file ##
  ##################################################################
  onSave <- function()
  {
     #create file variable
     fileName <- tclvalue(tkgetSaveFile(initialfile=".html",filetypes="{{html Files} {.html}} {{All files} *}"))
     if (fileName=="") return()

     #open file
     zz <- file(fileName,"w")

     #output will be writed in HTML
     cat("<html> <body> \n",file = zz)

     #output K
     cat("K=",K,"<br> \n",file = zz)

     ##ouput alpha
     if(!enterBoundsManually)
     {
       if(!BoundsSymmetry==3)
       {
         cat("&alpha; =",alpha[1],"<br>\n",file = zz)
       }
       else
       {
         cat("Upper &alpha; = ",alpha[1],"<br>\n",file = zz)
         cat("Lower &alpha; = ",alpha[2],"<br>\n",file = zz)
       }
     }
     if(!BoundsSymmetry==3)
     {
       if(enterBoundsManually)
       {
         cat("<b>manually entered</b> Bounds <br>\n",file = zz)
       }
       else
       {
         ##output names of spending functions that were used
         FunctionNames <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: &alpha;&sdot;t<sup>",phi[1],"</sup>"),
                          paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")

         # substitute according funtion in output
         cat("<b>",FunctionNames[spendingFunctionUsed[1]],"</b>"," was used as spending Function.","<br>\n",file = zz)
       }
     }
     else
     {
       if(enterBoundsManually)
       {
         cat("UPPER Bounds: <b>manually entered</b> <br>\n",file = zz)
         cat("LOWER Bounds: <b>manually entered</b> <br>\n",file = zz)
       }
       else
       {
         ##names of spending functions that could have been used
         FunctionNamesUpper <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: &alpha;&sdot;t<sup>",phi[1],"</sup>"),
                          paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")
         FunctionNamesLower <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: &alpha;&sdot;t<sup>",phi[2],"</sup>"),
                          paste("Hwang-Shih-DeCani Family ( phi =",phi[2],")"),"Exact Pocock Bounds")

         # substitute according funtion in output
         cat("Spending Function for UPPER Bound:","<b>",FunctionNamesUpper[spendingFunctionUsed[1]],"</b>","<br>\n",file = zz)
         cat("Spending Function for LOWER Bound:","<b>",FunctionNamesLower[spendingFunctionUsed[2]],"</b>","<br>\n",file = zz)
       }
     }

     #drift parameters
     cat("drift=",round(drift,digits=5),"<br> \n",file=zz)
     cat("Drift is equal to the expectation of the Z statistic when time=1.<br> \n",file=zz)
     if(t2max!=0)
     {
       cat("Maximum Information=",t2max,"<br> \n",file=zz)
     }


     ##output the bounds
     #labels
     cat("<br>\n",file = zz)
     cat("<table border=\"3\"> \n",file = zz)
     cat("<tr> \n",file = zz)
     cat("<td>Times &#160</td> \n" ,file=zz)
     if(secondTimeScaleIsUsed) { cat("<td>2nd Time Scale &#160</td> \n" ,file=zz) }
     cat("<td>Lower Bounds &#160</td>  <td>Upper Bounds &#160</td> \n",file = zz)
     cat("<td>Exit Probability &#160</td>  <td>Cumulative Exit Prob. &#160</td> \n",file = zz)
     cat("</tr> \n",file = zz)

     #values
     for(i in 1:K)
     {
       cat("<tr> \n",file = zz)
       cat("<td>",round(t[i],digits=3),"</td> \n",file = zz )
       if(secondTimeScaleIsUsed) { cat("<td>",round(t2[i],digits=3),"</td> \n",file = zz ) }
       cat("<td>",round(lowerBounds[i],digits=4),"</td>",   "<td>",round(upperBounds[i],digits=4),"</td>",
           "<td>",round(resultExitProb[i],digits=10),"</td>",   "<td>",round(cumulativeExitProb[i],digits=10),"</td> \n",file = zz )
       cat("</tr> \n",file = zz)
     }

     cat("</table> \n",file = zz)
     cat("</body> </html> \n",file = zz)
     close(zz)
  }


  ###########################################################################
  ##function handles click onto button to Cancel i.e. close current window ##
  ###########################################################################
  onCancel <- function()
  {
   tkdestroy(outTask3Toplevel)
  }

  #frame for the buttons
  buttonFrame<-tkframe(OutputTask3,relief="groove",borderwidth=0)

  #button to show graphic
  showGraph.button <-tkbutton(buttonFrame,text="  Show Graph  ",command=onShowGraph)

  #button to save in file
  save.button <-tkbutton(buttonFrame,text="  Save to File  ",command=onSave)

  #button to cancel i.e. close current window
  cancel.button <-tkbutton(buttonFrame,text="  Cancel   ",command=onCancel)

  #grid buttons
  tkgrid( tklabel(buttonFrame, text=""))   #blank line
  tkgrid(showGraph.button,tklabel(buttonFrame, text="            "),
         save.button, tklabel(buttonFrame, text="            "),
         cancel.button, sticky="we")
  tkgrid(buttonFrame)
  tkgrid( tklabel(buttonFrame, text=""))   #blank line

  #grid allover frame and focus
  tkgrid(OutputTask3,sticky="w")
  tkfocus(outTask3Toplevel)



}#end <--*function(...)*


"guiOutputTask4" <-
function(K,confidenceLevel,secondTimeScaleIsUsed,t,t2,t2max,lowerBounds,upperBounds,BoundsSymmetry,
         enterBoundsManually,alpha,phi,confidenceIntervall,spendingFunctionUsed,Zvalue, taskWindow)
{
  # Initializing
  FunctionNames=NULL;
  FunctionNamesUpper=NULL;
  FunctionNamesLower=NULL;

  #Set Toplevel
  outTask4Toplevel <- tktoplevel(taskWindow)
  tkwm.title(outTask4Toplevel,paste("-4-    K =",K,", Z-value =",Zvalue))

  #Define main Frame
  OutputTask4 <- tkframe(outTask4Toplevel)

  #Define subframes
  parametersFrame <- tkframe(OutputTask4,relief="groove",borderwidth=0)

  #create labels with parameter values:
  tkgrid( tklabel(parametersFrame, text=paste("K=",K)),sticky="w")

  if(!enterBoundsManually)
  {
    if(!BoundsSymmetry==3)
    {
      tkgrid( tklabel(parametersFrame, text=paste("alpha =",alpha[1])),sticky="w")
    }
    else
    {
      tkgrid( tklabel(parametersFrame, text=paste("alpha - Upper Bounds =",alpha[1])),sticky="w")
      tkgrid( tklabel(parametersFrame, text=paste("alpha - Lower Bounds =",alpha[2])),sticky="w")

    }
  }

  if(!BoundsSymmetry==3)
  {
    if(enterBoundsManually)
    {
      tkgrid( tklabel(parametersFrame, text="manually entered Bounds"),sticky="w")
    }
    else
    {
      ##names of spending functions that could have been used
      FunctionNames <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: alpha*t^",phi[1],sep=""),
                       paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")

      # substitute according funtion in output
      tkgrid( tklabel(parametersFrame, text=paste("Function: ",FunctionNames[[spendingFunctionUsed[1]]])),sticky="w")
    }
  }
  else
  {
    if(enterBoundsManually)
    {
      tkgrid( tklabel(parametersFrame, text="Upper Bounds: manually entered "),sticky="w")
      tkgrid( tklabel(parametersFrame, text="Lower Bounds: manually entered "),sticky="w")
    }
    else
    {
      ##names of spending functions that could have been used
      FunctionNamesUpper <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: alpha*t^",phi[1],sep=""),
                            paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")
      FunctionNamesLower <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: alpha*t^",phi[2],sep=""),
                            paste("Hwang-Shih-DeCani Family ( phi =",phi[2],")"),"Exact Pocock Bounds")

      # substitute according funtion in output
      tkgrid( tklabel(parametersFrame, text=paste("Function - Upper Bounds: ",FunctionNamesUpper[spendingFunctionUsed[1]])),sticky="w")
      tkgrid( tklabel(parametersFrame, text=paste("Function - Lower Bounds: ",FunctionNamesLower[spendingFunctionUsed[2]])),sticky="w")
    }
  }

  #confidence- level and intervall
  tkgrid( tklabel(parametersFrame, text=""),sticky="w") #blank line
  tkgrid( tklabel(parametersFrame, text=paste("Z-value at last analysis= ",Zvalue)),sticky="w")
  tkgrid( tklabel(parametersFrame, text=paste("Confidence Level= ",confidenceLevel,"%")),sticky="w")
  tkgrid( tklabel(parametersFrame, text=""),sticky="w") #blank line
  tkgrid( tklabel(parametersFrame, text=paste("Confidence Intervall= <",round(confidenceIntervall[1],digits=5)," , ",
                                                                      round(confidenceIntervall[2],digits=5),">")),sticky="w")
  tkgrid( tklabel(parametersFrame, text="Drift is equal to the expectation of the Z statistic when time=1."),sticky="w")
  if(t2max!=0)
  {
    tkgrid( tklabel(parametersFrame, text=paste("Maximum Information=",t2max)),sticky="w")
  }

  tkgrid( tklabel(parametersFrame, text=""),sticky="w") #blank line
  tkgrid(parametersFrame,sticky="w")


  ###########################################################################
  # function handles click onto button to show results of bounds in a graph #
  ###########################################################################
  onShowGraph <- function()
  {
    lines = graphics::lines
    if(enterBoundsManually)
    {
      ## if one-Sided-Test we won't see negative Z-Values
      if(BoundsSymmetry==1)
      {
        xCoordinate<-t
        yCoordinate<-upperBounds

        ## first plotting bounds as points...
        plot(xCoordinate,yCoordinate,main=paste("-3-  K=",K,", Z=",Zvalue,
             ", confidence level = ",round(confidenceLevel,digits=5), ", confidence intervall= < ",
             round(confidenceIntervall[1],digits=5),",",round(confidenceIntervall[2],digits=5)," >",
             "\n Bounds manually entered", sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(0,4))

        ##...then add lines between them
        lines(t,upperBounds,col="blue")
      }

      else
      {
        xCoordinate<-c(t,t)
        yCoordinate<-c(lowerBounds,upperBounds)

        ## first plotting bounds as points...
        plot(xCoordinate,yCoordinate,main=paste("-3-  K=",K,", Z=",Zvalue,
             ", confidence level = ",round(confidenceLevel,digits=5), ", confidence intervall= < ",
             round(confidenceIntervall[1],digits=5),",",round(confidenceIntervall[2],digits=5)," >",
             "\n Bounds manually entered", sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(-4,4))

        ##...then add lines between them
        lines(t,lowerBounds,col="blue")
        lines(t,upperBounds,col="blue")
      }
    }# endif 'if(enterBoundsManually)'

    else # spending function was used
    {
    ## if one-Sided-Test we won't see negative Z-Values
      if(BoundsSymmetry==1)
      {
        xCoordinate<-t
        yCoordinate<-upperBounds

        ## first plotting bounds as points...
        plot(xCoordinate,yCoordinate,main=paste("-3-  K=",K,", Z=",Zvalue,
             ", confidence level = ",round(confidenceLevel,digits=5), ", confidence intervall= < ",
             round(confidenceIntervall[1],digits=5),",",round(confidenceIntervall[2],digits=5)," >",
             "\n Function:", FunctionNames[spendingFunctionUsed[1]],", alpha=",round(alpha[1],digits=5), sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(0,4))

        ##...then add lines between them
        lines(t,upperBounds,col="blue")
      }

      else
      {
        xCoordinate<-c(t,t)
        yCoordinate<-c(lowerBounds,upperBounds)

        if(BoundsSymmetry==2)
        {
          ## first plotting bounds as points...
         plot(xCoordinate,yCoordinate,main=paste("-3-  K=",K,", Z=",Zvalue,
             ", confidence level = ",round(confidenceLevel,digits=5), ", confidence intervall= < ",
             round(confidenceIntervall[1],digits=5),",",round(confidenceIntervall[2],digits=5)," >",
             "\n Function:", FunctionNames[spendingFunctionUsed[1]],", alpha=",round(alpha[1],digits=5), sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(-4,4))
        }
        else
        {
         ## first plotting bounds as points...
         plot(xCoordinate,yCoordinate,main=paste("-3-  K=",K,", Z=",Zvalue,
             ", confidence level = ",round(confidenceLevel,digits=5), ", confidence intervall= < ",
             round(confidenceIntervall[1],digits=5),",",round(confidenceIntervall[2],digits=5)," >",
             "\n upper Function:", FunctionNamesUpper[spendingFunctionUsed[1]],", alpha=",round(alpha[1],digits=5),
             "\n lower Function:", FunctionNamesLower[spendingFunctionUsed[2]],", alpha=",round(alpha[2],digits=5),sep=""),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4, cex.main=0.9,
             xlab="Times",ylab="Standardized Z-Value",ylim=c(-4,4))
        }

        ##...then add lines between them
        lines(t,lowerBounds,col="blue")
        lines(t,upperBounds,col="blue")
      }
    }
  }

  ##################################################################
  ## function handles click onto button to save results in a file ##
  ##################################################################
  onSave <- function()
  {
     #create file variable
     fileName <- tclvalue(tkgetSaveFile(initialfile=".html",filetypes="{{html Files} {.html}} {{All files} *}"))
     if (fileName=="") return()

     #open file
     zz <- file(fileName,"w")

     #output will be writed in HTML
     cat("<html> <body> \n",file = zz)

     #output K
     cat("K=",K,"<br> \n",file = zz)

     ##ouput alpha
     if(!enterBoundsManually)
     {
       if(!BoundsSymmetry==3)
       {
         cat("&alpha; =",alpha[1],"<br>\n",file = zz)
       }
       else
       {
         cat("Upper &alpha; = ",alpha[1],"<br>\n",file = zz)
         cat("Lower &alpha; = ",alpha[2],"<br>\n",file = zz)
       }
     }
     if(!BoundsSymmetry==3)
     {
       if(enterBoundsManually)
       {
         cat("<b>manually entered</b> Bounds <br>\n",file = zz)
       }
       else
       {
         ##output names of spending functions that were used
         FunctionNames <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: &alpha;&sdot;t<sup>",phi[1],"</sup>"),
                          paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")

         # substitute according funtion in output
         cat("<b>",FunctionNames[spendingFunctionUsed[1]],"</b>"," was used as spending Function.","<br>\n",file = zz)
       }
     }
     else
     {
       if(enterBoundsManually)
       {
         cat("UPPER Bounds: <b>manually entered</b> <br>\n",file = zz)
         cat("LOWER Bounds: <b>manually entered</b> <br>\n",file = zz)
       }
       else
       {
         ##names of spending functions that could have been used
         FunctionNamesUpper <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: &alpha;&sdot;t<sup>",phi[1],"</sup>"),
                          paste("Hwang-Shih-DeCani Family ( phi =",phi[1],")"),"Exact Pocock Bounds")
         FunctionNamesLower <- c("O'Brien-Fleming Type","Pocock Type",paste("Power Family: &alpha;&sdot;t<sup>",phi[2],"</sup>"),
                          paste("Hwang-Shih-DeCani Family ( phi =",phi[2],")"),"Exact Pocock Bounds")

         # substitute according funtion in output
         cat("Spending Function for UPPER Bound:","<b>",FunctionNamesUpper[spendingFunctionUsed[1]],"</b>","<br>\n",file = zz)
         cat("Spending Function for LOWER Bound:","<b>",FunctionNamesLower[spendingFunctionUsed[2]],"</b>","<br>\n",file = zz)
       }
     }

     #confidence- level and intervall
     cat("<br>Confidence Level= ",confidenceLevel,"% <br>\n",file = zz)
     cat("Confidence Intervall= <",round(confidenceIntervall[1],digits=5)," , ",
                                   round(confidenceIntervall[2],digits=5),"> <br>\n",file = zz)
     cat("Drift is equal to the expectation of the Z statistic when time=1.<br> \n",file=zz)
     if(t2max!=0)
     {
       cat("Maximum Information=",t2max,"<br> \n",file=zz)
     }

     cat("</body> </html> \n",file = zz)
     close(zz)
  }


  ###########################################################################
  ##function handles click onto button to Cancel i.e. close current window ##
  ###########################################################################
  onCancel <- function()
  {
   tkdestroy(outTask4Toplevel)
  }

  #frame for the buttons
  buttonFrame<-tkframe(OutputTask4,relief="groove",borderwidth=0)

  #button to show graphic
  showGraph.button <-tkbutton(buttonFrame,text="  Show Graph  ",command=onShowGraph)

  #button to save in file
  save.button <-tkbutton(buttonFrame,text="  Save to File  ",command=onSave)

  #button to cancel i.e. close current window
  cancel.button <-tkbutton(buttonFrame,text="  Cancel   ",command=onCancel)

  #grid buttons
  tkgrid( tklabel(buttonFrame, text=""))   #blank line
  tkgrid(showGraph.button,tklabel(buttonFrame, text="      "),
         save.button, tklabel(buttonFrame,     text="      "),
         cancel.button, sticky="we")
  tkgrid(buttonFrame)
  tkgrid( tklabel(buttonFrame, text=""))   #blank line

  #grid allover frame and focus
  tkgrid(OutputTask4,sticky="w")
  tkfocus(outTask4Toplevel)



}#end <--*function(...)*


"integrateByTrapez" <-
function(functionValues, numberOfIntegrationIntervalls, gridSize)
{
  ###Initialize variables###
  sumUp<-0
  area<-0


  ##sum up all function values
  ##All but the first and last function values appear twice in the summation
  sumUp <- sumUp + functionValues[1]
  sumUp <- 2*sum(functionValues[2:numberOfIntegrationIntervalls])
  sumUp <- sumUp + functionValues[numberOfIntegrationIntervalls+1]

  ##Multiply the sum by gridSize/2
  area <- (gridSize/2)*sumUp

  return(area)

}#end <--*function(...)*



"jointDensity" <-
function (i, lowerIntegrationLimit, upperIntegrationLimit,standardDeviation, numberOfIntegrationIntervalls, lastGrid)
{
  ##first analysis
  if (i==1)
  {
    ###Initialize variables###
    currentGridSize<-0


    ##Compute grid size to be used
    currentGridSize <- (upperIntegrationLimit[1]-lowerIntegrationLimit[1])/numberOfIntegrationIntervalls[1]


    ##Evaluate function (normal density) at grid points
    ##lastGrid <- seq(lowerIntegrationLimit,upperIntegrationLimit,by=currentGridSize)
    lastGrid <- seq(lowerIntegrationLimit,upperIntegrationLimit,length=numberOfIntegrationIntervalls[1]+1)
    lastGrid <- stats::dnorm( lastGrid/standardDeviation ) / standardDeviation

  }

  ##second analysis and later
  else
  {
    ###Initialize variables###
    currentGridSize<-0 # 'currentGridSize' is the grid size used.
    grid<-0 # 'grid' is the argument for function evaluation.
    lastCopy<-0 # 'lastCopy' is a temporary vector of function values because 'lastGrid' from
        # previous step itself is needed to compute each current value.

    ##Current grid size to be used
    currentGridSize <- (upperIntegrationLimit[i]-lowerIntegrationLimit[i])/numberOfIntegrationIntervalls[i]

    ##Previous grid size.
    previousGridSize <- (upperIntegrationLimit[i-1]-lowerIntegrationLimit[i-1]) / numberOfIntegrationIntervalls[i-1]

    lastlast <- seq(lowerIntegrationLimit[i-1],upperIntegrationLimit[i-1],length=numberOfIntegrationIntervalls[i-1]+1)


    ##Evaluate function over grid lowerIntegrationLimit + [j-1]*currentGridSize, j=1,numberOfIntegrationIntervalls+1.
    for ( j in 1 : (numberOfIntegrationIntervalls[i]+1) )
    {
      grid <- lowerIntegrationLimit[i] + ( currentGridSize * (j-1) )
      f <- lastGrid * (stats::dnorm((grid-lastlast)/standardDeviation) / standardDeviation )
      lastCopy[j] <- integrateByTrapez ( f, numberOfIntegrationIntervalls[i-1], previousGridSize )
    }


    ##Return vector 'lastGrid' respectively 'lastCopy'
    return(lastCopy)

  }#end <--*else*

}#end <--*function(...)*


"searchForBound" <-
function (lastGrid, numberOfIntegrationIntervalls, i, gridSize, probDifference, standardDeviation, lowerIntegrationLimit, upperIntegrationLimit, numberOfInterimAnalysis)
{
  ###Initialize variables###
  noError<-TRUE

  ##Initialize tolerance for result - that implies our accuracy
  tolerance<-1.0e-07


    ##---------------------------------------------------------------------##
    ##---- first use the "Sekanten-Verfahren" based on Newton Iteration ---##
    ##---------------------------------------------------------------------##
    ##   ##
    ## calculation obeys following pattern whereby x    converges against  ##
    ## the value we are searching for               k+1   ##

    ##             x  _  x                                                 ##
    ##              k     k-1                                              ##
    ##  x    =  ------------------ * f(x )                                 ##
    ##   k+1     f(x ) - f(x   )        k      ##
    ##              k       k-1                ##


    ## Initialize values
    newtonFailed <- FALSE

    ##set x    and f(x   )
    ##     k-1        k-1
    ##--------------------
    xkMinusOne <- upperIntegrationLimit[i-1]
    fxkMinusOne <- abs(tailProbability(xkMinusOne, lastGrid, numberOfIntegrationIntervalls[i-1], lowerIntegrationLimit[i-1], upperIntegrationLimit[i-1], standardDeviation)-probDifference)

    ## choose start point for iteration
    ## the more interim analysis we have the closer we choose our startpoint to upperIntegrationLimit[i-1]
    ## which is done by (xkMinusOne-epsilon)

    ## this choice leads to an average number of about 4 iterations to converge -
    ## in case of equally spaced interim analysis it is mostly even better leading to about 3 iterations

    ## in spite of that i would be fine with better choices
    ## if the Reader knows how to choose them

    ## if we got very many interim analysis we have to limit our epsilon
    if(numberOfInterimAnalysis > 10)
    {
      epsilon <- 10^(-10)
    }
    else
    {
      epsilon <- 10^(-numberOfInterimAnalysis)
    }

    ##set x  and f(x )
    ##     k        k
    ##----------------
    xk <- xkMinusOne-epsilon
    fxk <- abs(tailProbability(xk, lastGrid, numberOfIntegrationIntervalls[i-1], lowerIntegrationLimit[i-1], upperIntegrationLimit[i-1], standardDeviation)-probDifference)

    numberOfLoops<-0

###########################################################################
########################### BEGIN SEARCHING ###############################
###########################################################################

    ## We do 20 iterations maximally -
    ## if we do not have finished then, we won't have convergence at all
    for (j in 1:20)
    {
      numberOfLoops <- j

      ## get new xkPlusOne like shown above
      xkPlusOne <- xk - ( (xk - xkMinusOne)/(fxk - fxkMinusOne) * fxk )

      ##catch xkPlusOne is not defined for any reason for example if (fxk-fxkMinusOne == 0)
      if(is.nan(xkPlusOne))
      {
        newtonFailed <- TRUE
        break
      }

      ##catch diverging xkPlusOne
      if(xkPlusOne==Inf || xkPlusOne==-Inf)
      {
        newtonFailed <- TRUE
        break
      }

      ##calculate new fxkPlusOne
      fxkPlusOne <- abs(tailProbability(xkPlusOne, lastGrid, numberOfIntegrationIntervalls[i-1], lowerIntegrationLimit[i-1], upperIntegrationLimit[i-1], standardDeviation)-probDifference)


      ## check if we reached tolerance
      if ( fxkPlusOne <= tolerance)
      {
        ##tolerance is fulfilled - return xkPlusOne
        upperIntegrationLimit[i]<-xkPlusOne
        noError<-TRUE
        break ##leave the loop
      }
      else
      {
        ## not within tolerance yet - set new values and do next iteration
        xkMinusOne<- xk
        fxkMinusOne <- fxk

        xk <- xkPlusOne
        fxk <- fxkPlusOne
      }
    }#end <--*for (j in 1:20)*

    ##If all 20 loops were made, something must be wrong -
    ##therefore we try old method from Fortran Implementation
    if (numberOfLoops==20)
    {
      newtonFailed <- TRUE
    }

###########################################################################
########### Old Implementation - usually it should NOT be used ############
###########################################################################


  ##"Sekanten-Verfahren" failed - so we try old conservative method
  if(newtonFailed)
  {
    searchingWithOldMethod <- TRUE

    ##Initialize variables and values
    numberOfLoops<-0

    ##Initialize estimates at previous integration limit.
    uppr <- upperIntegrationLimit[i-1]

    ##Initialize step size.
    del<-10.0

    q <- tailProbability(uppr, lastGrid, numberOfIntegrationIntervalls[i-1], lowerIntegrationLimit[i-1], upperIntegrationLimit[i-1], standardDeviation)

    while(searchingWithOldMethod)
    {
       ##--------------------------------------------------------##
       ##--------------------- q is alright ---------------------##
       ##--------------------------------------------------------##
       ##If q and probDifference are nearly equal, set upperIntegrationLimit[i] and return
       if ( (abs(q-probDifference)) <= tolerance)
       {
         upperIntegrationLimit[i]<-uppr
         noError<-TRUE
         searchingWithOldMethod<-FALSE
       }

       ##--------------------------------------------------------##
       ##------------------- q is too large ---------------------##
       ##--------------------------------------------------------##
       ##Else if q is too large, start increasing uppr by steps.
       else if (q>(probDifference+tolerance))
       {
         ##count for-loops for the purpose of controlling convergence
         numberOfLoops<-0

         ##Reduce step size by factor of 10.
         del <- del/10

         ##Increase uppr by del...
         for (j in 1:50)
         {
           numberOfLoops <- numberOfLoops + 1
           uppr <- uppr + del

           #...and check whether q is near probDifference.
           q <- tailProbability(uppr, lastGrid, numberOfIntegrationIntervalls[i-1], lowerIntegrationLimit[i-1], upperIntegrationLimit[i-1], standardDeviation)

           if (q<=(probDifference+tolerance))
           {
             break ##leave the for-loop and do another *while(stillSearching)*-loop
           }

           #If many iterations do not converge, print warning after each 10 loops.
           if ((j %% 10) == 0)
           {
             print(" Large change in bounds, possible error.",quote=FALSE)
             print(" Time/Point of interim analysis:",quote=FALSE)
             print(i)
           }
         }#end <--*for*
         ##If all 50 loops were made, something must be wrong - abort!
         if (numberOfLoops==50)
         {
           searchingWithOldMethod<-FALSE
           noError<-FALSE
         }

       }#end <--*else if (q>(probDifference+tolerance))*

       ##--------------------------------------------------------##
       ##------------------- q is too small ---------------------##
       ##--------------------------------------------------------##
       ##Else if q is too small, start decreasing uppr by steps.
       else if (q<(probDifference-tolerance))
            {
              ##count for-loops for the purpose of controlling convergence
              numberOfLoops<-0

              ##Reduce step size by factor of 10.
              del <- del/10

              ##Increase uppr by del...
              for (j in 1:80)
              {
                numberOfLoops <- numberOfLoops + 1
                uppr <- uppr - del

                #...and check whether q is near probDifference.
                q <- tailProbability(uppr, lastGrid, numberOfIntegrationIntervalls[i-1], lowerIntegrationLimit[i-1], upperIntegrationLimit[i-1], standardDeviation)

                if (q>=(probDifference-tolerance))
                {
                  break ##leave the for-loop and do another *while(searchingWithOldMethod)*-loop
                }

                #If many iterations do not converge, print warning after each 10 loops.
                if ((j %% 10) == 0)
                {
                  print(" Large change in bounds, possible error.",quote=FALSE)
                  print(" Time/Point of interim analysis:",quote=FALSE)
                  print(i)
                }
              }#end <--*for*
              ##If all 80 loops were made, something must be wrong - abort!
              if (numberOfLoops==80)
              {
                searchingWithOldMethod<-FALSE
                noError<-FALSE
              }

            }#end <--*else if (q>(probDifference+tolerance))*

    }#end <--*while(searchingWithOldMethod)*
  }#end <--*if(newtonFailed)*

     else
     {
       ## "Sekanten-Verfahren" was successful
     }


  ##if one of the routines above worked correctly -> return the calculated value...
  if (noError)
  {
    return(upperIntegrationLimit[i])
  }

  ##...else abort analysis
  else
  {
    print("Error in search: not converging. Abort analysis!",quote=FALSE)
    return(noError)
  }



}#end <--*function(...)



"tailProbability" <-
function(upperBound, previousDensity, numberOfIntegrationIntervalls,lowerIntegrationLimit,upperIntegrationLimit,standardDeviation)
{
  ###Initialize variables###
  tempValue<-0
  grid<-0
  previousGrid<-0

  ##previous grid size
  previousGrid <- (upperIntegrationLimit-lowerIntegrationLimit)/numberOfIntegrationIntervalls


  ##Compute function grid points##
  tempValue <- seq(lowerIntegrationLimit,upperIntegrationLimit,length=numberOfIntegrationIntervalls+1)
  tempValue <- previousDensity * ( 1 - stats::pnorm( (upperBound-tempValue)/standardDeviation ) )


  #Numerical integration
  result <- integrateByTrapez(tempValue, numberOfIntegrationIntervalls, previousGrid)

  return(result)
}#end <--*function(...)*

