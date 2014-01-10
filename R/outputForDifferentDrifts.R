"outputForDifferentDrifts" <-
function(n, probTotal, drift, expectedStoppingTime, secondTimeScaleIsUsed, t, t2, t2max, lowerBounds, upperBounds, probStopping, probExceedingUpper, probExceedingLower)
{
  ###INITIALiZE VARIABLES###
  cumulativeExitProb<-0 #cumulative exit probability
  resultExitProb<-0 #exit probability
  
    
  ##compute exit probability and cumulative exit probability
  for(i in 1:n)
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



  ##Start output##
  cat("\n")
  cat("n=",n,",  Drift=",drift,"\n")
  cat("Maximum Information:",t2max,"\n") 
  cat("-----------------------------------------------------------------------------------------------\n")

  ##if no second information time is used output without information time
  if(!secondTimeScaleIsUsed || t2max<=0)
  {
    times <- data.frame(" Times"=t,check.names=FALSE)
    bounds <- data.frame(" Lower Bounds"=lowerBounds," Upper Bounds"=upperBounds,check.names=FALSE)
    exitProbability <- data.frame(" exit Prob"=resultExitProb, check.names=FALSE)
    cumulativeExitProbability <- data.frame(" cumulative Exit Prob"=cumulativeExitProb, check.names=FALSE)

    print( cbind( format(times,digits=3), format(bounds,digits=5),
                       format(exitProbability,digits=5), format(cumulativeExitProbability,digits=5) ) )
  cat("-----------------------------------------------------------------------------------------------\n")
    cat("\n")  

  }

  else
  {
    ##Output with information time.
    times <- data.frame(" Times"=t,check.names=FALSE)
    info <- data.frame(" Info" =t2,check.names=FALSE)
    bounds <- data.frame(" Lower Bounds"=lowerBounds," Upper Bounds"=upperBounds,check.names=FALSE)
    exitProbability <- data.frame(" exit Prob"=resultExitProb, check.names=FALSE)
    cumulativeExitProbability <- data.frame(" cumulative Exit Prob"=cumulativeExitProb, check.names=FALSE)

    print( cbind( format(times,digits=3), format(info,digits=3), format(bounds,digits=5),
                       format(exitProbability,digits=5), format(cumulativeExitProbability,digits=5) ) )
  cat("-----------------------------------------------------------------------------------------------\n")  
    cat("\n")
  }
    
  #Return cumulativeExitProb[n] - needed for the power in the graph,if Task 3 was chosen
  return(cumulativeExitProb[n])

}#end <--*function(...)*
