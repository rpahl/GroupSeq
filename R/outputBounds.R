"outputBounds" <-
function(n,alpha,t,lowerBounds,upperBounds,probDifference,probExit,symmetricBoundsYesNo,spendingFunctionUsed)
{
    cat("", "\n")
    cat("", "\n")  
    cat("#################################", "\n")
    cat("#                               #", "\n")
    cat("# Output of the computed Bounds #", "\n")
    cat("#                               #", "\n")
    cat("#################################", "\n")
    cat("", "\n")
    cat("", "\n")        
    cat("number of interim analyses =",n,"\n")

    ##ouput alpha
    if(symmetricBoundsYesNo==1)
    {
      cat("alpha =",alpha[1],"\n")
    }
    else
    {
      cat("Upper alpha = ",alpha[1],"\n")
      cat("Lower alpha = ",alpha[2],"\n")
    }

    ##output names of spending functions that were used
    FunctionNames <- c("O'Brien-Fleming Type","Pocock Type","Power family: alpha* t^phi",
                       "Hwang-Shih-DeCani fammily","Exact Pocock Bounds")
    if(symmetricBoundsYesNo==1)
    {
      cat(FunctionNames[spendingFunctionUsed[1]],"was used as spending Function.","\n")
      cat("","\n")
    }
    else
    {       
      cat("Spending Function for UPPER Bound:",FunctionNames[spendingFunctionUsed[1]],"\n")
      cat("Spending Function for LOWER Bound:",FunctionNames[spendingFunctionUsed[2]],"\n")
      cat("","\n")
    }

    ##output the bounds
    times <- data.frame(" *Times*"=t,check.names=FALSE)
    bounds <- data.frame(" *Lower Bounds*"=lowerBounds," *Upper Bounds*"=upperBounds,check.names=FALSE)
    currentAlpha <- data.frame(" *alpha[i]-alpha[i-1]*"=probDifference, check.names=FALSE)
    cumulativeAlpha <- data.frame(" *cumulative alpha*"=probExit, check.names=FALSE)

    print(cbind(format(times,digits=3),format(bounds,digits=5), format(currentAlpha,digits=5), format(cumulativeAlpha,digits=5) ))

}#end <--*function(...)*
