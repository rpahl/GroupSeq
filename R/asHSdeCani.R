"asHSdeCani" <-
function( alpha, tk,OneOrTwoSidedBounds, gamma )
{
  if ( gamma == 0 )
  {
    return (alpha*tk)
  }  
  else
  {
    return ( (alpha/OneOrTwoSidedBounds) * (   (1-exp(-gamma*tk)) / (1-exp(-gamma))  )   )
  }
}
